use any_stack::AnySplit;

pub extern crate macros;

pub mod any_stack;

pub type Result<T> = std::result::Result<T, ParserError>;

pub enum ParserError {
    ExpectedLiteral(&'static str),
    ExpectedToken(&'static str),
}

pub struct ParserContext<'a> {
    original: &'a str,
    cur: &'a str,
    pub split: AnySplit<'a>,
}

impl<'a> ParserContext<'a> {
    pub fn literal(&mut self, literal: &'static str, case_sensitive: bool) -> Result<()> {
        if self.cur.len() < literal.len() {
            return Err(ParserError::ExpectedLiteral(literal));
        }
        let found = if case_sensitive {
            self.cur.starts_with(literal)
        } else {
            let a = self.cur.chars().map(|c| c.to_lowercase()).flatten();
            let b = literal.chars().map(|c| c.to_lowercase()).flatten();
            a.zip(b).all(|(a, b)| a == b)
        };
        if found {
            self.cur = &self.cur[literal.len()..];
            Ok(())
        } else {
            Err(ParserError::ExpectedLiteral(literal))
        }
    }

    pub fn char_filter(
        &mut self,
        filter: impl Fn(&char) -> bool,
        token_name: &'static str,
    ) -> Result<char> {
        let c = self.cur.chars().next();
        let res = c
            .filter(filter)
            .ok_or(ParserError::ExpectedToken(token_name));
        if let Ok(c) = res {
            self.cur = &self.cur[..c.len_utf8()];
        }
        res
    }

    pub fn with_split(mut self, split: AnySplit<'a>) -> Self {
        self.split = split;
        self
    }

    pub fn original(&self) -> &str {
        self.original
    }
}
