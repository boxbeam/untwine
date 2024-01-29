#![allow(unused)]
use std::ops::RangeInclusive;

use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use syn::{
    braced, bracketed, custom_keyword, parenthesized,
    parse::{discouraged::Speculative, Parse, ParseStream},
    parse_macro_input,
    token::{Brace, Bracket, Paren},
    visit::Visit,
    Block, Expr, Ident, Lit, LitChar, LitStr, MacroDelimiter, Result, Token, Type,
};

mod type_utils;

#[derive(Debug)]
pub(crate) struct Header {
    ctx_name: Ident,
    colon: Token![:],
    ctx_type: Type,
}

impl Parse for Header {
    fn parse(input: ParseStream) -> Result<Self> {
        let content;
        bracketed!(content in input);
        Ok(Header {
            ctx_name: content.parse()?,
            colon: content.parse()?,
            ctx_type: content.parse()?,
        })
    }
}

#[derive(Debug)]
pub(crate) struct ParserBlock {
    header: Option<Header>,
    parsers: Vec<ParserDef>,
}

impl Parse for ParserBlock {
    fn parse(input: ParseStream) -> Result<Self> {
        let block = ParserBlock {
            header: optional(input),
            parsers: list(input, false, |i| i.is_empty())?,
        };
        Ok(block)
    }
}

#[derive(Debug)]
pub(crate) struct ParserDef {
    public: Option<Token![pub]>,
    name: Ident,
    colon: Token![:],
    patterns: PatternList,
    arrow: Token![->],
    return_type: Type,
    block: Block,
}

impl Parse for ParserDef {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(ParserDef {
            public: input.parse()?,
            name: input.parse()?,
            colon: input.parse()?,
            patterns: input.parse()?,
            arrow: input.parse()?,
            return_type: input.parse()?,
            block: input.parse()?,
        })
    }
}

#[derive(Debug)]
pub(crate) enum PatternFragment {
    Literal(StringLiteral),
    CharRange(CharRange),
    CharGroup(CharGroup),
    CharFilter(CharFilter),
    ParserRef(Ident),
    Labeled(Box<LabeledPattern>),
    Ignore(Box<IgnoredPattern>),
    Span(PatternList),
    Nested(PatternList),
    AnyChar,
}

impl Parse for PatternFragment {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek(LitChar) || input.peek(Token![^]) {
            input.parse().map(PatternFragment::CharRange)
        } else if input.peek(Bracket) {
            input.parse().map(PatternFragment::CharGroup)
        } else if input.peek(Brace) {
            input.parse().map(PatternFragment::CharFilter)
        } else if input.peek(LitStr) || input.peek(kw::i) {
            input.parse().map(PatternFragment::Literal)
        } else if input.peek(Paren) {
            let content;
            parenthesized!(content in input);
            content.parse().map(PatternFragment::Nested)
        } else if input.peek(Token![.]) {
            input.parse::<Token![.]>()?;
            Ok(PatternFragment::AnyChar)
        } else if input.peek(Ident) && input.peek2(Token![=]) {
            input.parse().map(|v| PatternFragment::Labeled(Box::new(v)))
        } else if input.peek(Token![_]) {
            input.parse().map(|v| PatternFragment::Ignore(Box::new(v)))
        } else if input.peek(Token![<]) {
            input.parse::<Token![<]>()?;
            let patterns = input.parse()?;
            input.parse::<Token![>]>()?;
            Ok(PatternFragment::Span(patterns))
        } else if input.peek(Ident) {
            input.parse().map(PatternFragment::ParserRef)
        } else {
            Err(input.error("expected pattern fragment"))
        }
    }
}

impl PatternFragment {
    pub fn walk(&self, f: &mut impl FnMut(&PatternFragment)) {
        f(self);
        match self {
            PatternFragment::Literal(_) => {}
            PatternFragment::CharRange(_) => {}
            PatternFragment::CharGroup(_) => {}
            PatternFragment::CharFilter(_) => {}
            PatternFragment::ParserRef(_) => {}
            PatternFragment::AnyChar => {}
            PatternFragment::Span(_) => {}
            PatternFragment::Labeled(inner) => {
                inner.pattern.fragment.walk(f);
            }
            PatternFragment::Ignore(inner) => {
                inner.pattern.fragment.walk(f);
            }
            PatternFragment::Nested(PatternList::List(list)) => {
                for pat in list {
                    pat.fragment.walk(f);
                }
            }
            PatternFragment::Nested(PatternList::Choices(choices)) => {
                for pat in choices.iter().flatten() {
                    pat.fragment.walk(f);
                }
            }
        }
    }

    pub fn children(&self) -> Vec<&Pattern> {
        use PatternFragment as PF;
        use PatternList as PL;
        match self {
            PF::Literal(_) => vec![],
            PF::CharRange(_) => vec![],
            PF::CharGroup(_) => vec![],
            PF::CharFilter(_) => vec![],
            PF::ParserRef(_) => vec![],
            PF::Labeled(p) => vec![&p.as_ref().pattern],
            PF::Ignore(p) => vec![&p.as_ref().pattern],
            PF::Nested(PL::List(l)) | PF::Span(PL::List(l)) => l.iter().collect(),
            PF::Nested(PL::Choices(l)) | PF::Span(PL::Choices(l)) => l.iter().flatten().collect(),
            PF::AnyChar => vec![],
        }
    }
}

#[derive(Debug)]
pub(crate) struct Pattern {
    fragment: PatternFragment,
    modifier: Option<Modifier>,
}

impl Parse for Pattern {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(Pattern {
            fragment: input.parse()?,
            modifier: optional(input),
        })
    }
}

mod kw {
    use syn::custom_keyword;

    custom_keyword!(i);
}

#[derive(Debug)]
pub(crate) struct StringLiteral {
    case_sensitive: bool,
    string: LitStr,
}

impl Parse for StringLiteral {
    fn parse(input: ParseStream) -> Result<Self> {
        let case_insensitive: Option<kw::i> = input.parse()?;
        let string: LitStr = input.parse()?;
        Ok(StringLiteral {
            case_sensitive: case_insensitive.is_none(),
            string,
        })
    }
}

#[derive(Debug)]
pub(crate) struct CharRange {
    inverted: bool,
    range: RangeInclusive<char>,
}

impl Parse for CharRange {
    fn parse(input: ParseStream) -> Result<Self> {
        let inverted: Option<Token![^]> = input.parse()?;
        let begin: LitChar = input.parse()?;
        input.parse::<Token![-]>()?;
        let end: LitChar = input.parse()?;
        Ok(CharRange {
            inverted: inverted.is_some(),
            range: begin.value()..=end.value(),
        })
    }
}

#[derive(Debug)]
pub(crate) struct CharGroup {
    inverted: bool,
    chars: Vec<char>,
}

impl Parse for CharGroup {
    fn parse(input: ParseStream) -> Result<Self> {
        let content;
        bracketed!(content in input);
        let inverted: Option<Token![^]> = content.parse()?;
        let str: LitStr = content.parse()?;
        Ok(CharGroup {
            inverted: inverted.is_some(),
            chars: str.value().chars().collect(),
        })
    }
}

#[derive(Debug)]
pub(crate) struct CharFilter {
    expr: Expr,
}

impl Parse for CharFilter {
    fn parse(input: ParseStream) -> Result<Self> {
        let content;
        braced!(content in input);
        Ok(CharFilter {
            expr: content.parse()?,
        })
    }
}

#[derive(Debug)]
pub(crate) struct IgnoredPattern {
    pattern: Pattern,
}

impl Parse for IgnoredPattern {
    fn parse(input: ParseStream) -> Result<Self> {
        input.parse::<Token![_]>();
        input.parse::<Token![=]>();
        let fragment: PatternFragment = input.parse()?;
        Ok(IgnoredPattern {
            pattern: Pattern {
                fragment,
                modifier: None,
            },
        })
    }
}

#[derive(Debug)]
pub(crate) struct LabeledPattern {
    label: Ident,
    pattern: Pattern,
}

impl Parse for LabeledPattern {
    fn parse(input: ParseStream) -> Result<Self> {
        let label: Ident = input.parse()?;
        input.parse::<Token![=]>()?;
        let fragment: PatternFragment = input.parse()?;
        let mut inner_labeled = false;
        fragment.walk(&mut |v| {
            if matches!(v, PatternFragment::Labeled(_)) {
                inner_labeled = true;
            }
        });
        if inner_labeled {
            Err(syn::Error::new(
                label.span(),
                "Cannot label a pattern which contains further labels",
            ))
        } else {
            Ok(LabeledPattern {
                label,
                pattern: Pattern {
                    fragment,
                    modifier: None,
                },
            })
        }
    }
}

#[derive(Debug)]
pub(crate) enum PatternList {
    List(Vec<Pattern>),
    Choices(Vec<Vec<Pattern>>),
}

fn list_terminator(input: ParseStream) -> bool {
    input.peek(Token![|]) || input.peek(Token![->]) || input.peek(Token![>])
}

impl Parse for PatternList {
    fn parse(input: ParseStream) -> Result<Self> {
        let patterns: Vec<Pattern> = list(input, true, list_terminator)?;
        if !input.peek(Token![|]) {
            return Ok(PatternList::List(patterns));
        }
        let mut choices: Vec<Vec<Pattern>> = vec![patterns];
        while input.peek(Token![|]) {
            input.parse::<Token![|]>()?;
            choices.push(list(input, true, list_terminator)?);
        }
        Ok(PatternList::Choices(choices))
    }
}

#[derive(Debug, Clone)]
pub(crate) enum Modifier {
    Optional,
    Repeating,
    OptionalRepeating,
}

impl Parse for Modifier {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.parse::<Option<Token![+]>>()?.is_some() {
            Ok(Modifier::Repeating)
        } else if input.parse::<Option<Token![?]>>()?.is_some() {
            Ok(Modifier::Optional)
        } else if input.parse::<Option<Token![*]>>()?.is_some() {
            Ok(Modifier::OptionalRepeating)
        } else {
            Err(input.error("expected modifier"))
        }
    }
}

fn list<T: Parse>(
    input: ParseStream,
    require: bool,
    terminator: fn(ParseStream) -> bool,
) -> Result<Vec<T>> {
    let mut vec = vec![];
    while (require && vec.is_empty()) || (!input.is_empty() && !terminator(input)) {
        vec.push(input.parse()?);
    }
    Ok(vec)
}

fn optional<T: Parse>(input: ParseStream) -> Option<T> {
    let fork = input.fork();
    match fork.parse() {
        Ok(val) => {
            input.advance_to(&fork);
            Some(val)
        }
        Err(_) => None,
    }
}

#[derive(Default)]
struct IdentVisitor(Vec<Ident>);

impl<'ast> Visit<'ast> for IdentVisitor {
    fn visit_ident(&mut self, i: &'ast Ident) {
        self.0.push(i.clone());
    }
}

#[proc_macro]
pub fn parser(input: TokenStream) -> TokenStream {
    let header: ParserBlock = parse_macro_input!(input as ParserBlock);
    for parser in &header.parsers {
        let mut visitor = IdentVisitor::default();
        visitor.visit_block(&parser.block);
        let IdentVisitor(idents) = visitor;
    }
    println!("{header:#?}");
    quote! {}.into()
}
