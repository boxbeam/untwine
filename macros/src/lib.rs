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
            parsers: list(input, false)?,
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
    Ignore(Box<PatternFragment>),
    Span(Vec<Pattern>),
    Nested(PatternList),
    AnyChar,
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
            let patterns = list(input, true)?;
            input.parse::<Token![>]>()?;
            Ok(PatternFragment::Span(patterns))
        } else if input.peek(Ident) {
            input.parse().map(PatternFragment::ParserRef)
        } else {
            Err(input.error("expected pattern fragment"))
        }
    }
}

mod kw {
    use syn::custom_keyword;

    custom_keyword!(i);
}

#[derive(Debug)]
pub(crate) struct StringLiteral {
    case_sensitive: bool,
    string: String,
}

impl Parse for StringLiteral {
    fn parse(input: ParseStream) -> Result<Self> {
        let case_insensitive: Option<kw::i> = input.parse()?;
        let s: LitStr = input.parse()?;
        Ok(StringLiteral {
            case_sensitive: case_insensitive.is_none(),
            string: s.value(),
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
pub(crate) struct LabeledPattern {
    label: Ident,
    pattern: PatternFragment,
}

impl Parse for LabeledPattern {
    fn parse(input: ParseStream) -> Result<Self> {
        let label = input.parse()?;
        input.parse::<Token![=]>()?;
        let pattern = input.parse()?;
        Ok(LabeledPattern { label, pattern })
    }
}

#[derive(Debug)]
pub(crate) enum PatternList {
    List(Vec<Pattern>),
    Choices(Vec<Vec<Pattern>>),
}

impl Parse for PatternList {
    fn parse(input: ParseStream) -> Result<Self> {
        let patterns: Vec<Pattern> = list(input, true)?;
        if !input.peek(Token![|]) {
            return Ok(PatternList::List(patterns));
        }
        let mut choices: Vec<Vec<Pattern>> = vec![patterns];
        while input.peek(Token![|]) {
            input.parse::<Token![|]>()?;
            choices.push(list(input, true)?);
        }
        Ok(PatternList::Choices(choices))
    }
}

#[derive(Debug)]
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

fn list<T: Parse>(input: ParseStream, require: bool) -> Result<Vec<T>> {
    let mut vec = vec![];
    if require {
        vec.push(input.parse()?);
    }
    loop {
        let fork = input.fork();
        let Ok(val) = fork.parse() else {
            break;
        };
        vec.push(val);
        input.advance_to(&fork);
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
