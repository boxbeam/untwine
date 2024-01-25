#![allow(unused)]
use std::ops::RangeInclusive;

use proc_macro::TokenStream;
use quote::quote;
use syn::{
    bracketed, custom_keyword, parenthesized,
    parse::{discouraged::Speculative, Parse, ParseStream},
    parse_macro_input, Block, Ident, Lit, LitChar, LitStr, Result, Token, Type,
};

#[derive(Debug)]
struct Header {
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
struct ParserBlock {
    header: Option<Header>,
    parsers: Vec<ParserDef>,
}

impl Parse for ParserBlock {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(ParserBlock {
            header: input.parse().ok(),
            parsers: list(input, false)?,
        })
    }
}

#[derive(Debug)]
struct ParserDef {
    public: Option<Token![pub]>,
    name: Ident,
    colon: Token![:],
    patterns: Vec<Pattern>,
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
            patterns: list(input, true)?,
            arrow: input.parse()?,
            return_type: input.parse()?,
            block: input.parse()?,
        })
    }
}

#[derive(Debug)]
enum PatternFragment {
    Literal(StringLiteral),
    CharRange(CharRange),
    ParserRef(Ident),
    Labeled(Box<LabeledPattern>),
    Nested(NestedPatternList),
    Choice(PatternChoiceList),
}

#[derive(Debug)]
struct Pattern {
    fragment: PatternFragment,
    modifier: Option<Modifier>,
}

impl Parse for Pattern {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(Pattern {
            fragment: input.parse()?,
            modifier: input.parse().ok(),
        })
    }
}

impl Parse for PatternFragment {
    fn parse(input: ParseStream) -> Result<Self> {
        input
            .parse()
            .map(PatternFragment::Literal)
            .or_else(|_| input.parse().map(PatternFragment::CharRange))
            .or_else(|_| input.parse().map(|v| PatternFragment::Labeled(Box::new(v))))
            .or_else(|_| input.parse().map(PatternFragment::Choice))
            .or_else(|_| input.parse().map(PatternFragment::Nested))
            .or_else(|_| input.parse().map(PatternFragment::ParserRef))
    }
}

mod kw {
    use syn::custom_keyword;

    custom_keyword!(i);
}

#[derive(Debug)]
struct StringLiteral {
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
struct CharRange {
    inverted: bool,
    range: RangeInclusive<char>,
}

impl Parse for CharRange {
    fn parse(input: ParseStream) -> Result<Self> {
        let content;
        bracketed!(content in input);
        let inverted: Option<Token![^]> = content.parse()?;
        let begin: LitChar = content.parse()?;
        content.parse::<Token![-]>()?;
        let end: LitChar = content.parse()?;
        Ok(CharRange {
            inverted: inverted.is_some(),
            range: begin.value()..=end.value(),
        })
    }
}

#[derive(Debug)]
struct LabeledPattern {
    label: Ident,
    pattern: PatternFragment,
}

impl Parse for LabeledPattern {
    fn parse(input: ParseStream) -> Result<Self> {
        scoped(input, |input| {
            Ok(LabeledPattern {
                label: input.parse()?,
                pattern: input.parse()?,
            })
        })
    }
}

#[derive(Debug)]
struct NestedPatternList {
    patterns: Vec<Pattern>,
}

impl Parse for NestedPatternList {
    fn parse(input: ParseStream) -> Result<Self> {
        scoped(input, |input| {
            let content;
            parenthesized!(content in input);
            Ok(NestedPatternList {
                patterns: list(&content, true)?,
            })
        })
    }
}

#[derive(Debug)]
struct PatternChoiceList {
    patterns: Vec<Pattern>,
}

impl Parse for PatternChoiceList {
    fn parse(input: ParseStream) -> Result<Self> {
        scoped(input, |input| {
            let content;
            parenthesized!(content in input);
            Ok(PatternChoiceList {
                patterns: delimited_list::<_, Token![|]>(&content, true)?,
            })
        })
    }
}

#[derive(Debug)]
enum Modifier {
    Optional,
    Repeating,
    OptionalRepeating,
}

impl Parse for Modifier {
    fn parse(input: ParseStream) -> Result<Self> {
        input
            .parse::<Token![+]>()
            .map(|_| Modifier::Repeating)
            .or_else(|_| input.parse::<Token![?]>().map(|_| Modifier::Optional))
            .or_else(|_| {
                input
                    .parse::<Token![*]>()
                    .map(|_| Modifier::OptionalRepeating)
            })
    }
}

fn list<T: Parse>(input: ParseStream, require: bool) -> Result<Vec<T>> {
    scoped(input, |input| {
        let mut vec = vec![];
        if require {
            vec.push(input.parse()?);
        }
        while let Ok(elem) = input.parse() {
            vec.push(elem);
        }
        Ok(vec)
    })
}

fn delimited_list<T: Parse, D: Parse>(input: ParseStream, require: bool) -> Result<Vec<T>> {
    scoped(input, |input| {
        let mut vec = vec![];
        match (input.parse(), require) {
            (Ok(v), _) => {
                vec.push(v);
            }
            (Err(e), true) => return Err(e),
            (Err(_), false) => return Ok(vec),
        }
        while let Ok(_) = input.parse::<D>() {
            vec.push(input.parse()?);
        }
        Ok(vec)
    })
}

/// An operation which parses multiple values but may fail partway through can leave
/// the parser head at an invalid location. Use this to make the operation fail or
/// succeed completely.
fn scoped<T>(input: ParseStream, parser: impl FnOnce(ParseStream) -> Result<T>) -> Result<T> {
    let newinput = input.fork();
    let val = parser(&newinput)?;
    input.advance_to(&newinput);
    Ok(val)
}

#[proc_macro]
pub fn parser(input: TokenStream) -> TokenStream {
    let header: ParserBlock = parse_macro_input!(input);
    quote! {}.into()
}
