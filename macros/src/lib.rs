use std::ops::RangeInclusive;

use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use syn::{
    braced, bracketed, parenthesized,
    parse::{discouraged::Speculative, Parse, ParseStream},
    parse_macro_input,
    token::{Brace, Bracket, Paren},
    Block, Expr, Ident, LitChar, LitStr, Result, Token, Type, Visibility,
};

mod codegen;

mod kw {
    use syn::custom_keyword;

    custom_keyword!(i);
    custom_keyword!(error);
    custom_keyword!(context);
    custom_keyword!(data);
}

#[derive(Debug)]
pub(crate) struct Header {
    ctx_name: Ident,
    error_type: Type,
    data_type: Type,
}

#[derive(Debug)]
pub(crate) enum HeaderParam {
    ContextName(Ident),
    ErrorType(Type),
    DataType(Type),
}

impl Parse for HeaderParam {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.parse::<kw::error>().is_ok() {
            input.parse::<Token![=]>()?;
            Ok(HeaderParam::ErrorType(input.parse()?))
        } else if input.parse::<kw::context>().is_ok() {
            input.parse::<Token![=]>()?;
            Ok(HeaderParam::ContextName(input.parse()?))
        } else if input.parse::<kw::data>().is_ok() {
            input.parse::<Token![=]>()?;
            Ok(HeaderParam::DataType(input.parse()?))
        } else {
            Err(input.error("expected parameter name 'error' or 'context'"))
        }
    }
}

impl Parse for Header {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut ctx_name = Ident::new("__ctx", Span::call_site());
        let mut error_type = syn::parse(quote! {untwine::ParserError}.into())?;
        let mut data_type = syn::parse(quote! {()}.into())?;
        if input.peek(Bracket) {
            let content;
            bracketed!(content in input);
            while !content.is_empty() {
                let param: HeaderParam = content.parse()?;
                match param {
                    HeaderParam::ContextName(name) => ctx_name = name,
                    HeaderParam::ErrorType(typ) => error_type = typ,
                    HeaderParam::DataType(typ) => data_type = typ,
                }
                if !content.is_empty() {
                    content.parse::<Token![,]>()?;
                }
            }
        }
        Ok(Header {
            ctx_name,
            error_type,
            data_type,
        })
    }
}

#[derive(Debug)]
pub(crate) struct ParserBlock {
    header: Header,
    parsers: Vec<ParserDef>,
}

impl Parse for ParserBlock {
    fn parse(input: ParseStream) -> Result<Self> {
        let block = ParserBlock {
            header: input.parse()?,
            parsers: list(input, false, |i| i.is_empty())?,
        };
        Ok(block)
    }
}

#[derive(Debug)]
pub(crate) struct ParserDef {
    vis: Visibility,
    name: Ident,
    patterns: TopLevelPatterns,
    return_type: Type,
    block: Block,
}

impl Parse for ParserDef {
    fn parse(input: ParseStream) -> Result<Self> {
        let vis = input.parse()?;
        let name: Ident = input.parse()?;
        let colon = input
            .parse::<Token![:]>()
            .map(|_| true)
            .or_else(|_| input.parse::<Token![=]>().map(|_| false))?;
        let mut patterns: TopLevelPatterns = input.parse()?;
        let return_type: Type = if input.parse::<Token![->]>().is_ok() {
            input.parse()?
        } else {
            syn::parse(quote! {()}.into())?
        };

        let block = if colon {
            input.parse()?
        } else {
            input.parse::<Token![;]>()?;
            if let Some(label) = patterns.patterns.iter().flat_map(|p| p.label.iter()).next() {
                return Err(syn::Error::new(
                    label.span(),
                    "Cannot use explicit labels here (implicitly captured by = at beginning)",
                ));
            }

            let pattern = Pattern {
                fragment: PatternFragment::Nested(PatternList::List(
                    patterns.patterns.into_iter().map(|p| p.pattern).collect(),
                )),
                modifier: None,
            };

            patterns = TopLevelPatterns {
                patterns: vec![LabeledPattern {
                    label: Some(name.clone()),
                    pattern,
                }],
            };

            syn::parse(quote! {{#name}}.into())?
        };
        Ok(ParserDef {
            vis,
            name,
            patterns,
            return_type,
            block,
        })
    }
}

#[derive(Debug, Clone)]
pub(crate) struct TopLevelPatterns {
    patterns: Vec<LabeledPattern>,
}

impl Parse for TopLevelPatterns {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut patterns = vec![];
        while !input.peek(Token![->]) && !input.peek(Token![;]) {
            patterns.push(input.parse()?);
        }
        Ok(TopLevelPatterns { patterns })
    }
}

#[derive(Debug, Clone)]
pub(crate) enum PatternFragment {
    Literal(LitStr),
    CharRange(CharRange),
    CharGroup(CharGroup),
    CharFilter(CharFilter),
    ParserRef(Ident),
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
        } else if input.peek(Token![#]) {
            let ignored: IgnoredPattern = input.parse()?;
            Ok(PatternFragment::Ignore(Box::new(ignored)))
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

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub(crate) struct IgnoredPattern {
    pattern: Pattern,
}

impl Parse for IgnoredPattern {
    fn parse(input: ParseStream) -> Result<Self> {
        input.parse::<Token![#]>()?;
        let pattern = input.parse()?;
        Ok(IgnoredPattern { pattern })
    }
}

#[derive(Debug, Clone)]
pub(crate) struct LabeledPattern {
    label: Option<Ident>,
    pattern: Pattern,
}

impl Parse for LabeledPattern {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut label = None;
        if input.peek2(Token![=]) {
            label = Some(input.parse()?);
            input.parse::<Token![=]>()?;
        }
        let pattern: Pattern = input.parse()?;
        Ok(LabeledPattern { label, pattern })
    }
}

#[derive(Debug, Clone)]
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
    Delimited(PatternFragment),
    OptionalDelimited(PatternFragment),
}

impl Modifier {
    fn is_repeating(&self) -> bool {
        match self {
            Modifier::Optional => false,
            Modifier::Repeating => true,
            Modifier::OptionalRepeating => true,
            Modifier::Delimited(_) => true,
            Modifier::OptionalDelimited(_) => true,
        }
    }
}

impl Parse for Modifier {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.parse::<Option<Token![+]>>()?.is_some() {
            Ok(Modifier::Repeating)
        } else if input.parse::<Option<Token![?]>>()?.is_some() {
            Ok(Modifier::Optional)
        } else if input.parse::<Option<Token![*]>>()?.is_some() {
            Ok(Modifier::OptionalRepeating)
        } else if input.parse::<Option<Token![$]>>()?.is_some() {
            let fragment = input.parse()?;
            if input.peek(Token![+]) {
                input.parse::<Token![+]>()?;
                Ok(Modifier::Delimited(fragment))
            } else {
                input.parse::<Token![*]>()?;
                Ok(Modifier::OptionalDelimited(fragment))
            }
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

#[proc_macro]
pub fn parser(input: TokenStream) -> TokenStream {
    let block: ParserBlock = parse_macro_input!(input as ParserBlock);

    match codegen::generate_parser_block(block) {
        Ok(stream) => stream.into(),
        Err(err) => err.to_compile_error().into(),
    }
}
