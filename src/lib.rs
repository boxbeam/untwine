use std::ops::RangeInclusive;

use proc_macro::TokenStream;
use quote::quote;
use syn::{
    bracketed, custom_keyword,
    parse::{Parse, ParseStream},
    parse_macro_input, Ident, Lit, LitChar, LitStr, Result, Token, Type,
};

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

struct ParserDef {
    public: Option<Token![pub]>,
    name: Ident,
    colon: Token![:],
    arrow: Token![->],
    return_type: Type,
}

enum ParserPattern {
    Literal(StringLiteral),
    CharRange(CharRange),
}

impl Parse for ParserPattern {
    fn parse(input: ParseStream) -> Result<Self> {
        input
            .parse::<StringLiteral>()
            .map(ParserPattern::Literal)
            .or_else(|_| input.parse::<CharRange>().map(ParserPattern::CharRange))
    }
}

mod kw {
    use syn::custom_keyword;

    custom_keyword!(i);
}

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

impl Parse for ParserDef {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(ParserDef {
            public: input.parse()?,
            name: input.parse()?,
            colon: input.parse()?,
            arrow: input.parse()?,
            return_type: input.parse()?,
        })
    }
}

#[proc_macro]
pub fn parser(input: TokenStream) -> TokenStream {
    let header: Header = parse_macro_input!(input);
    // bracketed!(header_content in input);
    // input.into()
    quote! {}.into()
}
