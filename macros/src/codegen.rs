use std::{
    collections::{HashMap, HashSet, VecDeque},
    ops::Deref,
    rc::Rc,
};

use proc_macro2::{Ident, Span, TokenStream};
use quote::{quote, TokenStreamExt};
use syn::{
    punctuated::Punctuated,
    token::{Gt, Lt},
    AngleBracketedGenericArguments, Path, PathArguments, PathSegment, Type, TypePath, TypeTuple,
};

use crate::{Modifier, Pattern, PatternFragment, PatternList};

pub fn option_of(typ: &Type) -> Type {
    let tokens = quote! {
        Option<#typ>
    };
    syn::parse(tokens.into()).unwrap()
}

pub fn vec_of(typ: &Type) -> Type {
    let tokens = quote! {
        Vec<#typ>
    };
    syn::parse(tokens.into()).unwrap()
}

fn is_unit(typ: &Type) -> bool {
    matches!(typ, Type::Tuple(tup) if tup.elems.len() == 0)
}

struct CodegenState {
    parser_context_name: Ident,
    parser_name: String,
    label_types: Vec<(Ident, Type)>,
    current_capture: Option<Ident>,
    parser_types: Rc<HashMap<String, Type>>,
}

fn parse_fragment(
    fragment: &PatternFragment,
    state: &CodegenState,
    capture: bool,
) -> syn::Result<TokenStream> {
    let ctx = &state.parser_context_name;
    let parser_name = &state.parser_name;
    let stream = match fragment {
        PatternFragment::Literal(lit) => {
            quote! {
                untwine::literal(#lit)
            }
        }
        PatternFragment::CharRange(range) => {
            let inverted = range.inverted.then(|| quote! {!});
            let range_min = range.range.start();
            let range_max = range.range.end();
            quote! {
                untwine::char_filter(|c| #inverted (#range_min ..= #range_max).contains(c), #parser_name)
            }
        }
        PatternFragment::CharGroup(group) => {
            let inverted = if group.inverted {
                quote! {!}
            } else {
                quote! {}
            };
            let chars: String = group.chars.iter().collect();
            quote! {
                untwine::char_filter(|c| #inverted #chars.contains(c), #parser_name)
            }
        }
        PatternFragment::CharFilter(filter) => {
            let filter = &filter.expr;
            quote! {
                untwine::char_filter(#filter, #parser_name)?
            }
        }
        PatternFragment::ParserRef(parser) => {
            quote! {
                untwine::parser(|ctx| #parser(ctx))
            }
        }
        PatternFragment::Ignore(pattern) => {
            let parse_inner = parse_pattern(&pattern.pattern, state, false)?;
            quote! { #parse_inner.ignore() }
        }
        PatternFragment::Span(span) => {
            let parse_inner = parse_pattern_list(span, state, false)?;
            quote! {
                #parse_inner.span()
            }
        }
        PatternFragment::Nested(list) => parse_pattern_list(list, state, capture)?,
        PatternFragment::AnyChar => {
            quote! {#ctx.char_filter(|_| true, "more content")}
        }
    };
    Ok(stream)
}

fn parse_pattern(
    pattern: &Pattern,
    state: &CodegenState,
    capture: bool,
) -> syn::Result<TokenStream> {
    let fragment_parser = parse_fragment(&pattern.fragment, state, capture)?;

    Ok(match &pattern.modifier {
        Some(Modifier::Optional) => quote! {#fragment_parser.optional()},
        Some(Modifier::Repeating) => quote! {#fragment_parser.repeating()},
        Some(Modifier::OptionalRepeating) => quote! {#fragment_parser.optional_repeating()},
        Some(Modifier::Delimited(delimiter)) => {
            let delimiter_parser = parse_fragment(delimiter, state, false)?;
            quote! {#fragment_parser.delimited(#delimiter_parser)}
        }
        Some(Modifier::OptionalDelimited(delimiter)) => {
            let delimiter_parser = parse_fragment(delimiter, state, false)?;
            quote! {#fragment_parser.optional_delimited(#delimiter_parser)}
        }
        None => fragment_parser,
    })
}

fn parse_pattern_list(
    patterns: &PatternList,
    state: &CodegenState,
    capture: bool,
) -> syn::Result<TokenStream> {
    match patterns {
        PatternList::List(list) => parse_patterns(list, state, capture),
        PatternList::Choices(choices) => parse_pattern_choices(choices, state, capture),
    }
}

fn parse_pattern_choices(
    patterns: &Vec<Vec<Pattern>>,
    state: &CodegenState,
    capture: bool,
) -> syn::Result<TokenStream> {
    let first = parse_patterns(&patterns[0], state, capture)?;
    let mut rest = vec![];
    for parser in patterns[1..].iter() {
        rest.push(parse_patterns(parser, state, capture)?);
    }

    Ok(quote! {
        #first #( .or(#rest) )*
    })
}

fn parse_patterns(
    patterns: &Vec<Pattern>,
    state: &CodegenState,
    capture: bool,
) -> syn::Result<TokenStream> {
    let mut parsers = vec![];
    let mut captured = vec![];
    for (i, pattern) in patterns.iter().enumerate() {
        let parser = parse_pattern(pattern, state, capture)?;
        let ident = numbered_ident(i);
        let ctx = &state.parser_context_name;

        let parser = quote! {
            let #ident = #parser.parse(#ctx)?;
        };
        parsers.push(parser);

        let parser_type = pattern_type(pattern, &state.parser_types)?;
        if capture && !is_unit(&parser_type) {
            captured.push(ident);
        }
    }
    Ok(quote! {
        untwine::parser(|ctx| {
            #(
                #parsers
            )*
            Ok(( #(#captured),* ))
        })
    })
}

fn numbered_ident(num: usize) -> Ident {
    Ident::new(&format!("_{num}"), Span::call_site())
}

fn fragment_type(
    fragment: &PatternFragment,
    parser_types: &HashMap<String, Type>,
) -> syn::Result<Type> {
    use PatternFragment as P;
    let tokens = match fragment {
        P::Span(_) => quote! {&str},
        P::CharRange(_) | P::CharGroup(_) | P::AnyChar | P::CharFilter(_) => quote! {char},
        P::Ignore(_) | P::Literal(_) => quote! {()},
        P::ParserRef(ident) => return Ok(parser_types[&ident.to_string()].clone()),
        P::Nested(PatternList::List(l)) => return Ok(list_type(l, parser_types)?),
        P::Nested(PatternList::Choices(c)) => return Ok(list_type(&c[0], parser_types)?),
    };
    Ok(syn::parse(tokens.into()).unwrap())
}

fn list_type(patterns: &Vec<Pattern>, parser_types: &HashMap<String, Type>) -> syn::Result<Type> {
    let mut tuple = vec![];
    for pattern in patterns {
        let typ = pattern_type(pattern, parser_types)?;
        if !is_unit(&typ) {
            tuple.push(typ);
        }
    }

    let tokens = quote! {
        (
            #(#tuple),*
        )
    };
    Ok(syn::parse(tokens.into()).unwrap())
}

pub fn pattern_type(pattern: &Pattern, parser_types: &HashMap<String, Type>) -> syn::Result<Type> {
    let typ = fragment_type(&pattern.fragment, parser_types)?;
    let typ = match pattern.modifier {
        Some(Modifier::Optional) => option_of(&typ),
        Some(
            Modifier::Repeating
            | Modifier::OptionalRepeating
            | Modifier::Delimited(_)
            | Modifier::OptionalDelimited(_),
        ) => vec_of(&typ),
        None => typ,
    };
    Ok(typ)
}

fn extend_front<T>(vec: &mut VecDeque<T>, elems: impl IntoIterator<Item = T>) {
    for elem in elems {
        vec.push_front(elem);
    }
}
