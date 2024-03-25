use std::{
    collections::HashMap,
    hash::{DefaultHasher, Hash, Hasher},
};

use proc_macro2::{Ident, Span, TokenStream};
use quote::quote;
use syn::{Result, Type, Visibility};

use crate::{Modifier, ParserBlock, ParserDef, Pattern, PatternFragment, PatternList};

pub fn option_of(typ: &Type) -> Type {
    if is_unit(typ) {
        return typ.clone();
    }
    let tokens = quote! {
        Option<#typ>
    };
    syn::parse(tokens.into()).unwrap()
}

pub fn vec_of(typ: &Type) -> Type {
    if is_unit(typ) {
        return typ.clone();
    }
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
    error_type: Type,
    data_type: Type,
    parser_name: String,
    parser_types: HashMap<String, Type>,
}

fn parse_fragment(
    fragment: &PatternFragment,
    state: &CodegenState,
    capture: bool,
) -> Result<TokenStream> {
    let parser_name = &state.parser_name;
    let data = &state.data_type;
    let err = &state.error_type;
    let stream = match fragment {
        PatternFragment::Literal(lit) => {
            quote! {
                untwine::literal::<#data, #err>(#lit)
            }
        }
        PatternFragment::CharRange(range) => {
            let inverted = range.inverted.then(|| quote! {!});
            let range_min = range.range.start();
            let range_max = range.range.end();
            quote! {
                untwine::char_filter::<#data, #err>(|c| #inverted (#range_min ..= #range_max).contains(c), #parser_name)
            }
        }
        PatternFragment::CharGroup(group) => {
            let inverted = if group.inverted {
                quote! {!}
            } else {
                quote! {}
            };
            let chars: Vec<char> = group.chars.iter().cloned().collect();
            quote! {
                untwine::char_filter::<#data, #err>(|c| #inverted matches!(c, #(#chars)|*), #parser_name)
            }
        }
        PatternFragment::CharFilter(filter) => {
            let filter = &filter.expr;
            quote! {
                untwine::char_filter::<#data, #err>(#filter, #parser_name)
            }
        }
        PatternFragment::ParserRef(parser) => {
            quote! {
                untwine::parser::<#data, _, #err>(|ctx| #parser(ctx))
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
            quote! {untwine::char_filter::<#data, #err>(|_| true, "any character")}
        }
    };
    Ok(stream)
}

fn parse_pattern(pattern: &Pattern, state: &CodegenState, capture: bool) -> Result<TokenStream> {
    let mut fragment_parser = parse_fragment(&pattern.fragment, state, capture)?;

    if !capture
        && pattern
            .modifier
            .as_ref()
            .is_some_and(|modifier| modifier.is_repeating())
    {
        fragment_parser = quote! {#fragment_parser.ignore()};
    }

    let mut pattern_parser = match &pattern.modifier {
        Some(Modifier::Optional) => {
            quote! {#fragment_parser.optional()}
        }
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
    };

    if is_unit(&fragment_type(&pattern.fragment, &state.parser_types)?) {
        pattern_parser = quote! {#pattern_parser.ignore()}
    }
    Ok(pattern_parser)
}

fn parse_pattern_list(
    patterns: &PatternList,
    state: &CodegenState,
    capture: bool,
) -> Result<TokenStream> {
    match patterns {
        PatternList::List(list) => parse_patterns(list, state, capture),
        PatternList::Choices(choices) => parse_pattern_choices(choices, state, capture),
    }
}

fn parse_pattern_choices(
    patterns: &Vec<Vec<Pattern>>,
    state: &CodegenState,
    capture: bool,
) -> Result<TokenStream> {
    let name = &state.parser_name;
    let first = parse_patterns(&patterns[0], state, capture)?;
    let mut rest = vec![];
    for parser in patterns[1..].iter() {
        rest.push(parse_patterns(parser, state, capture)?);
    }

    Ok(quote! {
        #first #( .or(#rest, #name) )*
    })
}

fn parse_patterns(
    patterns: &Vec<Pattern>,
    state: &CodegenState,
    capture: bool,
) -> Result<TokenStream> {
    if patterns.len() == 1 {
        return parse_pattern(&patterns[0], state, capture);
    }

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
    let err = &state.error_type;
    let data = &state.data_type;
    let ctx = &state.parser_context_name;
    Ok(quote! {
        untwine::parser::<#data, _, #err>(|#ctx| {
            #(
                #parsers
            )*
            Ok(( #(#captured),* ))
        }).unilateral()
    })
}

fn generate_parser_function(parser: &ParserDef, state: &CodegenState) -> Result<TokenStream> {
    let vis = &parser.vis;
    let name = &parser.name;
    let ctx = &state.parser_context_name;
    let typ = &parser.return_type;
    let err = &state.error_type;
    let data = &state.data_type;

    let mut parsers = vec![];

    for pattern in &parser.patterns.patterns {
        let prefix = pattern
            .label
            .clone()
            .map(|ident| quote! {let #ident =})
            .unwrap_or_default();
        let parser = parse_pattern(&pattern.pattern, state, pattern.label.is_some())?;
        parsers.push(quote! {#prefix #parser.parse(#ctx)?;});
    }

    let block = &parser.block;
    let block = if block.stmts.len() == 1 {
        let stmt = &block.stmts[0];
        quote! {#stmt}
    } else {
        quote! {#block}
    };
    Ok(quote! {
        #vis fn #name<'p>(#ctx: &'p untwine::ParserContext<'p, #data>) -> Result<#typ, #err> {
            #(
                #parsers
            )*

            Ok(#block)
        }
    })
}

pub fn generate_parser_block(block: ParserBlock) -> Result<TokenStream> {
    let parser_types = block
        .parsers
        .iter()
        .map(|parser| (parser.name.to_string(), parser.return_type.clone()))
        .collect();

    let parser_name: String = block
        .parsers
        .iter()
        .filter(|p| !matches!(p.vis, Visibility::Inherited))
        .map(|p| p.name.to_string() + "_")
        .collect();
    let mut hasher = DefaultHasher::new();
    parser_name.hash(&mut hasher);
    let hash = hasher.finish();
    let parser_name = Ident::new(&format!("__parser_{hash}"), Span::call_site());

    let mut state = CodegenState {
        parser_context_name: block.header.ctx_name,
        error_type: block.header.error_type,
        data_type: block.header.data_type,
        parser_name: Default::default(),
        parser_types,
    };

    let mut parsers = vec![];
    let mut exports = vec![];
    for parser in block.parsers {
        state.parser_name = parser.name.to_string();
        parsers.push(generate_parser_function(&parser, &state)?);
        if !matches!(parser.vis, Visibility::Inherited) {
            let vis = parser.vis;
            let name = parser.name;
            exports.push(quote! {
                #vis use #parser_name::#name;
            })
        }
    }

    Ok(quote! {
        mod #parser_name {
            use super::*;
            use untwine::{parser, Parser, literal, char_filter};

            #(
                #parsers
            )*
        }
        #(
            #exports
        )*
    })
}

fn numbered_ident(num: usize) -> Ident {
    Ident::new(&format!("_{num}"), Span::call_site())
}

fn fragment_type(fragment: &PatternFragment, parser_types: &HashMap<String, Type>) -> Result<Type> {
    use PatternFragment as P;
    let tokens = match fragment {
        P::Span(_) => quote! {&str},
        P::CharRange(_) | P::CharGroup(_) | P::AnyChar | P::CharFilter(_) => quote! {char},
        P::Ignore(_) | P::Literal(_) => quote! {()},
        P::ParserRef(ident) => {
            let Some(typ) = parser_types.get(&ident.to_string()) else {
                return Err(syn::Error::new_spanned(
                    ident,
                    "Reference to undefined parser",
                ));
            };
            return Ok(typ.clone());
        }
        P::Nested(PatternList::List(l)) => return Ok(list_type(l, parser_types)?),
        P::Nested(PatternList::Choices(c)) => return Ok(list_type(&c[0], parser_types)?),
    };
    Ok(syn::parse(tokens.into())?)
}

fn list_type(patterns: &Vec<Pattern>, parser_types: &HashMap<String, Type>) -> Result<Type> {
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
    Ok(syn::parse(tokens.into())?)
}

pub fn pattern_type(pattern: &Pattern, parser_types: &HashMap<String, Type>) -> Result<Type> {
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
