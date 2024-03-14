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
    last_stack: Ident,
    parser_name: String,
    label_types: Vec<(Ident, Type)>,
    current_capture: Option<Ident>,
    parser_types: Rc<HashMap<String, Type>>,
}

fn parse_fragment(fragment: &PatternFragment, state: &CodegenState) -> TokenStream {
    let ctx = &state.parser_context_name;
    let last_stack = &state.last_stack;
    let parser_name = &state.parser_name;
    match fragment {
        PatternFragment::Literal(lit) => {
            let literal = &lit.string;
            let case_sensitive = lit.case_sensitive;
            quote! {
                #ctx.literal(#literal, #case_sensitive)?
            }
        }
        PatternFragment::CharRange(range) => {
            let inverted = range.inverted.then(|| quote! {!});
            let range_min = range.range.start();
            let range_max = range.range.end();
            quote! {
                #ctx.char_filter(|c| #inverted (#range_min ..= #range_max).contains(c), #parser_name)?
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
                #ctx.char_filter(|c| #inverted #chars.contains(c), #parser_name)?
            }
        }
        PatternFragment::CharFilter(filter) => {
            let filter = &filter.expr;
            quote! {
                #ctx.char_filter(#filter, #parser_name)?
            }
        }
        PatternFragment::ParserRef(parser) => {
            quote! {
                #parser(#ctx.with_split(#last_stack.any()))?
            }
        }
        PatternFragment::Ignore(pattern) => {
            let parse_inner = parse_fragment(&pattern.pattern.fragment, state);
            quote! { drop(#parse_inner) }
        }
        PatternFragment::Span(span) => {
            let parse_inner = parse_pattern_list(span, state);
            quote! {
                {
                    let start = #ctx.cur;
                    #parse_inner;
                    start[..start.len() - #ctx.cur.len()]
                }
            }
        }
        PatternFragment::Nested(list) => parse_pattern_list(list, state),
        PatternFragment::AnyChar => {
            quote! {#ctx.char_filter(|_| true, "more content")}
        }
    }
}

fn parse_pattern(pattern: &Pattern, state: &CodegenState) -> TokenStream {
    // Parse pattern fragment and wrap in any necessary modifier logic
    todo!()
}

fn children(list: &PatternList) -> Vec<&Pattern> {
    match list {
        PatternList::List(patterns) => patterns.iter().collect(),
        PatternList::Choices(choices) => choices.iter().flatten().collect(),
    }
}

fn parse_pattern_list(patterns: &PatternList, state: &CodegenState) -> TokenStream {
    todo!()
}

fn parse_patterns(
    patterns: &Vec<Pattern>,
    state: &CodegenState,
) -> Result<TokenStream, syn::Error> {
    let mut tuple_params = 0;
    let mut parsers = vec![];
    for pattern in patterns {
        let typ = get_type(pattern, &state.parser_types)?;
        let prefix = (!is_unit(&typ)).then(|| {
            let ident = numbered_ident(tuple_params);
            tuple_params += 1;
            quote! {let #ident =}
        });
        let parse = parse_fragment(&pattern.fragment, state);
        parsers.push(quote! {#prefix #parse});
    }
    let tuple_names = (0..tuple_params).map(numbered_ident);
    Ok(quote! {
        {
            #(#parsers)*
            (#(#tuple_names),*)
        }
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
        P::Literal(_) | P::Span(_) => quote! {&str},
        P::CharRange(_) | P::CharGroup(_) | P::AnyChar | P::CharFilter(_) => quote! {char},
        P::Ignore(_) => quote! {()},
        P::ParserRef(ident) => return Ok(parser_types[&ident.to_string()].clone()),
        P::Nested(PatternList::List(l)) => return Ok(list_type(l, parser_types)?),
        P::Nested(PatternList::Choices(c)) => return Ok(list_type(&c[0], parser_types)?),
    };
    Ok(syn::parse(tokens.into()).unwrap())
}

fn list_type(patterns: &Vec<Pattern>, parser_types: &HashMap<String, Type>) -> syn::Result<Type> {
    let mut tuple = vec![];
    for pattern in patterns {
        let typ = get_type(pattern, parser_types)?;
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

pub fn get_type(pattern: &Pattern, parser_types: &HashMap<String, Type>) -> syn::Result<Type> {
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
