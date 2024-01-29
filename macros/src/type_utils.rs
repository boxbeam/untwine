use std::collections::HashMap;

use proc_macro2::{Ident, Span, TokenStream};
use quote::quote;
use syn::{
    punctuated::Punctuated,
    token::{Gt, Lt},
    AngleBracketedGenericArguments, Path, PathArguments, PathSegment, Type, TypePath, TypeTuple,
};

use crate::{Modifier, Pattern, PatternFragment, PatternList};

pub fn optional(typ: &Type) -> Type {
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
        P::Labeled(pat) => return Ok(fragment_type(&pat.pattern.fragment, parser_types)?),
        P::Nested(PatternList::List(l)) => return Ok(list_type(l, parser_types)?),
        P::Nested(PatternList::Choices(c)) => return Ok(list_type(&c[0], parser_types)?),
    };
    Ok(syn::parse(tokens.into()).unwrap())
}

fn list_type(patterns: &Vec<Pattern>, parser_types: &HashMap<String, Type>) -> syn::Result<Type> {
    let mut tuple = vec![];
    for pattern in patterns {
        let typ = get_type(pattern, parser_types)?;
        if !is_unit(&typ.typ) {
            tuple.push(typ.typ);
        }
    }

    let tokens = quote! {
        (
            #(#tuple),*
        )
    };
    Ok(syn::parse(tokens.into()).unwrap())
}

pub fn get_type(
    pattern: &Pattern,
    parser_types: &HashMap<String, Type>,
) -> syn::Result<ParserType> {
    let typ = fragment_type(&pattern.fragment, parser_types)?;
    let wrapper = pattern
        .modifier
        .clone()
        .map(Wrapper::from)
        .unwrap_or(Wrapper::None);
    Ok(ParserType { typ, wrapper })
}

pub struct ParserType {
    typ: Type,
    wrapper: Wrapper,
}

#[derive(Clone, Copy)]
pub enum Wrapper {
    None,
    Vec,
    Option,
}

struct CodegenState {
    ctx: Ident,
    last_stack: Ident,
    parser_name: String,
    label_types: HashMap<Ident, ParserType>,
}

impl Wrapper {
    fn combine(&self, other: Wrapper) -> Wrapper {
        match (self, other) {
            (Wrapper::None, _) => other,
            (Wrapper::Vec, _) => Wrapper::Vec,
            (Wrapper::Option, Wrapper::Vec) | (Wrapper::Vec, Wrapper::Option) => Wrapper::Vec,
            (Wrapper::Option, _) => Wrapper::Option,
        }
    }

    fn wrap(&self, typ: Type) -> Type {
        match self {
            Wrapper::None => typ,
            Wrapper::Vec => vec_of(&typ),
            Wrapper::Option => optional(&typ),
        }
    }

    fn define(&self, var: &Ident, last_stack: &mut Ident) -> TokenStream {
        match self {
            Wrapper::None => quote! {let #var;},
            Wrapper::Vec => {
                let stream = quote! {let mut #var;};
                *last_stack = var.clone();
                stream
            }
            Wrapper::Option => quote! {let mut #var = None;},
        }
    }

    fn init(&self, var: &Ident, last_stack: &mut Ident) -> TokenStream {
        match self {
            Wrapper::None => quote! {},
            Wrapper::Vec => {
                let tokens = quote! {
                    #var = #last_stack.stack();
                };
                *last_stack = var.clone();
                tokens
            }
            Wrapper::Option => todo!(),
        }
    }

    fn insert(&self, var: &Ident, parse: &TokenStream) -> TokenStream {
        match self {
            Wrapper::None => quote! {#var = #parse;}.into(),
            Wrapper::Vec => quote! {#var.push(#parse);},
            Wrapper::Option => quote! {#var.insert(#parse);},
        }
    }
}

fn parse_value(fragment: &PatternFragment, state: &CodegenState) -> TokenStream {
    let ctx = &state.ctx;
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
            let inverted = if range.inverted {
                quote! {!}
            } else {
                quote! {}
            };
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
        PatternFragment::Labeled(pattern) => {
            let typ = &state.label_types[&pattern.label];
            let parse_inner = parse_value(&pattern.pattern.fragment, state);
            typ.wrapper.insert(&pattern.label, &parse_inner)
        }
        PatternFragment::Ignore(pattern) => {
            let parse_inner = parse_value(&pattern.pattern.fragment, state);
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

fn parse_pattern_list(patterns: &PatternList, state: &CodegenState) -> TokenStream {
    todo!()
}

impl From<Modifier> for Wrapper {
    fn from(value: Modifier) -> Self {
        match value {
            Modifier::Optional => Wrapper::Option,
            Modifier::Repeating => Wrapper::Vec,
            Modifier::OptionalRepeating => Wrapper::Vec,
        }
    }
}

pub fn get_label_types(
    patterns: PatternList,
    parser_types: &HashMap<String, Type>,
) -> syn::Result<HashMap<Ident, ParserType>> {
    let mut label_types = HashMap::new();
    let list: Vec<_> = match &patterns {
        PatternList::List(l) => l.iter().collect(),
        PatternList::Choices(c) => c.iter().flatten().collect(),
    };
    populate_label_types_recursive(list, parser_types, &mut label_types, Wrapper::None)?;
    Ok(label_types)
}

fn populate_label_types_recursive<'a>(
    children: impl IntoIterator<Item = &'a Pattern>,
    parser_types: &HashMap<String, Type>,
    label_types: &mut HashMap<Ident, ParserType>,
    parent_wrapper: Wrapper,
) -> syn::Result<()> {
    for child in children {
        if let PatternFragment::Labeled(labeled) = &child.fragment {
            let mut typ = get_type(child, parser_types)?;
            typ.wrapper = typ.wrapper.combine(parent_wrapper);
            if label_types.insert(labeled.label.clone(), typ).is_some() {
                return Err(syn::Error::new_spanned(
                    labeled.label.clone(),
                    "Duplicate variable binding",
                ));
            }
        } else {
            let child_wrapper = child
                .modifier
                .clone()
                .map(Wrapper::from)
                .unwrap_or(Wrapper::None);
            populate_label_types_recursive(
                child.fragment.children(),
                parser_types,
                label_types,
                parent_wrapper.combine(child_wrapper),
            );
        }
    }
    Ok(())
}
