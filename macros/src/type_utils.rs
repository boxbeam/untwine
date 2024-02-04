use std::collections::{HashMap, HashSet, VecDeque};

use proc_macro2::{Ident, Span, TokenStream};
use quote::{quote, TokenStreamExt};
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

pub struct ParserType {
    typ: Type,
    wrappers: VecDeque<Wrapper>,
}

impl ParserType {
    fn insert(&self, ident: &Ident, inner_value: &TokenStream) -> TokenStream {
        todo!()
    }
}

#[derive(Clone, Copy)]
pub enum Wrapper {
    Vec,
    Option,
}

struct CodegenState {
    ctx: Ident,
    last_stack: Ident,
    parser_name: String,
    label_types: HashMap<Ident, ParserType>,
    parser_types: HashMap<String, Type>,
}

impl Wrapper {
    fn combine(&self, other: Wrapper) -> Wrapper {
        match (self, other) {
            (Wrapper::Vec, _) => Wrapper::Vec,
            (Wrapper::Option, Wrapper::Vec) | (Wrapper::Vec, Wrapper::Option) => Wrapper::Vec,
            (Wrapper::Option, _) => Wrapper::Option,
        }
    }

    fn wrap(&self, typ: Type) -> Type {
        match self {
            Wrapper::Vec => vec_of(&typ),
            Wrapper::Option => optional(&typ),
        }
    }

    fn define(&self, var: &Ident, last_stack: &mut Ident) -> TokenStream {
        match self {
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
        PatternFragment::Labeled(pattern) => {
            let typ = &state.label_types[&pattern.label];
            let parse_inner = parse_value(&pattern.pattern.fragment, state);
            typ.insert(&pattern.label, &parse_inner)
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

fn label(fragment: &PatternFragment) -> Option<&Ident> {
    match fragment {
        PatternFragment::Literal(_) => None,
        PatternFragment::CharRange(_) => None,
        PatternFragment::CharGroup(_) => None,
        PatternFragment::CharFilter(_) => None,
        PatternFragment::ParserRef(_) => None,
        PatternFragment::Labeled(l) => Some(&l.label),
        PatternFragment::Ignore(l) => label(&l.pattern.fragment),
        PatternFragment::Span(list) | PatternFragment::Nested(list) => {
            children(list).into_iter().find_map(|p| label(&p.fragment))
        }
        PatternFragment::AnyChar => None,
    }
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
        let prefix = (!is_unit(&typ.typ)).then(|| {
            let ident = numbered_ident(tuple_params);
            tuple_params += 1;
            quote! {let #ident =}
        });
        let parse = parse_value(&pattern.fragment, state);
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

fn parse_patterns_top_level(
    patterns: &Vec<Pattern>,
    parser_types: &HashMap<String, Type>,
) -> TokenStream {
    // initialize each pattern (Wrapper::init) before parsing
    todo!()
}

fn numbered_ident(num: usize) -> Ident {
    Ident::new(&format!("_{num}"), Span::call_site())
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
    let wrappers = pattern
        .modifier
        .clone()
        .map(Wrapper::from)
        .into_iter()
        .collect();
    Ok(ParserType { typ, wrappers })
}

pub fn get_label_types(
    patterns: PatternList,
    parser_types: &HashMap<String, Type>,
) -> syn::Result<Vec<(Ident, ParserType)>> {
    let mut label_types = vec![];
    let children: Vec<_> = match &patterns {
        PatternList::List(l) => l.iter().collect(),
        PatternList::Choices(c) => c.iter().flatten().collect(),
    };
    let mut wrappers = vec![];
    let mut seen_names = HashSet::new();
    populate_label_types_recursive(
        children,
        parser_types,
        &mut label_types,
        &mut seen_names,
        &mut wrappers,
    )?;
    Ok(label_types)
}

fn extend_front<T>(vec: &mut VecDeque<T>, elems: impl IntoIterator<Item = T>) {
    for elem in elems {
        vec.push_front(elem);
    }
}

fn populate_label_types_recursive<'a>(
    children: impl IntoIterator<Item = &'a Pattern>,
    parser_types: &HashMap<String, Type>,
    label_types: &mut Vec<(Ident, ParserType)>,
    seen_names: &mut HashSet<String>,
    parent_wrappers: &mut Vec<Wrapper>,
) -> syn::Result<()> {
    for child in children {
        let parent_wrappers_len = parent_wrappers.len();
        if let PatternFragment::Labeled(labeled) = &child.fragment {
            let label = labeled.label.clone();
            let mut typ = get_type(&labeled.pattern, parser_types)?;
            extend_front(&mut typ.wrappers, parent_wrappers.iter().cloned());
            if !seen_names.insert(label.to_string()) {
                return Err(syn::Error::new_spanned(label, "Duplicate variable name"));
            }
            label_types.push((label, typ));
        } else {
            parent_wrappers.extend(child.modifier.as_ref().cloned().map(Wrapper::from));
            if let PatternFragment::Nested(PatternList::Choices(choices)) = &child.fragment {
                parent_wrappers.push(Wrapper::Option);
            }
            populate_label_types_recursive(
                child.fragment.children(),
                parser_types,
                label_types,
                seen_names,
                parent_wrappers,
            );
        }
        parent_wrappers.truncate(parent_wrappers_len);
    }
    Ok(())
}
