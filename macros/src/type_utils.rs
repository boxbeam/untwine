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

    fn define(&self, var: &Ident) -> TokenStream {
        match self.wrappers.front() {
            Some(Wrapper::Vec) => {
                let stream = quote! {let mut #var;};
                stream
            }
            Some(Wrapper::Stack) => todo!(),
            Some(Wrapper::Option) => quote! {let mut #var},
            None => quote! {let #var;},
        }
    }
}

#[derive(Clone, Copy)]
pub enum Wrapper {
    Vec,
    Stack,
    Option,
}

struct CodegenState {
    parser_context_name: Ident,
    last_stack: Ident,
    parser_name: String,
    label_types: Vec<(Ident, ParserType)>,
    current_capture: Option<Ident>,
    parser_types: Rc<HashMap<String, Type>>,
}

impl CodegenState {
    fn label_type(&self, ident: &Ident) -> &ParserType {
        self.label_types
            .iter()
            .find_map(|(label, typ)| (label == ident).then_some(typ))
            .unwrap()
    }
}

impl Wrapper {
    fn wrap(&self, typ: Type) -> Type {
        match self {
            Wrapper::Vec => vec_of(&typ),
            Wrapper::Stack => todo!(),
            Wrapper::Option => optional(&typ),
        }
    }

    fn init(&self, var: &Ident, last_stack: &mut Ident) -> TokenStream {
        match self {
            Wrapper::Stack => {
                let tokens = quote! {
                    #var = #last_stack.stack();
                };
                *last_stack = var.clone();
                tokens
            }
            Wrapper::Vec => quote! {#var = vec![]},
            Wrapper::Option => quote! {},
        }
    }

    fn insert(&self, var: &Ident, parse: &TokenStream) -> TokenStream {
        match self {
            Wrapper::Vec | Wrapper::Stack => quote! {#var.push(#parse);},
            Wrapper::Option => quote! {#var.insert(#parse);},
        }
    }
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
        PatternFragment::Labeled(pattern) => {
            let typ = state.label_type(&pattern.label);
            let parse_inner = parse_fragment(&pattern.pattern.fragment, state);
            typ.insert(&pattern.label, &parse_inner)
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

fn parse_patterns_top_level(
    parser_name: String,
    parser_context_name: Ident,
    patterns: &Vec<Pattern>,
    parser_types: Rc<HashMap<String, Type>>,
) -> Result<TokenStream, syn::Error> {
    // "[" thing=digit+ "]"
    let label_types = get_label_types(patterns, &*parser_types)?;

    // Define all variables for capture
    let var_init: Vec<TokenStream> = label_types
        .iter()
        .map(|(var, pattern)| pattern.define(var))
        .collect();

    let state = CodegenState {
        parser_context_name,
        // TODO implement stack optimization
        last_stack: syn::parse(quote! { TODO }.into()).unwrap(),
        parser_name,
        label_types,
        current_capture: None,
        parser_types: parser_types.clone(),
    };

    let mut parsers = vec![];
    for pattern in patterns {
        parsers.push(parse_pattern(pattern, &state));
    }
    Ok(quote! {
        #(#var_init)*
        #(#parsers)*
    })
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
    patterns: &Vec<Pattern>,
    parser_types: &HashMap<String, Type>,
) -> syn::Result<Vec<(Ident, ParserType)>> {
    let mut label_types = vec![];
    let mut wrappers = vec![];
    let mut seen_names = HashSet::new();
    populate_label_types_recursive(
        patterns,
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
