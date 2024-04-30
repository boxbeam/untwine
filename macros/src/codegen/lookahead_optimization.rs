use std::collections::{HashMap, HashSet};

use proc_macro2::TokenStream;
use quote::quote;

use crate::{
    codegen::parse_pattern_choices, Modifier, ParserBlock, Pattern, PatternFragment, PatternList,
};

use super::CodegenState;

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum NextChar {
    Char(char),
    ParserRef(String),
    Any,
}

pub fn generate_lookahead_table(
    choices: &Vec<Vec<Pattern>>,
    state: &CodegenState,
    capture: bool,
) -> syn::Result<TokenStream> {
    let mut map: HashMap<Option<char>, Vec<usize>> = HashMap::new();

    for (i, choice) in choices.iter().enumerate() {
        let lookaheads = get_patterns_lookahead(choice.iter());
        let lookaheads = resolve_lookaheads(&lookaheads, &state.parser_lookaheads);
        for lookahead in lookaheads {
            map.entry(lookahead).or_default().push(i);
        }
    }

    let mut branches: Vec<TokenStream> = vec![];
    let mut parsers: Vec<TokenStream> = vec![];

    for (branch, indices) in &map {
        let selected_choices: HashSet<_> = indices
            .iter()
            .chain(map.get(&None).into_iter().flatten())
            .collect();

        let mut selected_choices: Vec<_> = selected_choices.into_iter().collect();
        selected_choices.sort();

        let selected_choices: Vec<Vec<Pattern>> = selected_choices
            .into_iter()
            .map(|i| choices[*i].clone())
            .collect();

        branches.push(match branch {
            Some(c) => quote! { Some(#c) },
            None => quote! { None },
        });
        parsers.push(parse_pattern_choices(&selected_choices, state, capture)?);
    }

    let mut any_case: Vec<_> = map.get(&None).iter().flat_map(|v| v.iter()).collect();
    any_case.sort();

    let any_case: Vec<Vec<Pattern>> = any_case.into_iter().map(|i| choices[*i].clone()).collect();
    let any_case = parse_pattern_choices(&any_case, state, capture)?;

    let ctx = &state.parser_context_name;
    let data = &state.data_type;
    let err = &state.error_type;

    Ok(quote! {
        untwine::parser::parser::<#data, _, #err>(|#ctx| {
            match #ctx.slice().chars().next() {
                #(
                    #branches => #parsers.parse(#ctx),
                )*
                _ => #any_case.parse(#ctx)
            }
        })
    })
}

pub fn generate_lookahead_optional(
    fragment: &PatternFragment,
    parser: TokenStream,
    default: TokenStream,
    state: &CodegenState,
) -> syn::Result<TokenStream> {
    let lookaheads = get_fragment_lookahead(fragment);
    let lookaheads = resolve_lookaheads(&lookaheads, &state.parser_lookaheads);
    if lookaheads.contains(&None) || lookaheads.is_empty() {
        return Ok(parser);
    }
    let chars: Vec<char> = lookaheads.iter().flatten().cloned().collect();
    let ctx = &state.parser_context_name;
    let data = &state.data_type;
    let err = &state.error_type;

    Ok(quote! {
        untwine::parser::parser::<#data, _, #err>(|#ctx| {
            match #ctx.slice().chars().next() {
                Some( #(#chars)|* ) => {
                    #parser.parse(#ctx)
                },
                _ => Some(#default),
            }
        })
    })
}

fn resolve_lookaheads(
    lookaheads: &[NextChar],
    parser_lookaheads: &HashMap<String, Vec<NextChar>>,
) -> Vec<Option<char>> {
    let mut seen = HashSet::new();
    let mut chars = HashSet::new();

    let mut stack: Vec<_> = lookaheads.iter().cloned().collect();

    while let Some(lookahead) = stack.pop() {
        match lookahead {
            NextChar::Char(c) => {
                chars.insert(Some(c));
            }
            NextChar::ParserRef(name) => {
                if !seen.insert(name.clone()) {
                    continue;
                }
                stack.extend(parser_lookaheads[&name].iter().cloned());
            }
            NextChar::Any => {
                chars.insert(None);
            }
        }
    }

    chars.into_iter().collect()
}

pub fn build_lookahead_map(parsers: &ParserBlock) -> HashMap<String, Vec<NextChar>> {
    parsers
        .parsers
        .iter()
        .map(|parser| {
            (
                parser.name.to_string(),
                get_patterns_lookahead(parser.patterns.patterns.iter().map(|p| &p.pattern)),
            )
        })
        .collect()
}

fn get_pattern_lookahead(pattern: &Pattern) -> Vec<NextChar> {
    let mut lookaheads = get_fragment_lookahead(&pattern.fragment);
    if matches!(
        pattern.modifier,
        Some(Modifier::Optional | Modifier::OptionalRepeating | Modifier::OptionalDelimited(_))
    ) && !lookaheads.contains(&NextChar::Any)
    {
        lookaheads.push(NextChar::Any);
    }
    lookaheads
}

fn get_fragment_lookahead(fragment: &PatternFragment) -> Vec<NextChar> {
    match fragment {
        PatternFragment::Literal(lit) => vec![lit
            .value()
            .chars()
            .next()
            .map(NextChar::Char)
            .unwrap_or(NextChar::Any)],
        PatternFragment::CharRange(range) => {
            if range.inverted {
                vec![NextChar::Any]
            } else {
                range.range.clone().map(NextChar::Char).collect()
            }
        }
        PatternFragment::CharGroup(group) => {
            if group.inverted {
                vec![NextChar::Any]
            } else {
                group.chars.iter().copied().map(NextChar::Char).collect()
            }
        }
        PatternFragment::CharFilter(_) => vec![NextChar::Any],
        PatternFragment::ParserRef(ident) => vec![NextChar::ParserRef(ident.to_string())],
        PatternFragment::Ignore(inner) => get_pattern_lookahead(&inner.pattern),
        PatternFragment::Span(inner) => get_pattern_list_lookahead(&inner),
        PatternFragment::Nested(inner) => get_pattern_list_lookahead(&inner),
        PatternFragment::Annotated(_) => vec![NextChar::Any],
        PatternFragment::AnyChar => vec![NextChar::Any],
    }
}

fn get_pattern_list_lookahead(list: &PatternList) -> Vec<NextChar> {
    match list {
        PatternList::List(list) => get_patterns_lookahead(list.iter()),
        PatternList::Choices(choices) => {
            let lookaheads: HashSet<_> = choices
                .iter()
                .flat_map(|choice| get_patterns_lookahead(choice.iter()))
                .collect();
            lookaheads.into_iter().collect()
        }
    }
}

fn get_patterns_lookahead<'a>(
    patterns: impl ExactSizeIterator<Item = &'a Pattern>,
) -> Vec<NextChar> {
    let mut next = vec![];
    let mut all_optional = true;
    for pattern in patterns {
        let mut lookahead = get_pattern_lookahead(pattern);
        let optional = lookahead
            .iter()
            .position(|c| c == &NextChar::Any)
            .map(|i| lookahead.remove(i))
            .is_some();
        next.extend(lookahead);
        if !optional {
            all_optional = false;
            break;
        }
    }
    if all_optional {
        next.push(NextChar::Any);
    }
    next
}
