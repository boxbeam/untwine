use crate::{Modifier, Pattern, PatternFragment, PatternList};
use quote::quote;

impl ToString for Pattern {
    fn to_string(&self) -> String {
        let fragment = self.fragment.to_string();
        match &self.modifier {
            None => fragment,
            Some(Modifier::Optional) => format!("{fragment}?"),
            Some(Modifier::Repeating(_)) => format!("{fragment}+"),
            Some(Modifier::OptionalRepeating(_)) => format!("{fragment}*"),
            Some(Modifier::Delimited(delim, _)) => {
                let delim = delim.to_string();
                format!("{fragment}${delim}+")
            }
            Some(Modifier::OptionalDelimited(delim, _)) => {
                let delim = delim.to_string();
                format!("{fragment}${delim}*")
            }
        }
    }
}

impl ToString for PatternList {
    fn to_string(&self) -> String {
        match self {
            PatternList::List(list) => {
                let list: Vec<String> = list.iter().map(Pattern::to_string).collect();
                list.join(" ")
            }
            PatternList::Choices(choices) => {
                let choices_joined: Vec<String> = choices
                    .iter()
                    .map(|list| {
                        let list: Vec<String> = list.iter().map(Pattern::to_string).collect();
                        list.join(" ")
                    })
                    .collect();
                choices_joined.join(" | ")
            }
        }
    }
}

impl ToString for PatternFragment {
    fn to_string(&self) -> String {
        match self {
            PatternFragment::Literal(lit) => format!("{:?}", lit.value()),
            PatternFragment::CharRange(range) => {
                format!(
                    "{}{:?}-{:?}",
                    range.inverted.then_some("^").unwrap_or_default(),
                    range.range.start(),
                    range.range.end()
                )
            }
            PatternFragment::CharGroup(group) => format!(
                "[{}{:?}]",
                group.inverted.then_some("^").unwrap_or_default(),
                group.chars
            ),
            PatternFragment::CharFilter(filter) => {
                let filter = &filter.expr;
                format!("{{{:?}}}", quote! {#filter}.to_string())
            }
            PatternFragment::ParserRef(ident) => ident.to_string(),
            PatternFragment::Ignore(ignored) => format!("#{}", ignored.pattern.to_string()),
            PatternFragment::Span(span) => format!("<{}>", span.to_string()),
            PatternFragment::Nested(nested) => format!("({})", nested.to_string()),
            PatternFragment::Annotated(annotated) => annotated.pattern.to_string(),
            PatternFragment::AnyChar => ".".to_string(),
        }
    }
}
