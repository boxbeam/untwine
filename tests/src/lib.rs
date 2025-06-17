#![allow(unused)]
#![feature(test)]

mod benches;
extern crate test;

#[cfg(test)]
mod tests {
    use std::{
        collections::HashMap,
        num::{ParseFloatError, ParseIntError},
        ops::Range,
    };

    use insta::assert_snapshot;
    use untwine::{
        parse, parse_pretty, parser, prelude::Recoverable, pretty::PrettyOptions, ParserError,
    };

    parser! {
        num: num=<'0'-'9'+> -> u32 { num.parse().unwrap() }
        pub num_list = num$","+ -> Vec<u32>;
    }

    fn operate(left: f64, op: char, right: f64) -> f64 {
        match op {
            '+' => left + right,
            '-' => left - right,
            '/' => left / right,
            '*' => left * right,
            _ => unreachable!(),
        }
    }

    parser! {
        sep = #{char::is_ascii_whitespace}*;
        num: num=<"-"? '0'-'9'+ ("." '0'-'9'+)?> -> f64 { num.parse().unwrap() }
        term = (num | "(" sep expr sep ")") -> f64;
        add: first=mul sep ops=(["+-"] sep mul)* -> f64 { ops.into_iter().fold(first, |left, (op, right)| operate(left, op, right)) }
        mul: first=term sep ops=(["*/"] sep term)* -> f64 { ops.into_iter().fold(first, |left, (op, right)| operate(left, op, right)) }
        pub expr = add -> f64;
    }

    #[test]
    pub fn test_num_list() {
        assert_eq!(parse(num_list, "1,2,3").unwrap(), vec![1, 2, 3]);
        assert_snapshot!(parse_pretty(num_list, "1,2,", PrettyOptions::no_color()).unwrap_err());
    }

    #[test]
    pub fn test_expr() {
        assert_eq!(parse(expr, "1+2*3").unwrap(), 7.0);
        assert_eq!(parse(expr, "1+(3/4)").unwrap(), 1.75);
        assert_eq!(parse(expr, "4--1").unwrap(), 5.0);
        assert_snapshot!(parse_pretty(expr, "1.", PrettyOptions::no_color()).unwrap_err());
        assert_snapshot!(parse_pretty(expr, "(1\n\n+", PrettyOptions::no_color()).unwrap_err());
        assert_snapshot!(parse_pretty(expr, "(1\n\n+5", PrettyOptions::no_color()).unwrap_err());
        assert_snapshot!(parse_pretty(expr, "--1", PrettyOptions::no_color()).unwrap_err());
    }

    #[derive(Debug, Recoverable)]
    pub enum JSONValue {
        String(String),
        Null,
        Int(i128),
        Float(f64),
        Bool(bool),
        List(Vec<JSONValue>),
        Map(HashMap<String, JSONValue>),
        #[recover]
        Error(Range<usize>),
    }

    impl JSONValue {
        pub fn string(self) -> Option<String> {
            match self {
                JSONValue::String(s) => Some(s),
                _ => None,
            }
        }
    }

    #[derive(Debug, thiserror::Error)]
    enum ParseJSONError {
        #[error(transparent)]
        Untwine(#[from] untwine::ParserError),
        #[error("Failed to parse number: {0}")]
        ParseInt(#[from] ParseIntError),
        #[error("Failed to parse number: {0}")]
        ParseFloat(#[from] ParseFloatError),
    }

    parser! {
        [error = ParseJSONError, recover = true]
        sep = #{char::is_ascii_whitespace}*;
        comma = sep "," sep;
        int: num=<"-"? '0'-'9'+> -> JSONValue { JSONValue::Int(num.parse()?) }
        float: num=<"-"? '0'-'9'+ "." '0'-'9'+> -> JSONValue { JSONValue::Float(num.parse()?) }
        str_char = ("\\" . | [^"\""]) -> char;
        str: "\"" chars=str_char* "\"" -> JSONValue { JSONValue::String(chars.into_iter().collect()) }
        null: "null" -> JSONValue { JSONValue::Null }
        bool: bool=<"true" | "false"> -> JSONValue { JSONValue::Bool(bool == "true") }
        list: "[" sep values=json_value$comma* sep "]" -> JSONValue { JSONValue::List(values) }
        map_entry: key=str sep ":" sep value=json_value -> (String, JSONValue) { (key.string().unwrap(), value) }
        map: "{" sep values=map_entry$comma* sep "}" -> JSONValue { JSONValue::Map(values.into_iter().collect()) }
        pub json_value = (bool | null | str | float | int | map | list) -> JSONValue;
    }

    #[test]
    fn test_recovering_json() {
        assert_snapshot!(parse_pretty(
            json_value,
            r#"{"a" true, "b" fals}"#,
            PrettyOptions::no_color()
        )
        .unwrap_err());

        assert_snapshot!(
            parse_pretty(json_value, r#"[[,], 3, 1.]"#, PrettyOptions::no_color()).unwrap_err()
        );

        assert_snapshot!(
            parse_pretty(json_value, r#"[1., 2.5"#, PrettyOptions::no_color()).unwrap_err()
        );

        assert_snapshot!(
            parse_pretty(json_value, r#"[[],"#, PrettyOptions::no_color()).unwrap_err()
        );
    }

    #[test]
    fn test_delimiter_errors() {
        assert_snapshot!(
            parse_pretty(json_value, r#"[1 2]"#, PrettyOptions::no_color()).unwrap_err()
        );

        assert_snapshot!(
            parse_pretty(json_value, r#"[1true"#, PrettyOptions::no_color()).unwrap_err()
        );
    }

    parser! {
        int = <"-"? '0'-'9'+> -> &str;
        float = <"-"? '0'-'9'+ "." '0'-'9'+> -> &str;
        pub number = #(float | int);
    }

    #[test]
    fn test_numbers() {
        assert!(parse(number, "1").is_ok());
        assert!(parse(number, "-1").is_ok());
        assert!(parse(number, "-1.5").is_ok());
        assert!(parse(number, "-1.5a").is_err());
        assert!(parse(number, "").is_err());
    }

    parser! {
        pub paren = ("(" paren* ")")+;
    }

    #[test]
    fn test_parens() {
        assert!(parse(paren, "").is_err());
        assert!(parse(paren, "()").is_ok());
        assert!(parse(paren, "()()").is_ok());
        assert!(parse(paren, "(()").is_err());
        assert!(parse(paren, "(()").is_err());
        assert!(parse(paren, "(()))").is_err());
        assert!(parse(paren, "(()(()))()").is_ok());
    }

    parser! {
        pub optional_after_space = " "* ("?" | "!")$","+;
    }

    #[test]
    fn test_optional_after_space() {
        assert_snapshot!(
            parse_pretty(optional_after_space, " ?,", PrettyOptions::no_color()).unwrap_err()
        );
    }

    #[derive(Debug, Eq, PartialEq)]
    enum CustomError {
        Untwine(ParserError),
    }

    impl From<ParserError> for CustomError {
        fn from(value: ParserError) -> Self {
            CustomError::Untwine(value)
        }
    }

    parser! {
        [error = CustomError]
        pub bool = match {
            "true" => true,
            "false" => false,
        } -> bool;

        pub empty_match = match {};
    }

    #[test]
    fn test_match() {
        assert_eq!(parse(bool, "true").unwrap(), true);
        assert_eq!(
            parse(empty_match, "").unwrap_err()[0].1,
            CustomError::Untwine(ParserError::UnexpectedToken)
        );
    }
}
