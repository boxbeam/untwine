use std::{
    collections::HashMap,
    num::{ParseFloatError, ParseIntError},
    ops::Range,
};

use untwine::{parser, parser_repl, prelude::Recoverable};

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

#[derive(Debug, thiserror::Error)]
enum ParseJSONError {
    #[error(transparent)]
    Untwine(#[from] untwine::ParserError),
    #[error("Failed to parse number: {0}")]
    ParseInt(#[from] ParseIntError),
    #[error("Failed to parse number: {0}")]
    ParseFloat(#[from] ParseFloatError),
    #[error("Invalid hex code: {0}")]
    InvalidHexCode(String),
}

parser! {
    [error = ParseJSONError, recover = true]
    sep = #["\n\r\t "]*;
    comma = (sep "," sep);

    digit = '0'-'9' -> char;
    int: num=<'-'? digit+> -> JSONValue { JSONValue::Int(num.parse()?) }
    float: num=<"-"? digit+ "." digit+> -> JSONValue { JSONValue::Float(num.parse()?) }

    hex = #{|c| c.is_digit(16)};
    escape = match {
        "n" => '\n',
        "t" => '\t',
        "r" => '\r',
        "u" code=<hex hex hex hex> => {
            char::from_u32(u32::from_str_radix(code, 16)?)
                .ok_or_else(|| ParseJSONError::InvalidHexCode(code.to_string()))?
        },
    } -> char;

    str_char = ("\\" escape | [^"\""]) -> char;
    str: '"' chars=str_char*  '"' -> String { chars.into_iter().collect() }

    null: "null" -> JSONValue { JSONValue::Null }

    bool = match {
        "true" => JSONValue::Bool(true),
        "false" => JSONValue::Bool(false),
    } -> JSONValue;

    list: "[" sep values=(json_value)$comma* sep "]" -> JSONValue { JSONValue::List(values) }

    map_entry: key=str sep ":" sep value=json_value -> (String, JSONValue) { (key, value) }

    map: "{" sep values=map_entry$comma* sep "}" -> JSONValue { JSONValue::Map(values.into_iter().collect()) }

    pub json_value = (bool | null | #[convert(JSONValue::String)] str | float | int | map | list) -> JSONValue;
}

fn main() {
    parser_repl(json_value);
}
