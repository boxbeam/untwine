use std::{
    collections::HashMap,
    num::{ParseFloatError, ParseIntError},
};

use untwine::{parser, parser_repl};

#[derive(Debug)]
pub enum JSONValue {
    String(String),
    Null,
    Int(i128),
    Float(f64),
    Bool(bool),
    List(Vec<JSONValue>),
    Map(HashMap<String, JSONValue>),
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
    #[error("Syntax error: {0}")]
    Untwine(#[from] untwine::ParserError),
    #[error("Failed to parse number: {0}")]
    ParseInt(#[from] ParseIntError),
    #[error("Failed to parse number: {0}")]
    ParseFloat(#[from] ParseFloatError),
}

parser! {
    [error = ParseJSONError]
    sep = #{char::is_ascii_whitespace}*;
    comma = sep "," sep;
    int: num=<"-"? '0'-'9'+> -> JSONValue { JSONValue::Int(num.parse()?) }
    float: num=<"-"? '0'-'9'+ "." '0'-'9'+> -> JSONValue { JSONValue::Float(num.parse()?) }
    str_char = ("\\" . | [^"\""]) -> char;
    str: "\"" chars=str_char* "\"" -> JSONValue { JSONValue::String(chars.into_iter().collect()) }
    null: "null" -> JSONValue { JSONValue::Null }
    bool: bool=<"true" | "false"> -> JSONValue { JSONValue::Bool(bool == "true") }
    list: "[" sep values=json$comma* sep "]" -> JSONValue { JSONValue::List(values) }
    map_entry: key=str sep ":" sep value=json -> (String, JSONValue) { (key.string().unwrap(), value) }
    map: "{" sep values=map_entry$comma*  sep "}" -> JSONValue { JSONValue::Map(values.into_iter().collect()) }
    pub json = (bool | null | str | float | int | list | map) -> JSONValue;
}

fn main() {
    parser_repl(json);
}
