use std::{
    collections::HashMap,
    num::{ParseFloatError, ParseIntError},
    ops::Range,
    process::Termination,
};

use serde::Deserialize;
use test::Bencher;
use untwine::prelude::*;

#[derive(Debug, Recoverable, Deserialize)]
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
    comma = sep "," sep;

    digit = '0'-'9' -> char;
    int: num=<'-'? digit+> -> JSONValue { JSONValue::Int(num.parse()?) }
    float: num=<"-"? digit+ "." digit+> -> JSONValue { JSONValue::Float(num.parse()?) }

    hex = #{|c| c.is_digit(16)};
    escape = match {
        "n" => '\n',
        "t" => '\t',
        "r" => '\r',
        "u" code=<#[repeat(4)] hex> => {
            char::from_u32(u32::from_str_radix(code, 16)?)
                .ok_or_else(|| ParseJSONError::InvalidHexCode(code.to_string()))?
        },
        c=[^"u"] => c,
    } -> char;

    str_char = ("\\" escape | [^"\"\\"]) -> char;
    str: '"' chars=str_char*  '"' -> String { chars.into_iter().collect() }

    null: "null" -> JSONValue { JSONValue::Null }

    bool = match {
        "true" => JSONValue::Bool(true),
        "false" => JSONValue::Bool(false),
    } -> JSONValue;

    list: "[" sep values=json_value$comma* sep "]" -> JSONValue { JSONValue::List(values) }

    map_entry: key=str sep ":" sep value=json_value -> (String, JSONValue) { (key, value) }

    map: "{" sep values=map_entry$comma* sep "}" -> JSONValue { JSONValue::Map(values.into_iter().collect()) }

    pub json_value = (bool | null | #[convert(JSONValue::String)] str | float | int | map | list) sep? -> JSONValue;
}

const JSON: &str = include_str!("sample2.json");

#[bench]
fn benchmark_json(bench: &mut Bencher) {
    bench.iter(|| untwine::parse(json_value, JSON).unwrap());
}

#[bench]
fn benchmark_json5_nodes(bench: &mut Bencher) {
    bench.iter(|| json5_nodes::parse(JSON).unwrap());
}

#[bench]
fn benchmark_serde_json(bencher: &mut Bencher) {
    bencher.iter(|| serde_json::from_str::<serde_json::Value>(JSON).unwrap());
}
