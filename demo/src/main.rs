use untwine::macros::parser;

fn main() {
    parser! {
        [ctx: Ctx]
        sep: {char::is_whitespace}* -> () {}
        test: a=("hello") b="d" -> () {}
        int: <"-"? '0'-'9'+> -> JSONValue { Int(int.parse()?) }
        float: <"-"? '0'-'9'+ ("." '0'-'9'+)?> -> JSONValue { Float(float.parse()?) }
        str_char: <("\\" . | [^"\""])> -> char { str_char.chars().last().unwrap() /* doesn't handle escape seqs properly */ }
        string: "\"" str_char+ "\"" -> JSONValue { String(str_char.iter().cloned().collect()) }
        null: "null" -> JSONValue { Null }
        bool: "true" | "false" -> JSONValue { Bool(bool == "true") }
        list: "[" (head=json sep rest=("," sep json sep)*)? "]" -> JSONValue { List(collect_from_parts(head, rest)) }
        mapEntry: string sep ":" sep json -> (JSONValue, JSONValue) { (string, json) }
        map: "{" (head=mapEntry sep rest=("," sep mapEntry sep)*)? "}" -> JSONValue { Map(collect_from_parts(head, rest)) }
        pub json: int | float | bool | string | null | list | map -> JSONValue { json }
    }
    println!("Hello, world!");
}
