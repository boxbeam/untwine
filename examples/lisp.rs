use std::{ops::Range, rc::Rc};

use untwine::{parser, parser_repl, prelude::Recoverable};

#[derive(Clone, Debug, Default, Recoverable)]
enum Cons<T> {
    Node(Rc<(T, Cons<T>)>),
    #[default]
    #[recover]
    End,
}

#[derive(Debug, Recoverable)]
enum Expr {
    List(Cons<Expr>),
    Eval(Cons<Expr>),
    Integer(i64),
    Ident(String),
    String(String),
    Bool(bool),
    #[recover]
    Error(Range<usize>),
}

fn valid_ident_char(c: &char) -> bool {
    match c {
        '(' | ')' | '[' | ']' | '{' | '}' | '"' | ' ' | '\t' | '\n' | '\r' => false,
        _ => true,
    }
}

parser! {
    [recover = true]
    sep = #[" \r\n\t"]+;
    int: num=<"-"? '0'-'9'+> -> Expr { Expr::Integer(num.parse().unwrap()) }
    ident: name=<{|c| valid_ident_char(c) && !c.is_ascii_digit()} {valid_ident_char}*> -> Expr { Expr::Ident(name.to_string()) }
    string: "\"" chars=("\\" . | [^"\""])* "\"" -> Expr { Expr::String(chars.into_iter().collect()) }
    bool: bool=<"true" | "false"> -> Expr { Expr::Bool(bool == "true") }
    node: head=expr sep rest=nodes -> Cons<Expr> { Cons::Node(Rc::new((head, rest))) }
    tail: node=expr -> Cons<Expr> { Cons::Node(Rc::new((node, Cons::End))) }
    nodes = (node | tail) -> Cons<Expr>;
    eval: "(" sep? nodes=nodes? sep? ")" -> Expr { Expr::Eval(nodes.unwrap_or_default()) }
    list: "[" sep? nodes=nodes? sep? "]" -> Expr { Expr::List(nodes.unwrap_or_default()) }
    pub expr = (int | bool | ident | eval | list | string) -> Expr;
}

fn main() {
    parser_repl(expr);
}

parser! {
    word:
        chars=<'a'-'z'+>
    -> String {
        chars.into()
    }

    word_list = word$" "+ -> Vec<String>;
}
