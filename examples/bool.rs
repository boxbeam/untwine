use untwine::{parser, parser_repl};

fn operate(first: bool, op: char, second: bool) -> bool {
    match op {
        '&' => first && second,
        '|' => first || second,
        _ => unreachable!(),
    }
}

parser! {
    sep = #{|c| c.is_ascii_whitespace()}*;

    boolean = match {
        "true" => true,
        "false" => false,
    } -> bool;

    negation: "!" sep value=term -> bool {
        !value
    }

    term = (boolean | negation | "(" sep expr sep ")") -> bool;

    operator = ["&|"] -> char;

    pub expr: first=term sep rest=(operator sep term)* -> bool {
        rest.into_iter().fold(first, |left, (op, right)| operate(left, op, right))
    }
}

fn main() {
    parser_repl(expr);
}
