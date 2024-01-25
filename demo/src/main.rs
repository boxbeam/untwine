use untwine::parser;

struct MyContext {}

parser! {
    [ctx: MyContext]
    digit: "a"+ ['0'-'9']+ ("true" | "false") (a b c d) -> char {
        digit
    }
}

fn main() {
    println!("Hello, world!");
}
