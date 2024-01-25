use untwine::parser;

struct MyContext {}

parser! {
    [ctx: MyContext]
    digit: "a"+ ['0'-'9']+ -> char {
        digit
    }
}

fn main() {
    println!("Hello, world!");
}
