use untwine::parser;

struct MyContext {}

fn main() {
    parser! {
        [ctx: MyContext]
        digit: (a | b) -> i32 {}
    }
    println!("Hello, world!");
}
