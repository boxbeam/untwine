use untwine::parser;

struct MyContext {}

fn main() {
    parser! {
        [ctx: MyContext]
        digit: ("a" "b" "c")? -> i32 {}
    }
    println!("Hello, world!");
}
