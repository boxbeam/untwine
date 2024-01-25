use untwine::parser;

struct MyContext {}

fn main() {
    parser! {
        [ctx: MyContext]
        digit: ("a" "b" "c")? | "b" -> i32 {
            a
        }
    }
    println!("Hello, world!");
}
