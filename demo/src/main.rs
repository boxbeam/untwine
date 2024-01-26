use untwine::parser;

struct MyContext {}

fn main() {
    parser! {
        int: negative="-"? digits=['0'-'9']+ -> i32 {
            let number = digits.parse_to_int();
            if negative {
                -number
            } else {
                number
            }
        }
        int_list: first=int rest=("," int)* -> Vec<i32> {
            first.into_iter().chain(rest).collect()
        }
    }
    println!("Hello, world!");
}
