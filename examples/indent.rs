use untwine::prelude::*;

parser! {
    [data = u32, context = ctx]

    indent: -> () {
        let indent_level = *ctx.data();
        for _ in 0..indent_level {
            if !ctx.slice().starts_with(" ") {
                return Err(ParserError::ExpectedToken("indent"));
            }
            ctx.advance(1);
        }
    }

    increase_indent: -> () {
        *ctx.data_mut() += 1;
    }

    decrease_indent: -> () {
        *ctx.data_mut() -= 1;
    }

    word = #'a'-'z'+;
    block = word ":" ("\n" increase_indent (indent line)$"\n"+ decrease_indent)?;
    line = (block | word);
    pub lines = line$"\n"+;
}

const INPUT: &'static str = "
a:
 b:
  c
b";

fn main() {
    untwine::parse(lines, INPUT.trim()).unwrap();
}
