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

    word = #'a'-'z'+;

    block = word ":" ("\n" (indent line)$"\n"+)?;

    indented_block: -> () {
        *ctx.data_mut() += 1;
        let ret = block(ctx);
        *ctx.data_mut() -= 1;
        return ctx.result_from(ret);
    }

    line = (indented_block | word);
    pub lines = line$"\n"+;
}

const INPUT: &'static str = "
a:
 b:
  c
b:
 c";

fn main() {
    untwine::parse(lines, INPUT.trim()).unwrap();
}
