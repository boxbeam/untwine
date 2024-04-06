#[cfg(test)]
mod tests {
    use insta::assert_snapshot;
    use untwine::parser;

    parser! {
        num: num=<'0'-'9'+> -> u32 { num.parse().unwrap() }
        pub num_list = num$","+ -> Vec<u32>;
    }

    fn operate(left: f64, op: char, right: f64) -> f64 {
        match op {
            '+' => left + right,
            '-' => left - right,
            '/' => left / right,
            '*' => left * right,
            _ => unreachable!(),
        }
    }

    parser! {
        sep = #{char::is_ascii_whitespace}*;
        num: num=<"-"? '0'-'9'+ ("." '0'-'9'+)?> -> f64 { num.parse().unwrap() }
        term = (num | "(" sep expr sep ")") -> f64;
        add: first=mul sep ops=(["+-"] sep mul)* -> f64 { ops.into_iter().fold(first, |left, (op, right)| operate(left, op, right)) }
        mul: first=term sep ops=(["*/"] sep term)* -> f64 { ops.into_iter().fold(first, |left, (op, right)| operate(left, op, right)) }
        pub expr = add -> f64;
    }

    #[test]
    pub fn test_num_list() {
        assert_eq!(untwine::parse(num_list, "1,2,3").unwrap(), vec![1, 2, 3]);
        assert_snapshot!(untwine::parse_pretty(num_list, "1,2,", false).unwrap_err());
    }

    #[test]
    pub fn test_expr() {
        assert_eq!(untwine::parse(expr, "1+2*3").unwrap(), 7.0);
        assert_eq!(untwine::parse(expr, "1+(3/4)").unwrap(), 1.75);
        assert_eq!(untwine::parse(expr, "4--1").unwrap(), 5.0);
        assert_snapshot!(untwine::parse_pretty(expr, "1.", false).unwrap_err());
        assert_snapshot!(untwine::parse_pretty(expr, "(1\n\n+", false).unwrap_err());
        assert_snapshot!(untwine::parse_pretty(expr, "(1\n\n+5", false).unwrap_err());
        assert_snapshot!(untwine::parse_pretty(expr, "--1", false).unwrap_err());
    }
}
