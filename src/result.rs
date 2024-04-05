use std::{fmt::Display, ops::Range};

use crate::context::*;

#[derive(Debug)]
/// The output of a [crate::parser::Parser]
pub struct ParserResult<T, E> {
    /// The successfully-parsed value.
    pub success: Option<T>,
    /// The deepest error encountered, even if parsing succeeded.
    pub error: Option<E>,
    /// The deepest position encountered when parsing, including failed branches.
    pub pos: Range<usize>,
}

impl<T, E> ParserResult<T, E> {
    /// Map the output type using a mapping function.
    pub fn map<V>(self, f: impl FnOnce(T) -> V) -> ParserResult<V, E> {
        ParserResult {
            success: self.success.map(f),
            error: self.error,
            pos: self.pos,
        }
    }

    /// Map the error type using a mapping function.
    pub fn map_err<E2>(self, f: impl FnOnce(E) -> E2) -> ParserResult<T, E2> {
        ParserResult {
            error: self.error.map(f),
            success: self.success,
            pos: self.pos,
        }
    }

    pub fn new(success: Option<T>, error: Option<E>, pos: Range<usize>) -> Self {
        ParserResult {
            success,
            error,
            pos,
        }
    }

    /// Integrate the error of another [ParserResult], if its position is higher.
    pub fn integrate_error<V>(mut self, other: ParserResult<V, E>) -> Self {
        if other.pos.end > self.pos.end {
            self.error = other.error;
            self.pos = other.pos;
        }
        self
    }

    /// Generate a [ParserResult] holding a success value and no error.
    pub fn success(result: T, pos: usize) -> Self {
        ParserResult {
            success: Some(result),
            error: None,
            pos: pos..pos,
        }
    }

    pub fn set_start_if_empty(mut self, start: usize) -> Self {
        if self.pos.is_empty() {
            self.pos.start = start;
        }
        self
    }

    fn require_complete<C>(mut self, ctx: &ParserContext<C>) -> Self {
        if ctx.cursor() == self.pos.end {
            self
        } else {
            self.success = None;
            self
        }
    }

    pub fn pretty_result<C>(mut self, ctx: &ParserContext<C>) -> Result<T, String>
    where
        E: Display,
    {
        self = self.require_complete(ctx);
        if let Some(success) = self.success {
            return Ok(success);
        }
        let error = match self.error {
            Some(e) => e.to_string(),
            None => "Unexpected token or end of input".to_string(),
        };
        Err(pretty_error(ctx.input, self.pos, error))
    }

    /// Convert this into a [Result]. If the entire input was not consumed, the parser is treated as having failed,
    /// even if a success value is present.
    pub fn result<C>(mut self, ctx: &ParserContext<C>) -> Result<T, Option<E>> {
        self = self.require_complete(ctx);
        self.success.ok_or(self.error)
    }
}

pub fn pretty_error(input: &str, span: Range<usize>, error: String) -> String {
    let line = line(&input[..span.end]);
    let col = col(&input[..span.end]);
    let display = show_span(input, span.clone());
    format!(
        "{display}\n[{line}:{col}] {error}",
        line = line.max(1),
        col = col.max(1)
    )
}

pub fn show_span(input: &str, span: Range<usize>) -> String {
    let lines: Vec<_> = lines(input).collect();

    let (start_line, start_col) = (
        line(&input[..span.start]).saturating_sub(1),
        col(&input[..span.start]),
    );
    let (start_range, start_cursor) = get_error_line(&lines, start_line, start_col);

    let (end_line, end_col) = (
        line(&input[..span.end]).saturating_sub(1),
        col(&input[..span.end]),
    );
    let (end_range, end_cursor) = get_error_line(&lines, end_line, end_col);

    if start_line == end_line {
        let line_num = (end_line + 1).to_string();
        let outer_pad = " ".repeat(line_num.len() + 3);
        let line_pad = format!("{line_num} \x1b[34;1m|\x1b[0m ");

        let diff = (end_col - start_col).min(end_cursor - end_range.start);
        let spaces = " ".repeat(start_col);
        let underline = "-".repeat(diff);
        format!(
            "{line_pad}{line}\n{outer_pad}{spaces}\x1b[31m{underline}^\x1b[0m",
            line = &lines[end_line][end_range]
        )
    } else {
        let top_line_num = (start_line + 1).to_string();
        let bottom_line_num = (end_line + 1).to_string();
        let left_pad = top_line_num.len().max(bottom_line_num.len()) + 1;
        let top_line_pad = format!(
            "{top_line_num}{pad}\x1b[34;1m|\x1b[0m ",
            pad = " ".repeat(left_pad - top_line_num.len())
        );
        let middle_line_pad = format!("{pad}\x1b[34;1m|\x1b[0m ", pad = " ".repeat(left_pad));
        let bottom_line_pad = format!(
            "{bottom_line_num}{pad}\x1b[34;1m|\x1b[0m ",
            pad = " ".repeat(left_pad - bottom_line_num.len())
        );
        let outer_pad = " ".repeat(left_pad + 2);

        let top_line = &lines[start_line][start_range];
        let top_line = format!(
            "\x1b[33m{outer_pad}{spaces}| beginning here\n{outer_pad}{spaces}v\n\x1b[0m{top_line_pad}{top_line}",
            spaces = " ".repeat(start_cursor),
        );

        let middle_line = (end_line - start_line > 1)
            .then_some(middle_line_pad + "\x1b[34m...\x1b[0m\n")
            .unwrap_or_default();

        let bottom_line = format!(
            "{bottom_line_pad}{line}\n{outer_pad}\x1b[31m{underline}^\x1b[0m",
            line = &lines[end_line][end_range.clone()],
            underline = "-".repeat(end_col - end_range.start)
        );
        format!("{top_line}\n{middle_line}{bottom_line}")
    }
}

fn get_error_line<'a>(lines: &'a [&'a str], line: usize, col: usize) -> (Range<usize>, usize) {
    let line = lines.get(line).map(|s| *s).unwrap_or_default();
    if col < 40 {
        let end = line.len().min(80);
        (0..end, col)
    } else if line.len() - col < 40 {
        let start = (line.len() - 80).max(0);
        (start..line.len(), col - start)
    } else {
        (0..line.len(), col)
    }
}
