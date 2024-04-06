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

    /// Set the start of the error range if it hasn't already been set.
    pub fn set_start_if_empty(mut self, start: usize) -> Self {
        if self.pos.is_empty() {
            self.pos.start = start;
        }
        self
    }

    fn require_complete<C>(mut self, ctx: &ParserContext<C>) -> Self {
        if ctx.cursor() == ctx.input.len() {
            self
        } else {
            self.success = None;
            self
        }
    }

    /// Convert this into a [Result], and generate a pretty error if parsing failed. Parsing is also counted as failed
    /// if the entire input was not consumed, even if a success value is present.
    pub fn pretty_result<C>(
        mut self,
        ctx: &ParserContext<C>,
        options: PrettyOptions,
    ) -> Result<T, String>
    where
        E: Display,
    {
        self = self.require_complete(ctx);
        if let Some(success) = self.success {
            return Ok(success);
        }
        let error = match self.error {
            Some(e) => e.to_string(),
            None if ctx.cursor() == ctx.input.len() => "Unexpected end of input".to_string(),
            None => "Unexpected token".to_string(),
        };
        Err(pretty_error(ctx.input, self.pos, error, options))
    }

    /// Convert this into a [Result]. If the entire input was not consumed, the parser is treated as having failed,
    /// even if a success value is present.
    pub fn result<C>(mut self, ctx: &ParserContext<C>) -> Result<T, Option<E>> {
        self = self.require_complete(ctx);
        self.success.ok_or(self.error)
    }
}

/// Options to configure the output of pretty errors
pub struct PrettyOptions {
    /// The color to use on the indicators for the line number at the beginning
    line_number_color: &'static str,
    /// The color code used for everything that is not usually colored
    default_color: &'static str,
    /// The color used on the border between the line numbers and the displayed input
    separator_color: &'static str,
    /// The color of the underline pointing to the error
    error_indicator_color: &'static str,
    /// The color of the arrow indicating the beginning position on multiline errors
    start_pointer_color: &'static str,
    /// The text giving info about the beginning position on multiline errors, defaults to `beginning here`
    start_pointer_text: &'static str,
    /// Whether to show the ^ at the position one past the error, where the insertion is probably expected
    show_caret: bool,
}

impl PrettyOptions {
    /// No colors
    pub fn none() -> Self {
        PrettyOptions {
            line_number_color: "",
            default_color: "",
            separator_color: "",
            error_indicator_color: "",
            start_pointer_color: "",
            start_pointer_text: "beginning here",
            show_caret: true,
        }
    }
}

impl Default for PrettyOptions {
    fn default() -> Self {
        PrettyOptions {
            line_number_color: "\x1b[37;1m",
            default_color: "\x1b[0m",
            separator_color: "\x1b[34;1m",
            error_indicator_color: "\x1b[31m",
            start_pointer_color: "\x1b[33m",
            start_pointer_text: "beginning here",
            show_caret: true,
        }
    }
}

/// Generate a pretty error with a specified message.
pub fn pretty_error(
    input: &str,
    span: Range<usize>,
    error: String,
    colors: PrettyOptions,
) -> String {
    let line = line(&input[..span.end]);
    let col = col(&input[..span.end]);
    let display = show_span(input, span.clone(), colors);
    format!(
        "{display}\n[{line}:{col}] {error}",
        line = line.max(1),
        col = col.max(1)
    )
}

/// Generate a display to point out a range in source code.
pub fn show_span(input: &str, span: Range<usize>, options: PrettyOptions) -> String {
    let lines: Vec<_> = lines(input).collect();

    let bold_white = options.line_number_color;
    let reset = options.default_color;
    let blue = options.separator_color;
    let red = options.error_indicator_color;
    let yellow = options.start_pointer_color;
    let overline_text = options.start_pointer_text;
    let caret = options.show_caret.then_some("^").unwrap_or("");

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
        let line_pad = format!("{bold_white}{line_num} {blue}|{reset} ");

        let diff = (end_col - start_col).min(end_cursor - end_range.start);
        let spaces = " ".repeat(start_col);
        let underline = "-".repeat(diff);
        format!(
            "{line_pad}{line}\n{outer_pad}{spaces}{red}{underline}^{reset}",
            line = &lines[end_line][end_range]
        )
    } else {
        let top_line_num = (start_line + 1).to_string();
        let bottom_line_num = (end_line + 1).to_string();
        let left_pad = top_line_num.len().max(bottom_line_num.len()) + 1;
        let top_line_pad = format!(
            "{bold_white}{top_line_num}{pad}{blue}|{reset} ",
            pad = " ".repeat(left_pad - top_line_num.len())
        );
        let middle_line_pad = format!("{pad}{blue}|{reset} ", pad = " ".repeat(left_pad));
        let bottom_line_pad = format!(
            "{bold_white}{bottom_line_num}{pad}{blue}|{reset} ",
            pad = " ".repeat(left_pad - bottom_line_num.len())
        );
        let outer_pad = " ".repeat(left_pad + 2);

        let top_line = &lines[start_line][start_range];
        let top_line = format!(
            "{yellow}{outer_pad}{spaces}| {overline_text}\n{outer_pad}{spaces}v\n{reset}{top_line_pad}{top_line}",
            spaces = " ".repeat(start_cursor),
        );

        let middle_line = (end_line - start_line > 1)
            .then(|| format!("{middle_line_pad}{blue}...{reset}\n"))
            .unwrap_or_default();

        let bottom_line = format!(
            "{bottom_line_pad}{line}\n{outer_pad}{red}{underline}{caret}{reset}",
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
