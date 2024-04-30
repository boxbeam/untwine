use std::ops::Range;

use crate::context::*;

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
    /// The separator between different error messages
    pub(crate) error_separator: &'static str,
    /// Whether to show the ^ at the position one past the error, where the insertion is probably expected
    show_caret: bool,
}

impl PrettyOptions {
    /// No colors
    pub fn no_color() -> Self {
        PrettyOptions {
            line_number_color: "",
            default_color: "",
            separator_color: "",
            error_indicator_color: "",
            start_pointer_color: "",
            error_separator: "\n\n==========\n\n",
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
            error_separator: "\n\n\x1b[90m==========\x1b[0m\n\n",
            show_caret: true,
        }
    }
}

/// Generate a pretty error with a specified message.
#[must_use]
pub fn pretty_error(
    input: &str,
    span: Range<usize>,
    error: String,
    colors: &PrettyOptions,
) -> String {
    let line = line(&input[..span.end]);
    let col = col(&input[..span.end]);
    let display = show_span(input, span.clone(), colors);
    let yellow = colors.start_pointer_color;
    let reset = colors.default_color;
    format!(
        "{display}\n{yellow}[{line}:{col}]{reset} {error}",
        line = line.max(1),
        col = col.max(1)
    )
}

/// Generate a display to point out a range in source code.
#[must_use]
pub fn show_span(input: &str, span: Range<usize>, options: &PrettyOptions) -> String {
    let lines: Vec<_> = lines(input).collect();

    let bold_white = options.line_number_color;
    let reset = options.default_color;
    let blue = options.separator_color;
    let red = options.error_indicator_color;
    let yellow = options.start_pointer_color;
    let overline_text = options.start_pointer_text;
    let caret = if options.show_caret { "^" } else { "" };

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
            "{line_pad}{line}\n{outer_pad}{spaces}{red}{underline}{caret}{reset}",
            line = &lines[end_line][end_range]
        )
    } else {
        let top_line_num = (start_line + 1).to_string();
        let bottom_line_num = (end_line + 1).to_string();
        let left_pad = bottom_line_num.len() + 1;
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
    let line = lines.get(line).copied().unwrap_or_default();
    if col < 40 {
        let end = line.len().min(80);
        (0..end, col)
    } else if line.len() - col < 40 {
        let start = line.len().saturating_sub(80);
        (start..line.len(), col - start)
    } else {
        (0..line.len(), col)
    }
}
