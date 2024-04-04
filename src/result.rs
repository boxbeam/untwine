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

    /// Generate a pretty error message visually pointing out the location of the error.
    pub fn pretty<C>(self, ctx: &ParserContext<C>) -> ParserResult<T, String>
    where
        E: Display,
    {
        let error = match self.error {
            Some(e) => format!("{e}"),
            None => "Unexpected token or end of input".to_string(),
        };
        let line = line(&ctx.input[..self.pos.end]);
        let col = col(&ctx.input[..self.pos.end]);
        let error_line = ctx
            .input
            .lines()
            .nth(line.checked_sub(1).unwrap_or(0))
            .unwrap_or_default();

        let min = col.checked_sub(40).unwrap_or(0);
        let max = error_line.len().min(col + 40);
        let selection_len = self.pos.len().min(col - min);

        let error_part = &error_line[min as usize..max as usize];
        let padding = " ".repeat(col.checked_sub(min + selection_len).unwrap_or(0))
            + &"-".repeat(selection_len);
        let err = format!(
            "{error_part}\n{padding}^\n[{line}:{col}]: {error}",
            line = line.max(1),
            col = col.max(1)
        );
        ParserResult::new(self.success, Some(err), self.pos)
    }

    /// Convert this into a [Result]. If the entire input was not consumed, the parser is treated as having failed,
    /// even if a success value is present. The default error will be used if one is not already present.
    pub fn result<C>(self, ctx: &ParserContext<C>) -> Result<T, E>
    where
        E: Default,
    {
        if ctx.cursor() == ctx.input.len() {
            if let Some(success) = self.success {
                return Ok(success);
            }
        }
        return Err(self.error.unwrap_or_default());
    }
}
