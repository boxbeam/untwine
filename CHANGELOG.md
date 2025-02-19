## 0.8.3
- Fix improper error reporting with lookahead optimization
  - Could sometimes cause panics due to the start position of an error being ahead of the end position, causing an overflow in display code
- Disable elided_named_lifetimes warning inside parser blocks

## 0.8.2

- Make all non-captured parsers use `.ignore()` to drop their outputs
  - This makes it possible to do things such as `#(a | b)` where `a` and `b` have different types. Since the overall output is ignored, both their outputs will be ignored and the parsers do not have to have the same output type.
- Added character literals like `'"'`

## 0.8.1

- Fix issue with incomplete parses sometimes not generating any errors
- Write tutorial

## 0.8.0

- Fixed some strange error messages
- Made AppendCell crate-private

## 0.7.0 

- Overhaul of error handling internals
- ~20% improvement in parser performance

## 0.6.1

- Improve errors for missing delimiters
- Add some missing documentation

## 0.6.0

- Implement automatic error recovery
  - Enabled with `recover = true` in your parser's configuration block
  - Automatically recovers to the delimiters of delimited lists and of wrapped named rules
    - Wrapped named rules like `parens: "(" parens* ")" -> MyType { ... }`
- Remove all runtime dependencies

## 0.5.0

- Implement lookahead optimization
  - Automatically generates a match statement at compiletime for choice patterns
  - Looks ahead by one character only
  - ~25% improvement in parser performance

## 0.4.0

- Add tests
- Improve docs

## 0.3.0

- Add parser_repl function for easy testing of a simple parser
  - Only intended for debugging, very minimal REPL without scrolling

## 0.2.0

- Prettier, more informative errors
- Error ranges, which point out an error's origin / context as well
- Implementation of proof of concept parsers
- Initial working release
