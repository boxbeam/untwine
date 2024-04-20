use std::ops::RangeInclusive;

use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use recoverable::{recover_enum, recover_struct};
use syn::{
    braced, bracketed, parenthesized,
    parse::{discouraged::Speculative, Parse, ParseStream},
    parse_macro_input,
    token::{Brace, Bracket, Paren},
    Block, DeriveInput, Expr, Ident, LitBool, LitChar, LitStr, Path, Result, Token, Type,
    Visibility,
};

mod codegen;
mod display;
mod recoverable;

mod kw {
    use syn::custom_keyword;

    custom_keyword!(i);
    custom_keyword!(error);
    custom_keyword!(context);
    custom_keyword!(data);
    custom_keyword!(lookahead_optimization);
    custom_keyword!(recover);
}

#[derive(Debug)]
pub(crate) struct Header {
    ctx_name: Ident,
    error_type: Type,
    data_type: Type,
    lookahead_optimization: bool,
    recover: bool,
}

#[derive(Debug)]
pub(crate) enum HeaderParam {
    ContextName(Ident),
    ErrorType(Type),
    DataType(Type),
    LookaheadOptimization(LitBool),
    Recover(LitBool),
}

impl Parse for HeaderParam {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.parse::<kw::error>().is_ok() {
            input.parse::<Token![=]>()?;
            Ok(HeaderParam::ErrorType(input.parse()?))
        } else if input.parse::<kw::context>().is_ok() {
            input.parse::<Token![=]>()?;
            Ok(HeaderParam::ContextName(input.parse()?))
        } else if input.parse::<kw::data>().is_ok() {
            input.parse::<Token![=]>()?;
            Ok(HeaderParam::DataType(input.parse()?))
        } else if input.parse::<kw::lookahead_optimization>().is_ok() {
            input.parse::<Token![=]>()?;
            Ok(HeaderParam::LookaheadOptimization(input.parse()?))
        } else if input.parse::<kw::recover>().is_ok() {
            input.parse::<Token![=]>()?;
            Ok(HeaderParam::Recover(input.parse()?))
        } else {
            Err(input.error("expected parameter name 'error' or 'context'"))
        }
    }
}

impl Parse for Header {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut ctx_name = Ident::new("__ctx", Span::call_site());
        let mut error_type = syn::parse(quote! {untwine::ParserError}.into())?;
        let mut data_type = syn::parse(quote! {()}.into())?;
        let mut lookahead_optimization = true;
        let mut recover = false;

        if input.peek(Bracket) {
            let content;
            bracketed!(content in input);
            while !content.is_empty() {
                let param: HeaderParam = content.parse()?;
                match param {
                    HeaderParam::ContextName(name) => ctx_name = name,
                    HeaderParam::ErrorType(typ) => error_type = typ,
                    HeaderParam::DataType(typ) => data_type = typ,
                    HeaderParam::LookaheadOptimization(enable) => {
                        lookahead_optimization = enable.value();
                    }
                    HeaderParam::Recover(enable) => recover = enable.value(),
                }
                if !content.is_empty() {
                    content.parse::<Token![,]>()?;
                }
            }
        }
        Ok(Header {
            ctx_name,
            error_type,
            data_type,
            lookahead_optimization,
            recover,
        })
    }
}

#[derive(Debug)]
pub(crate) struct ParserBlock {
    header: Header,
    parsers: Vec<ParserDef>,
}

impl Parse for ParserBlock {
    fn parse(input: ParseStream) -> Result<Self> {
        let block = ParserBlock {
            header: input.parse()?,
            parsers: list(input, false, |i| i.is_empty())?,
        };
        Ok(block)
    }
}

#[derive(Debug)]
pub(crate) struct ParserDef {
    vis: Visibility,
    name: Ident,
    patterns: TopLevelPatterns,
    return_type: Type,
    block: Block,
}

impl Parse for ParserDef {
    fn parse(input: ParseStream) -> Result<Self> {
        let vis = input.parse()?;
        let name: Ident = input.parse()?;
        let colon = input
            .parse::<Token![:]>()
            .map(|_| true)
            .or_else(|_| input.parse::<Token![=]>().map(|_| false))?;
        let mut patterns: TopLevelPatterns = input.parse()?;
        let return_type: Type = if input.parse::<Token![->]>().is_ok() {
            input.parse()?
        } else {
            syn::parse(quote! {()}.into())?
        };

        let block = if colon {
            input.parse()?
        } else {
            input.parse::<Token![;]>()?;
            if let Some(label) = patterns.patterns.iter().flat_map(|p| p.label.iter()).next() {
                return Err(syn::Error::new(
                    label.span(),
                    "Cannot use explicit labels here (implicitly captured by = at beginning)",
                ));
            }

            let pattern = Pattern {
                fragment: PatternFragment::Nested(PatternList::List(
                    patterns.patterns.into_iter().map(|p| p.pattern).collect(),
                )),
                modifier: None,
            };

            patterns = TopLevelPatterns {
                patterns: vec![LabeledPattern {
                    label: Some(name.clone()),
                    pattern,
                }],
            };

            syn::parse(quote! {{#name}}.into())?
        };
        Ok(ParserDef {
            vis,
            name,
            patterns,
            return_type,
            block,
        })
    }
}

#[derive(Debug, Clone)]
pub(crate) struct TopLevelPatterns {
    patterns: Vec<LabeledPattern>,
}

impl Parse for TopLevelPatterns {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut patterns = vec![];
        while !input.peek(Token![->]) && !input.peek(Token![;]) {
            patterns.push(input.parse()?);
        }
        Ok(TopLevelPatterns { patterns })
    }
}

#[derive(Debug, Clone)]
pub(crate) enum PatternFragment {
    Literal(LitStr),
    CharRange(CharRange),
    CharGroup(CharGroup),
    CharFilter(CharFilter),
    ParserRef(Ident),
    Ignore(Box<IgnoredPattern>),
    Span(PatternList),
    Nested(PatternList),
    Annotated(PatternAttribute),
    AnyChar,
}

impl Parse for PatternFragment {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek(LitChar) || input.peek(Token![^]) {
            input.parse().map(PatternFragment::CharRange)
        } else if input.peek(Bracket) {
            input.parse().map(PatternFragment::CharGroup)
        } else if input.peek(Brace) {
            input.parse().map(PatternFragment::CharFilter)
        } else if input.peek(LitStr) || input.peek(kw::i) {
            input.parse().map(PatternFragment::Literal)
        } else if input.peek(Paren) {
            let content;
            parenthesized!(content in input);
            content.parse().map(PatternFragment::Nested)
        } else if input.peek(Token![.]) {
            input.parse::<Token![.]>()?;
            Ok(PatternFragment::AnyChar)
        } else if input.peek(Token![#]) && input.peek2(Bracket) {
            let fork = input.fork();
            fork.parse::<Token![#]>()?;
            let content;
            bracketed!(content in fork);

            if content.peek(Ident) {
                Ok(PatternFragment::Annotated(input.parse()?))
            } else {
                Ok(PatternFragment::Ignore(Box::new(input.parse()?)))
            }
        } else if input.peek(Token![#]) {
            let ignored: IgnoredPattern = input.parse()?;
            Ok(PatternFragment::Ignore(Box::new(ignored)))
        } else if input.peek(Token![<]) {
            input.parse::<Token![<]>()?;
            let patterns = input.parse()?;
            input.parse::<Token![>]>()?;
            Ok(PatternFragment::Span(patterns))
        } else if input.peek(Ident) {
            input.parse().map(PatternFragment::ParserRef)
        } else {
            Err(input.error("expected pattern fragment"))
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Pattern {
    fragment: PatternFragment,
    modifier: Option<Modifier>,
}

impl Parse for Pattern {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(Pattern {
            fragment: input.parse()?,
            modifier: optional(input),
        })
    }
}

#[derive(Debug, Clone)]
pub(crate) struct PatternAttribute {
    name: Path,
    args: Vec<Expr>,
    pattern: Box<Pattern>,
}

impl Parse for PatternAttribute {
    fn parse(input: ParseStream) -> Result<Self> {
        input.parse::<Token![#]>()?;
        let brackets;
        bracketed!(brackets in input);
        let name = brackets.parse()?;
        let mut args = vec![];
        if brackets.peek(Paren) {
            let parens;
            parenthesized!(parens in brackets);
            while !parens.is_empty() {
                args.push(parens.parse()?);
                if !parens.is_empty() {
                    parens.parse::<Token![,]>()?;
                }
            }
        }

        let pattern = Box::new(input.parse()?);

        Ok(PatternAttribute {
            name,
            args,
            pattern,
        })
    }
}

#[derive(Debug, Clone)]
pub(crate) struct CharRange {
    inverted: bool,
    range: RangeInclusive<char>,
}

impl Parse for CharRange {
    fn parse(input: ParseStream) -> Result<Self> {
        let inverted: Option<Token![^]> = input.parse()?;
        let begin: LitChar = input.parse()?;
        input.parse::<Token![-]>()?;
        let end: LitChar = input.parse()?;
        Ok(CharRange {
            inverted: inverted.is_some(),
            range: begin.value()..=end.value(),
        })
    }
}

#[derive(Debug, Clone)]
pub(crate) struct CharGroup {
    inverted: bool,
    chars: Vec<char>,
}

impl Parse for CharGroup {
    fn parse(input: ParseStream) -> Result<Self> {
        let content;
        bracketed!(content in input);
        let inverted: Option<Token![^]> = content.parse()?;
        let str: LitStr = content.parse()?;
        if str.value().is_empty() {
            return Err(syn::Error::new_spanned(
                str,
                "Character groups must have at least one character",
            ));
        }
        Ok(CharGroup {
            inverted: inverted.is_some(),
            chars: str.value().chars().collect(),
        })
    }
}

#[derive(Debug, Clone)]
pub(crate) struct CharFilter {
    expr: Expr,
}

impl Parse for CharFilter {
    fn parse(input: ParseStream) -> Result<Self> {
        let content;
        braced!(content in input);
        Ok(CharFilter {
            expr: content.parse()?,
        })
    }
}

#[derive(Debug, Clone)]
pub(crate) struct IgnoredPattern {
    pattern: Pattern,
}

impl Parse for IgnoredPattern {
    fn parse(input: ParseStream) -> Result<Self> {
        input.parse::<Token![#]>()?;
        let pattern = input.parse()?;
        Ok(IgnoredPattern { pattern })
    }
}

#[derive(Debug, Clone)]
pub(crate) struct LabeledPattern {
    label: Option<Ident>,
    pattern: Pattern,
}

impl Parse for LabeledPattern {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut label = None;
        if input.peek2(Token![=]) {
            label = Some(input.parse()?);
            input.parse::<Token![=]>()?;
        }
        let pattern: Pattern = input.parse()?;
        Ok(LabeledPattern { label, pattern })
    }
}

#[derive(Debug, Clone)]
pub(crate) enum PatternList {
    List(Vec<Pattern>),
    Choices(Vec<Vec<Pattern>>),
}

fn list_terminator(input: ParseStream) -> bool {
    input.peek(Token![|]) || input.peek(Token![->]) || input.peek(Token![>])
}

impl Parse for PatternList {
    fn parse(input: ParseStream) -> Result<Self> {
        let patterns: Vec<Pattern> = list(input, true, list_terminator)?;
        if !input.peek(Token![|]) {
            return Ok(PatternList::List(patterns));
        }
        let mut choices: Vec<Vec<Pattern>> = vec![patterns];
        while input.peek(Token![|]) {
            input.parse::<Token![|]>()?;
            choices.push(list(input, true, list_terminator)?);
        }
        Ok(PatternList::Choices(choices))
    }
}

#[derive(Debug, Clone)]
pub(crate) enum Modifier {
    Optional,
    Repeating,
    OptionalRepeating,
    Delimited(PatternFragment),
    OptionalDelimited(PatternFragment),
}

impl Modifier {
    fn is_repeating(&self) -> bool {
        match self {
            Modifier::Optional => false,
            Modifier::Repeating => true,
            Modifier::OptionalRepeating => true,
            Modifier::Delimited(_) => true,
            Modifier::OptionalDelimited(_) => true,
        }
    }
}

impl Parse for Modifier {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.parse::<Option<Token![+]>>()?.is_some() {
            Ok(Modifier::Repeating)
        } else if input.parse::<Option<Token![?]>>()?.is_some() {
            Ok(Modifier::Optional)
        } else if input.parse::<Option<Token![*]>>()?.is_some() {
            Ok(Modifier::OptionalRepeating)
        } else if input.parse::<Option<Token![$]>>()?.is_some() {
            let fragment = input.parse()?;
            if input.peek(Token![+]) {
                input.parse::<Token![+]>()?;
                Ok(Modifier::Delimited(fragment))
            } else {
                input.parse::<Token![*]>()?;
                Ok(Modifier::OptionalDelimited(fragment))
            }
        } else {
            Err(input.error("expected modifier"))
        }
    }
}

fn list<T: Parse>(
    input: ParseStream,
    require: bool,
    terminator: fn(ParseStream) -> bool,
) -> Result<Vec<T>> {
    let mut vec = vec![];
    while (require && vec.is_empty()) || (!input.is_empty() && !terminator(input)) {
        vec.push(input.parse()?);
    }
    Ok(vec)
}

fn optional<T: Parse>(input: ParseStream) -> Option<T> {
    let fork = input.fork();
    match fork.parse() {
        Ok(val) => {
            input.advance_to(&fork);
            Some(val)
        }
        Err(_) => None,
    }
}

/// Generate parsers using a series of declarative patterns and blocks evaluating their values.
///
/// Parsers, or "rules", are structured similarly to regular rust functions, and will be converted into
/// them by this macro. Each rule consists of an optional visiblity modifier, a name, a series of patterns,
/// and a block to evaluate the output of the parser.
///
/// ## Example of a simple parser
/// ```ignore
/// parser! {
///    num: digits=<'0'-'9'+> -> u32 { digits.parse().unwrap() }
///    pub num_list: nums=num$","+ -> Vec<u32> { nums }
/// }
/// ```
/// Here, `num` is defined as a parser which parses the pattern `<'0'-'9'+>`, whose output is captured to the
/// variable `digits`, and it evaluates to a `u32` (the result is implicit, and the error type can be configured).
/// It generates its output by calling `.parse().unwrap()` on `digits`, which is captured as a [&str].
///
/// ## Summary of pattern syntax
/// - `"literal"` - Parses a string literal, and evaluates to `()` (since string literals are assumed to be structural and not desired in output).
/// - `'a'-'z'` - Parses characters within a range, equivalent to `'a'..='z'` in regular Rust. Evaluates to [char].
/// - `^'a'-'z'` - Similar to the above, but parses a character not contained in the range.
/// - `["abcd"]` - Match any character contained within the string literal. Evaluates to [char].
/// - `[^"abcd"]` - Similar to the above, but parses a character not contained in the group.
/// - `.` - Match any single character. Evaluates to [char].
/// - `{char::is_whitespace}` - Matches any character passing the predicate inside `{}`. Any function with the signature `fn(&char) -> bool` can be used, including a custom lambda.
/// - `<inner pattern>` - Captures the span of the parsed value as a [&str], rather than the value itself.
/// - `ident` - Uses a parser defined in this parser block by name.
/// - `var=pattern` - Captures the output of a parser to a variable which can be used in the parser's function body.
/// - `(pattern_a | pattern_b)` - Can chain as many patterns as desired together. The resulting parser will try parsing using each of them, in the order specified. Each pattern used must have the same type.
/// - `pattern_a pattern_b` - Parse two patterns in sequence. The output type will be `(A, B)` for the corresponding types `A` and `B` for the patterns. Units will not be included in this output.
/// - Modifiers
///   - `pattern+` - Modifies a pattern to make it repeat as many times as possible, at least once. Returns a [Vec] of the modified pattern's type.
///   - `pattern*` - Similar to the above, but allows zero matches.
///   - `pattern?` - Parses the pattern optionally. Returns an [Option] of the modified pattern's type.
///   - `pattern$delimiter+` - Parses a list of `pattern` delimited by `delimiter`, requiring at least one value. Returns a [Vec] of `pattern`'s output type. The delimiter's output is ignored.
///   - `pattern$delimiter*` - Similar to the above, but allows zero matches.
///   - `#pattern` - Ignores the output of the modified pattern, making it return `()`.
///   - `#[dbg] pattern` - Print the debug output of the pattern after parsing.
///     - The above syntax is not a special case, `untwine::attr::dbg` is its implementation. Any similar function can be used as an attribute.
///     - Additional parameters can be passed like `#[myattr(1, "hello")]`.
///
/// ## Using a parser
/// Every parser specified is rewritten into a regular Rust function, which will parse all of the patterns specified and evaluate your block at the end.
/// Those which have a visibility modifier like `pub` or `pub(crate)` will be accessible outside of the parser block.
///
/// To use one of these parsers, you can either construct a `ParserContext` and call it as normal, or use `untwine::parse` / `untwine::parse_pretty` to get
/// a regular [Result] instead of untwine's `ParserResult` type.

/// ## Special syntax for parsers
/// There is an alternate syntax for parsers whose output needs no modification.
/// The example parser `num_list` above could also be written like this:
/// ```ignore
/// parser! {
///    num: digits=<'0'-'9'+> -> u32 { digits.parse().unwrap() }
///    pub num_list = nums=num$","+ -> Vec<u32>;
/// }
/// ```
/// If the parser simply returns the output of all of its patterns unchanged, it may use an `=` instead of `:` and
/// use a semicolon instead of a function body. If the output type is `()`, the `-> ReturnType` can also be omitted,
/// allowing structural rules like whitespace to be defined simply, like `sep = {char::is_whitespace}*;`.
///
/// ## Configuring a parser block
/// There are some values that can be adjusted to change the behavior of a parser.
/// These arguments are specified inside `[]` prior to defining any parsers:
/// ```ignore
/// parser! {
///     [error = MyErrorType]
///     ...
/// }
/// ```
/// Here are the available options:
/// - `error = MyErrorType` - Use `MyErrorType` instead of `ParserError` as the error type for all parsers. `MyErrorType` must implement `From<ParserError>`.
/// - `context = ctx_name` - Expose the `ParserContext` to the parser function bodies using the name specified.
/// - `data_type = MyCustomContext` - Specify a custom context type which will be passed to all parser functions. It can be accessed using `.data()` or `.data_mut()` on the context argument.
/// - `lookahead_optimization = false` - Disable the lookahead optimization, which is on by default. This optimization can improve parser performance by as much as double, but also inflates the size of the generated code, which may cause slower clean builds.
/// - `recovery = true` - Enable automatic error recovery, which allows the parser to continue parsing after an operation fails, following specific rules. Read more below.
///
/// ## Error recovery
/// Automatic error recovery is one of the most powerful features of Untwine, but it is relatively basic and follows a few simple rules.
/// Being aware of these rules can help you improve the quality of error messages you get, and you can also specify certain recovery rules
/// manually using attributes.
///
/// In order for error recovery to work, you will need to implement the `Recoverable` trait on types which can be recovered from.
/// This is like [Default], but can take a range where the error occurred, and is used to generate a value which should be returned
/// in case of an error which was recovered from.
///
/// You can use `#[derive(Recoverable)]` to automatically derive the `Recoverable` trait on your types. If using a struct, all fields
/// must implement `Recoverable`. If using an enum, the variant to return must have the `#[recover]` attribute, and all constituent
/// fields must implement `Recoverable`. Note that `Range<usize>` implements this trait, and will return itself, meaning it will auto-populate
/// to the span of the error that was recovered from.
///
/// Here are the rules for error recovery:
/// - If `recover = true` is set:
///   - If a named parser begins and ends with unconditional string literals, recovery will happen automatically
///     - Recovery is attempted by trying to jump ahead to the closing delimiter
///     - This jump is limited to 1000 non-whitespace characters to prevent excessive lookaheads
///     - The balance of delimiters will be respected, so `[[]` will not consider the first closing delimiter a match because it is still unbalanced
///   - If a parser is delimited, when any element fails to parse, attempt to recover to the next delimiter
///     - This jump respects parent delimiters, so the lookahead will never look past a closing delimiter which ends the scope
///     - This jump is limited to 150 non-whitespace characters to prevent excessive lookaheads
///   - If a parser is annotated with `#[recover_to("literal")]`, the annotated parser will recover using the same strategy as above
///   - There is also a `#[recover_to_any(["a", "b"])]` attribute which allows recovering to one of multiple literals
#[proc_macro]
pub fn parser(input: TokenStream) -> TokenStream {
    let block: ParserBlock = parse_macro_input!(input as ParserBlock);

    match codegen::generate_parser_block(block) {
        Ok(stream) => stream.into(),
        Err(err) => err.to_compile_error().into(),
    }
}

#[proc_macro_derive(Recoverable, attributes(recover))]
pub fn recovery(input: TokenStream) -> TokenStream {
    let derive_input = parse_macro_input!(input as DeriveInput);
    match derive_input.data {
        syn::Data::Struct(data) => {
            recover_struct(&data, &derive_input.ident, &derive_input.generics)
        }
        syn::Data::Enum(data) => recover_enum(&data, &derive_input.ident, &derive_input.generics)
            .unwrap_or_else(|e| e.to_compile_error()),
        syn::Data::Union(_) => panic!("Cannot derive Recoverable on unions"),
    }
    .into()
}
