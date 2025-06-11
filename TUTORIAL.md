## Welcome to Untwine

This guide will serve as a ground-up introduction to Untwine, your ultimate toolkit for parsing languages.

Before beginning to use Untwine, it's a good idea to ask: Is this the right tool for me? Untwine is not intended for:

- Parsing byte formats / non-UTF8 
- Extremely performance critical parsing
  - Untwine is fast, but a handwritten parser will usually be about twice as fast
- Deserialization

Untwine is ideal for:

- Programming languages
- Domain-specific languages
- User-facing parsers which need good errors
- Fun little toys

Untwine comes with a set of parser-combinators, which you will ideally never need to touch. All of your parser code should generally be using the custom syntax within the `parser!` macro.

The `parser!` macro has a rundown of all of its capabilities in its [docs page](https://docs.rs/untwine/latest/untwine/macro.parser.html), which is recommended reading for anyone looking to make a serious parser.

For everyone who's just getting started, or is just curious about how this works, this guide will show you the basics.

## Writing your first parser

For this example, we're going to be going through building a simple boolean expression parser, similar to the [expr example](https://github.com/boxbeam/untwine/blob/master/examples/expr.rs), but with a step-by-step explanation which doesn't need to care about operator precedence.

To begin, let's just write an empty parser block:

```rust
parser! {
  
}
```

On its own, this block does nothing, other than define an empty module starting with `__parser`. This module will be the namespace for all of the parser functions you write in here.

Before writing any actual parsers, we have the option to place `[]` at the top of the parser block to specify any settings. In this case, we don't need to set any special settings, but you can find more information on these settings and what they do at https://docs.rs/untwine/latest/untwine/macro.parser.html#configuring-a-parser-block.

### Anatomy of a parser

To start writing an expression parser, we should first understand the anatomy of our parsers. The simplest thing you could write which would be a boolean expression would probably just be a boolean, so let's write a parser for that.

```rust
parser! {
  boolean:
    value=<"true" | "false">
  -> bool {
      value == "true"
  }
}
```

There's a lot going on here, so let's break it down. You probably notice that this looks like a function, and you'd be exactly right. In fact, this rule gets rewritten to a function called `boolean`, which will look something like this:

```rust
fn boolean(ctx: &ParserContext) -> Option<bool> {
  let value = literal("true")
    .or(literal("false"))
    .span()
    .parse(ctx)?;

  Some(
    {
      value == "true"
    }
  )
}
```

This should help pull away the curtain, and while it's not exactly what the generated code will look like, it should give you a good idea of how it works. Each "rule" is converted into a single function, which will create parsers using combinators corresponding to the patterns you described.

It will then call these parsers with the `.parse` function, and your code body will be placed in the return position wrapped by `Some`. *Note:* Your block actually evaluates to a `Result`, which allows you call fallible functions from your code block and use the `?` operator on them.

When this function is run, `ctx` will contain a parsing input, and when we call `.parse()` with the parser assembled via combinators, it will consume the beginning of the input if it begins with `true` or `false`, and it will emit an error (through the context object) if neither could be matched.

For example, if we begin with `true123`, this function will parse `true` and leave `123` in the parse buffer. If the input is `tru123`, it will parse nothing, leaving the buffer unchanged, and emitting an error.

With this, we can begin to digest the syntax we started with, as each piece corresponds cleanly to part of the output:

- `boolean` is the name of the parser function we defined
- `"true" | "false"` gets converted into `literal("true").or(literal("false"))`, which will parse one of those literals
  - The output type of a literal parser, like `"true"`, is `()`
  - This means that `"true" | "false"` is also `()`
- The `<>` wrapping `"true" | "false"` corresponds to `.span()`, which captures the portion of the input as a `&str`
  - This is necessary because it allows us to distinguish which literal was parsed, while if we didn't wrap the pattern in `<>` we would only get `()`, which gives us no information
- `value=` creates a binding (variable) assigned to the parsed value for `<"true" | "false">`
- `-> bool` defines the return type of the parser, which is converted into `Option<bool>` in the final function
- The block `{ value == "true" }` is inserted at the end of the function, to evaluate the final output

Now that we've defined what a boolean is, we're ready to start making our boolean expression evaluator. There are only three operations we care about: `!`, `&`, and `|`. We also want to be able to parenthesize expressions.

To start, let's define another rule which will handle parsing a term of an expression. For now, it will only parse a boolean.

```rust
parser! {
  boolean:
    value=<"true" | "false">
  -> bool {
      value == "true"
  }

  term = boolean -> bool;
}
```

This showcases an alternative syntax untwine offers for defining parsers, which is using `=` instead of `:` after the name of the parser. This expands to something like this:

```rust
fn term(ctx: &ParserContext) -> Option<bool> {
  let output = boolean(ctx)?;
  Some(output)
}
```

Since no operations are made on the raw outputs and they are returned unchanged, we can use `=`, ending with a `;` instead of a code body.

### Defining an expression

With this `term` rule in place, we can build onto it by expanding its definition to include the negation of another term.

```rust
parser! {
  boolean:
    value=<"true" | "false">
  -> bool {
      value == "true"
  }

  negation: "!" value=term -> bool {
    !value
  }
  
  term = (boolean | negation) -> bool;
}
```

A negation is just a `!` followed by any valid boolean expression, and it returns the boolean negation of that value. Note that our `term` rule's pattern has changed to `(boolean | negation)`, since a term may be either a boolean literal or a negation.

Now, we'll want to implement the binary operators, `|` and `&`. Many languages assign these precedences; I will not. We'll first want to define a function to handle a single operation, outside the parser block:

```rust
fn operate(left: bool, op: char, right: bool) -> bool {
  match op {
    '&' => left && right,
    '|' => left || right,
    _ => unreachable!(),
  }
}
```

This will allow us to easily define a rule for an expression:

```rust
parser! {
  boolean:
    value=<"true" | "false">
  -> bool {
      value == "true"
  }

  negation: "!" value=term -> bool {
    !value
  }
  
  term = (boolean | negation) -> bool;

  operator = ["&|"] -> char;

  pub expr: first=term rest=(operator term)* -> bool {
    rest.into_iter().fold(first, |left, (op, right)| operate(left, op, right))
  }
}
```

Here, two new rules are defined. `operator` uses `["&|"]`. Similar to regex, `[]` denotes a character group (though unfortunately inner characters must be wrapped in a string literal as well due to macro constraints). So this will match a single character, either `&` or `|`, and return that character.

The `expr` rule is far more complicated, so let's break it down:

- First, capture a single term into a variable called `first`
- Next, make a variable called `rest` and repeatedly parse more pairs of operators and terms
  - `(operator term)*` is a pattern `(operator term)`, which matches an operator followed by a term, modified by `*`, which makes it match repeatedly
  - The output type of `(operator term)` is `(char, bool)` since `operator` parses a `char` and `term` parses a `bool`.
  - The output type of `(operator term)*`, then, is `Vec<(char, bool)>`
  - The `*` modifier can match zero times, and will return an empty list in this case
  - The `+` modifier works exactly like `*` but requires at least one match

So we essentially define an expression as a term followed by any number of operators and additional terms.

This allows us to cleverly use `fold` to evaluate the expression, using `first` as the initial value and merging each subsequent value into it by applying the operator to it with the right-hand value.

### Using matches

While the above parser works perfectly fine, the definition of `boolean` is a bit awkward.
This becomes especially awkward if you have more than two elements. Take the example of a weekday parser:

```rust
parser! {
  weekday: day=<"monday" | "Monday" | "tuesday" | "Tuesday" | "wednesday" | "Wednesday" | "thursday" | "Thursday" | "friday" | "Friday" | "saturday" | "Saturday" | "sunday" | "Sunday">
  -> Weekday {
    match day {
      "monday" | "Monday" => Weekday::Monday,
      "tuesday" | "Tuesday" => Weekday::Tuesday,
      "wednesday" | "Wednesday" => Weekday::Wednesday,
      "thursday" | "Thursday" => Weekday::Thursday,
      "friday" | "Friday" => Weekday::Friday,
      "saturday" | "Saturday" => Weekday::Saturday,
      "sunday" | "Sunday" => Weekday::Sunday,
      _ => unreachable!()
    }
  }
}
```

When matching against many values, it is tedious to have to capture a string, only to match against it again
inside the parser body. It also makes it easy to miss a case without noticing, and having an "unreachable"
case which will crash your program if an entry is forgotten is not ideal.

To address this, an additional kind of parser was introduced to untwine: A match-type parser. These parsers
function much like a regular match statement in rust, mapping patterns to expressions. Using this syntax,
the same logic can be expressed much more simply:

```rust
parser! {
  weekday = match {
      ("monday" | "Monday") => Weekday::Monday,
      ("tuesday" | "Tuesday") => Weekday::Tuesday,
      ("wednesday" | "Wednesday") => Weekday::Wednesday,
      ("thursday" | "Thursday") => Weekday::Thursday,
      ("friday" | "Friday") => Weekday::Friday,
      ("saturday" | "Saturday") => Weekday::Saturday,
      ("sunday" | "Sunday") => Weekday::Sunday,
  } -> Weekday;
}
```

Note the `()` around each pattern is only required due to using a choice between two strings. This is because
these are actually top-level patterns using the same format as a regular parser, meaning you can capture
values directly into variables which are used in your expression:

```rs
parser! {
  value = match {
    "bool " b=boolean => Value::Boolean(b),
    "int " i=int => Value::Int(i),
  } -> Value;
}
```

This syntax is extremely useful for creating parsers which have many different structures that evaluate to the same type.
However, these match statements can only be used top-level, to prevent the syntactic clutter of nesting a match statement inside
the definition of another parser.

### Running your parsers

Notice that the `expr` parser has `pub` in front of it. This means it will be accessible outside the parser block, and generally means it makes sense as an "entry point". A single parser block may have multiple exported parsers, as this can often be useful for debugging.

Now that we have an exported parser, we're ready to test it. There are several ways to do this.

- To test it statically with hard-coded values, you can use `untwine::parse(expr, "false&true")` or `untwine::parse_pretty(expr, "false&true", Default::default())` for a prettified error message.
- To open a (very simple) interactive debug REPL using your parser, you can use `untwine::parser_repl(expr)`
- Note that both of these use cases require that the output type of the parser is `'static`
  - If you need to bypass this limitation, you can instantiate `ParserContext` yourself, then call it like `expr(&context)`
  - When executing like this, use `context.result(expr(&context))` to get a `Result` similar to `untwine::parse`

### Whitespace

However, if you test this out, you may notice that a few things are missing from this parser.
- You can run it with inputs like `false|false|true` and `!true&!false` just fine
- Inputs with spaces, like `true | false`, will break it
- It doesn't support parentheses

Luckily, these are both very easy to implement. First, we can define whitespace:

```rust
  sep = #{|c| c.is_ascii_whitespace()}*;
```

This introduces two new pieces of syntax. The `{}` enclose a function which takes `&char` and returns a `bool` indicating whether that character should be matched. `{|c| c.is_ascii_whitespace()}` means "parse an ascii whitespace character".

`#` is a symbol which explicitly ignores the output of whatever pattern follows. This allows us to omit the `->` and return type from the rule entirely, much like how you can omit `-> ()` on regular Rust functions.

So this rule will take any number of whitespace characters, including none at all (this is optional whitespace, thanks to `*`).

Now we can insert this rule in between all our tokens to allow whitespace:

```rust
parser! {
  sep = #{|c| c.is_ascii_whitespace()}*;

  boolean = match {
    "true" => true,
    "false" => false,
  } -> bool;

  negation: "!" sep value=term -> bool {
    !value
  }
  
  term = (boolean | negation) -> bool;

  operator = ["&|"] -> char;

  pub expr: first=term sep rest=(operator sep term)* -> bool {
    rest.into_iter().fold(first, |left, (op, right)| operate(left, op, right))
  }
}
```

If you test this out again, you'll be able to put whitespace anywhere in your expressions without issues.

### Parentheses

Lastly, we need to implement parenthesis. This one is extremely simple; we can just modify the `term` rule to include expressions wrapped in parentheses.

```rust
  term = (boolean | negation | "(" sep expr sep ")") -> bool;
```

With this, the boolean expression evaluator is complete, and we can evaluate complex expressions like `(true & false) | !!!(!false | !(false & true))`.

The complete code for this example can be found at https://github.com/boxbeam/untwine/blob/master/examples/expr.rs.

## Sentence parser

This example is much simpler, but covers a few pieces of syntax not used in the boolean expression parser.

We'll begin by parsing a word, allowing only lowercase letters:

```rust
parser! {
  word: value=<'a'-'z'+> -> String { value.into() }
}
```

Here, we use `'a'-'z'` to capture any lowercase alphabetic character (though `|c| c.is_ascii_lowercase()` would do the same) and `+` to parse it as many times as possible, requiring at least one match. The entire pattern is then captured as a single `&str` by `<>`.

Next, we can define a sentence as a list of words, separated by spaces and followed by a period.

```rust
parser! {
  word: value=<'a'-'z'+> -> String { value.into() }
  pub sentence = word$" "+ "." -> Vec<String>;
}
```

Here, `word$" "+` parses a list of words delimited by spaces, discarding the spaces implicitly. Since the final modifier is `+`, it must match at least one word. Each subsequent delimiter must be followed by another word.

Finally, we parse the period, which is ignored.

Since any pattern can be used as the delimiter, we can also nest a pattern to allow commas following each word (but not after the last word in the sentence):

```rust
parser! {
  word: value=<'a'-'z'+> -> String { value.into() }
  pub sentence = word$(","? " ")+ "." -> Vec<String>;
}
```

And now we have a parser which will validate a sentence structure, enforcing these rules:
- A word is composed of lowercase alphabetic characters
- A sentence must consist of at least one word
- After each word in a sentence, another word may follow, but there first must be a space or a comma followed by a space
- A sentence must end with a period

It will then return a list of all the words.

This is not a particularly useful parser, but it helps to demonstrate the last few common pieces of syntax.

## Closing notes

When using Untwine, remember that it is left-recursive. This means that each parser will be applied in the order it is shown. This matters in several ways.

First of all, self-referential patterns are legal, but a pattern which unconditionally matches itself will cause a stack overflow. Examples:

```rust
parser! {
  root = root "?";
  choices = (choices | "a");
}
```

In some cases this may be mitigated, but in general, avoid patterns that look anything like this.

Additionally, it is possible to create patterns which share a prefix of another pattern, which could cause parsing to "stop short". A common example is ints and floats:

```rust
struct Number {
  Int(i64),
  Float(f64),
}

parser! {
  int: value=<"-"? '0'-'9'+> -> Number { Number::Int(value.parse().unwrap()) }
  float: value<"-"? '0'-'9'+ "." '0'-'9'+> -> Number { Numbeer::Float(value.parse().unwrap()) }
  pub num = (int | float) -> Number;
}
```

In this situation, you will run into an error if you input a float like `1.23`. This is another common mistake, and it happens because choices (and repeating modifiers, including delimited lists) are greedy, and will take the first thing that matches.

For choice patterns like `(int | float)`, the order matters. It first tries matching `int`, which successfully matches `1`, and then leaves `.23` in the parsing buffer, indicating parsing has failed.

Instead, the `num` rule should be written like `(float | int)`. Since all floats start with an int, this rule should be written to try parsing it as a float first. If it can't be parsed as a float, it will then try parsing it as an int, allowing both `-1` and `-1.23` to parse just fine.
