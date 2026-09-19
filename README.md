# Slip

A pre-alpha compiler with an emphasis on simplicity and extensibility.

# Syntax

S-expressions.

The prefix sigils `$`, `@`, and `#` must be adjacent to their operands:
`$(operation ...)`, `@ref`, `@(doc "...")`, and `#(expression ...)`.
Spaces, tabs, newlines, and comments are not allowed between a prefix and its
operand. Whitespace inside forms and between expressions is unaffected.

## Types

Use ":" for specifying types. In many cases they can be deduced.
`(var x 99:int)`

## Annotations

Currently the only annotation in use is @ref for pass-by-ref parameters.

Use "@(s-exp)" for annotations. Applies to the immediately following expression.
`@(inline) (func hello():void ...`

If there are no args, the () may be ommitted.
`@inline (func hello():void ...`

If there are several annotations, they are all added.
`@inline @(doc "say hello") (func hello():void ...`

## Comments

From ";" to the end of the line

## Compile-time operations

`#(expression...)` evaluates `expression...` during compilation.

## Syntax operations (lexer support only)

`$(operation ...)` is a distinct syntax-operation form. The lexer preserves
the parenthesized contents without interpreting
the operation name.

Forms may nest, for example `$(let body_env $(child_env env))`, and may appear
in declaration-name, type, and annotation positions. Leading annotations and
trailing type annotations or member selectors attach to the complete form.

## Developing

Run 'tools/meson-init' to generate build configurations.
In 'build/clang-debug' run 'ninja' or 'ninja test'
