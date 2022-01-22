# Slip

A pre-alpha compiler with an emphasis on simplicity and extensibility.

# Syntax

S-expressions.

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

## Developing

Run 'tools/meson-init' to generate build configurations.
In 'build/clang-debug' run 'ninja' or 'ninja test'
