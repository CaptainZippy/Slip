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

## Macros and syntax operations

Macros take raw syntax arguments and a caller-environment parameter:

```slip
(macro twice (value) env
    (add $(expand value env) $(expand value env)))

(func main ():int
    (twice 3))
```

Invocation is an ordinary call, `(twice 3)`, not a `$` operation. Each invocation
has a private macro environment that is a child of its definition environment.
Caller syntax only sees that private environment if explicitly expanded there.
Inserting a declaration name does not publish it: generated declarations stay
private unless explicitly bound or expanded into the caller environment.
Function-generating macros must currently be invoked at module scope; generated
functions are not closures. A generated function named by the syntax parameter
`fname` can be published with `$(bind env $(insert fname) $(expand fname))`.

`$(operation ...)` performs syntax expansion before ordinary expression parsing:

| Operation | Meaning |
| --- | --- |
| `$(insert value)` | Replace one syntax node with a raw syntax handle from a macro parameter, an expansion-time binding, or a nested syntax operation. Do not look up the inserted identifier while inserting it. |
| `$(let name value)` | Create a read-only expansion-time binding to a syntax, environment, or declaration handle. Produce no runtime value; do not replace a same-scope name. |
| `$(make_env env)` | Create a fresh child of the supplied environment. |
| `$(bind env name declaration)` | Alias an existing named declaration into exactly `env`. The name is literal identifier syntax or `$(insert parameter)`. A computed value is not a declaration, and binding never copies a runtime value. |
| `$(expand syntax [env])` | Parse bound syntax in exactly the supplied environment, or the current macro environment when omitted. No implicit child is created, so declarations remain visible to later expansions in that environment. |
| `$(stringize syntax)` | Produce a runtime string containing the bound syntax. |

Forms may nest, for example `$(let body_env $(make_env env))`. Inserted syntax
works in expression and call-head positions, declaration, function, parameter
and field names, types, annotations, and member selectors. For example:

```slip
(var $(insert name):$(insert type) 0)
@$(insert annotation) (func $(insert fname) ():int 1)
$(insert object).$(insert field)
```

Annotations, types, and selectors decorate the inserted occurrence, not the
reusable raw argument. Insertion is single-node substitution, not splicing or
quotation.

The nearest lexical scope, including imports at that level, shadows ancestor
bindings. Ordinary function declarations with the same name can still overload
within one scope. `$(let ...)` and `$(bind ...)` reject duplicate same-scope names,
even for function handles; they do not extend overload groups. Expansion-time
`$(let ...)` bindings may also appear at module scope without generating runtime
code. Use a fresh body environment to keep exposed names local:

```slip
(macro countup (vname limit body) env
    $(let body_env $(make_env env))
    (var cur 0:int)
    (var end $(expand limit env))
    (while (lt? cur end)
        (var copy cur)
        $(bind body_env $(insert vname) copy)
        $(expand body body_env)
        (set! cur (add cur 1))))
```

Here `(countup i 4 (set! i 100))` terminates after four iterations: `i` aliases
the explicit per-iteration `copy`, not `cur`. Caller variables are unchanged,
and the body cannot access the macro-private `cur` or `end`. To deliberately
expose an alias to caller syntax, bind into `env` instead; the aliased runtime
declaration must still be used within its runtime scope.

## Compile-time operations

`#(expression ...)` evaluates an ordinary expression during compilation,
after syntax expansion.

## Developing

Run 'tools/meson-init' to generate build configurations.
In 'build/clang-debug' run 'ninja' or 'ninja test'
