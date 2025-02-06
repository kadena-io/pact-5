## env-set-debug-flag

Use `env-set-debug-flag` to display the result of the internal tree transformations that the Pact interpreter performs during program execution. 
This function lets you inspect Pact trees from modules and terms in the way the CEK machine understands them.
This function is primarily suitable for advanced debugging.

### Basic syntax

To set the Pact REPL debug flags, use the following syntax:

```pact
(env-set-debug-flag flag)
```

### Arguments

Use the following argument when using the `env-set-debug-flag` Pact function.

| Argument | Type         | Description   |
|----------|--------------|---------------|
| `flag`  | string | Specifies the debug flag to set. The valid values are "lexer", "parser", and "desugar".|

### Return value

The `env-set-debug-flag` function returns debugging details from the Pact interpreter.

### Examples

The following example demonstrates how to use the `env-set-debug-flag` function to display debugging output from the Pact lexer:

```pact
(env-set-debug-flag "lexer")
set debug flags to [lexer]
()
pact> (+ 2 3)
----------- Lexer output -----------------
[(, ident<+>, number<2>, number<3>, )]
5
```

The following example illustrates using the `env-set-debug-flag` function to display lexer, parser, and desugar debugging output from the Pact interpreter:

```pact
pact> (env-set-debug-flag "parser")
set debug flags to [parser]

pact> (env-set-debug-flag "lexer")
----------- Parser output ----------------
(env-set-debug-flag "lexer")
set debug flags to [lexer, parser]
()

pact> (env-set-debug-flag "desugar")
----------- Lexer output -----------------
[(, ident<env-set-debug-flag>, "desugar", )]
----------- Parser output ----------------
(env-set-debug-flag "desugar")
set debug flags to [lexer, parser, desugar]
()

pact> (fold (+) 0 [ 12 3 -9 30])
----------- Lexer output -----------------
[ (
, ident<fold>
, (
, ident<+>
, )
, number<0>
, [
, number<12>
, number<3>
, number<-9>
, number<30>
, ]
, ) ]
----------- Parser output ----------------
(fold (+) 0 [12, 3, -9, 30])
----------- Desugar output ---------------
(fold (+) 0 [12, 3, -9, 30])
36
```
