## expect

Use `expect` to evaluate an expression and verify that the result equals an expected value.

### Basic syntax

```pact
(expect doc expected actual)
```

### Arguments

Use the following arguments when using the `expect` Pact function.

| Argument | Type   | Description                                                                  |
|----------|--------|------------------------------------------------------------------------------|
| `doc`      | `string` | Specifies the documentation string describing the expectation.               |
| `expected` | `<a>`  | Specifies the expected value to compare against the result of `actual`.      |
| `actual`   | `<a>`  | Specifies the expression to evaluate. The expression can be of any Pact type. |

### Return value

The `expect` function returns a string indicating the success or failure of the expectation.

### Example

The following example demonstrates the usage of `expect` within a Pact REPL:

```pact
pact> (expect "Sanity prevails." 4 (+ 2 2))
"Expect: success: Sanity prevails."
```
