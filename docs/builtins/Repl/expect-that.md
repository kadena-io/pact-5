## expect-that

Use `expect-that` to evaluate an expression and succeed if the resulting value passes a predicate function.

### Basic syntax

```pact
(expect-that doc pred exp)
```

### Arguments

Use the following arguments when using the `expect-that` Pact function.

| Argument | Type                  | Description                                                                            |
|----------|----------------------|----------------------------------------------------------------------------------------|
| `doc`      | `string`               | Specifies the documentation string describing the expectation.                         |
| `pred`     | `value:<a> -> bool`  | Specifies the predicate function that takes the result of `exp` and returns a boolean. |
| `exp`      | `<a>`                | Specifies the expression to evaluate. The expression can be of any Pact type.          |

### Return value

The `expect-that` function returns a string indicating the success or failure of the expectation.

### Examples

The following examples demonstrate the usage of `expect-that` within a Pact REPL:

1. Expectation passes:
```pact
pact> (expect-that "addition" (< 2) (+ 1 2))
"Expect-that: success: addition"
```

2. Expectation fails:
```pact
pact> (expect-that "addition" (> 2) (+ 1 2))
"FAILURE: addition: did not satisfy (> 2) : 3:integer"
```
