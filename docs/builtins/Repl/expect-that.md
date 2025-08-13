## expect-that

Use `expect-that` to evaluate an expression and succeed if the resulting value passes a predicate function.

You can use any data type for the `exp` argument as long as the `pred` function can take that same data type and return the resulting boolean value .
By convention, the data type `<a>` is used to represent type-bound parameters that serve as input for functions and expressions or for generic arguments.

### Basic syntax

To evaluate an expression that returns a predicate function, use the following syntax:

```pact
(expect-that doc pred exp)
```

### Arguments

Use the following arguments when using the `expect-that` Pact function.

| Argument | Type | Description |
|----------|------|-------------|
| `doc` | string | Specifies the documentation string describing the expectation. |
| `pred` | value: `<a> -> bool` | Specifies the predicate function that takes the result of `exp` and returns a boolean. |
| `exp` | `<a>` | Specifies the expression to evaluate. The expression can be of any Pact type.          |

### Return value

The `expect-that` function returns a string indicating the success or failure of the expectation.

### Examples

The following example demonstrates how to use the `expect-that` function to evaluate an expression that returns the expected result:

```pact
pact> (expect-that "addition" (< 2) (+ 1 2))
"Expect-that: success: addition"
```

The following example demonstrates how to use the `expect-that` function to evaluate an expression that fails to return the expected result:

```pact
pact> (expect-that "addition" (> 2) (+ 1 2))
"FAILURE: addition: did not satisfy (> 2) : 3:integer"
```
