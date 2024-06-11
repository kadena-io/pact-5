## enforce
Use `enforce` to fail a transaction with a specified error message `MSG` if a pure expression `TEST` evaluates to false. Otherwise, it returns true.

### Basic syntax

To fail a transaction with a specified error message if a test expression evaluates to false, use the following syntax:

`(enforce TEST MSG)`

### Arguments

Use the following arguments to specify the test expression and error message for the `enforce` Pact function:

| Argument | Type   | Description                                    |
|----------|--------|------------------------------------------------|
| `TEST`   | `bool`   | Specifies the test expression to evaluate.     |
| `MSG`    | `string` | Specifies the error message if the test fails. |

### Return values

The `enforce` function returns true if the test expression is true. If the test expression is false, it fails the transaction with the specified error message.

### Examples

The following example demonstrates the `enforce` function inside the Pact REPL:

```pact
pact>(enforce (!= (+ 2 2) 4) "Chaos reigns")
<interactive>:0:0:Error: Chaos reigns
```

In this example, `(enforce (!= (+ 2 2) 4) "Chaos reigns")` is used to evaluate the test expression `(+ 2 2) != 4`. Since this expression is true (`4 != 4` is false), the transaction continues. However, if the expression were false, the transaction would fail with the error message "Chaos reigns". The `enforce` function provides a way to ensure conditions are met within a transaction.
