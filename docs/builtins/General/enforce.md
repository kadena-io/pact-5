## enforce

Use `enforce` to test whether a pure `expression` evaluates to true or false.
If the specified `expression` evaluates to true, the function returns true.
If the specified `expression` evaluates to false, the function fails the transaction and displays the specified error `message`.

### Basic syntax

To fail a transaction with a specified error message if an expression evaluates to false, use the following syntax:

```pact
(enforce expression message)
```

### Arguments

Use the following arguments to specify the test expression and error message for the `enforce` Pact function:

| Argument | Type   | Description                                    |
|----------|--------|------------------------------------------------|
| expression | bool | Specifies the expression to evaluate.     |
| `message` | string | Specifies the error message to display if the `expression` evaluates as false. |

### Return values

The `enforce` function returns `true` if the specified `expression` is true. If the `expression` is false, the function fails the transaction with the specified error message.

### Examples

The following example demonstrates how to use the `enforce` function to evaluate the expression `(+ 2 2) = 4`:

```pact
pact> (enforce (= (+ 2 2) 4) "All is well")
true
```

Because the specified expression (`2 + 2 = 4`) is true, the function returns true and the transaction continues.

The following example demonstrates how to use the `enforce` function to evaluate the expression `(2 + 2) != 4`:

```pact
pact> (enforce (!= (+ 2 2) 4) "The expression is false")
The expression is false
 at <interactive>:0:0: (enforce (native `!=`  True if X does not equal Y.  Type: x... "The expression is false")
```

Because the expression is false, the transaction fails with the error message specified.
