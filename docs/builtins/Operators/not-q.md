## not?

Use `not?` to apply a logical NOT operation to the results of applying a specified `value` to an application function.

### Basic syntax

To apply a logical NOT operation to the results of applying a specified `value` to an application function `app`, use the following syntax:

```pact
(not? app value)
```

### Arguments

Use the following arguments to specify the application function and the value to be applied using the `not?` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `app` | function x:<r> -> bool | Specifies the application function to apply the specified `value` to. The result of applying the specified value returns a boolean value. |
| `value` | <r> | Specifies the value to be applied to the application function. |

### Return value

The `not?` function returns a boolean value representing the logical negation of the result of applying the value to the application function.

### Examples

The following example demonstrates how to use the `not?` function in the Pact REPL:

```pact
pact> (not? (> 20) 15)
false
```

In this example, the application function is `(> 20)` and the value is `15`. Because the expression `20 > 15` evaluates to `true`, and `not?` negates this value, the `not?` function returns `false`.
