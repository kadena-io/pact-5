## not-q
Use `not?` to apply logical 'not' to the results of applying a value to an application function.

### Basic syntax

To apply logical 'not' to the results of applying a value to an application function, use the following syntax:

not? *app value*

### Arguments

Use the following arguments to specify the application function and the value to be applied using the `not?` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| app | x:<r> -> bool | Specifies the application function. |
| value | <r> | Specifies the value to be applied to the application function. |

### Return value

The `not?` function returns a boolean value representing the logical negation of the result of applying the value to the application function.

### Examples

The following example demonstrates the use of `not?` in the Pact REPL:

```lisp
pact>(not? (> 20) 15)
false
```

In this example, the application function is `>(20)` and the value is `15`. The expression `>(20, 15)` evaluates to `true`, and `not?` negates this value, resulting in `false`.
