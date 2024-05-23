## or-q
Use `or?` to apply logical 'or' with short-circuit evaluation to the results of applying a value to two application functions.

### Basic syntax

To apply logical 'or' with short-circuit evaluation to the results of applying a value to two application functions, use the following syntax:

or? *a b value*

### Arguments

Use the following arguments to specify the application functions and the value to be applied using the `or?` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| a | x:<r> -> bool | Specifies the first application function. |
| b | x:<r> -> bool | Specifies the second application function. |
| value | <r> | Specifies the value to be applied to both application functions. |

### Return value

The `or?` function returns a boolean value representing the logical 'or' operation with short-circuit evaluation applied to the results of applying the value to the two application functions.

### Examples

The following example demonstrates the use of `or?` in the Pact REPL:

```lisp
pact>(or? (> 20) (> 10) 15)
true
```

In this example, the value `15` is applied to the application functions `>(20)` and `>(10)`. The logical 'or' operation with short-circuit evaluation is performed on the results, resulting in `true`.
