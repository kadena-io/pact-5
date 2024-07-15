## or?

Use `or?` to apply a logical OR operation to the results of applying a specified `value` to application functions `func1` and `func2`, with short-circuit evaluation.

You can use any data type for the `value` argument as long as the two functions take that same data type and return the resulting boolean value for the logical OR operation performed by the `or?` function.

By convention, the data type <a> is used if an argument represents a type-bound parameter like the `value` argument in this function. 

### Basic syntax

To apply a logical OR operation to the results of applying a value to two application functions, use the following syntax:

```pact
(or? func1 func2 value)
```

### Arguments

Use the following arguments to specify the functions and the `value` to be applied using the `or?` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `func1` | function x:<a> -> bool | Specifies the first function to apply the specified `value` to. The result of applying the specified value returns a boolean value. |
| `func2` | function x:<a> -> bool | Specifies the second function to apply the specified `value` to. The result of applying the specified value returns a boolean value.|
| `value` | <a> | Specifies the value to apply to both `func1` and `func2` functions. |

### Return value

The `or?` function returns a boolean value representing the logical OR operation after evaluating the results from applying the specified value to the two application functions.

### Examples

The following example demonstrates how to use the `or?` function in the Pact REPL:

```pact
pact> (or? (> 20) (> 10) 15)
true
```

In this example, the `or?` function applies the value 15 to the function `(> 20)`, with the result being `true` because `20 > 15` is true.
Because the function performs short-circuit evaluation on the results, the `or?` function returns `true` because the first condition is true.
