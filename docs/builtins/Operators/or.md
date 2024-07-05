## or

Use `or` to apply a logical OR operation with short-circuit evaluation.

### Basic syntax

To perform a logical OR operation with short-circuit evaluation, use the following syntax:

```pact
(or oper1 oper2)
```

### Arguments

Use the following arguments to specify the boolean values for which you want to perform the logical OR operation using the `or` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `oper1` | bool | Specifies the first expression to evaluate that returns the boolean value to perform the logical OR operation on.|
| `oper2` | bool | Specifies the second expression to evaluate that returns the boolean value to perform the logical OR operation on. |

### Return value

The `or` function returns a boolean value based on the logical OR operation of the input values.

### Examples

The following example demonstrates how to use the `or` function in the Pact REPL:

```pact
pact> (or (> 20 10) (> 10 15))
true
```

In this example, the `or` function evaluates the expressions `(> 20 10)` and `(> 10 15)`. 
The boolean value returns for the first expression is `true` because `20 > 15` is true.
Because the `or` function performs short-circuit evaluation on the results, the function returns `true` because the first expression is true.

