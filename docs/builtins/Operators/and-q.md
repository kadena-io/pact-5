## and?

Use `and?` to apply a logical AND operation to the results of applying a specified `value` to application functions `oper1` and `oper2`, with short-circuit evaluation.

In practice, the data type of the `value` you specify is used to evaluate the expression in each function and to return the resulting boolean value for the logical AND operation performed by the `and?` function:

(defun <a> and?:bool (oper1:(<a> -> bool) oper2:(<a> -> bool) value:<a>))

### Basic syntax

To apply a logical AND operation to the results of applying a specified `value` to the functions `oper1` and `oper2`, use the following syntax:

```pact
(and? oper1 oper2 value)
```

### Arguments

Use the following arguments to specify the functions and `value` for the `and?` operation.

| Argument | Type | Description |
| --- | --- | --- |
| `oper1` | function x:<r> -> bool | Specifies the first function to apply the specified `value` to. The result of applying the specified value of type `<r>` returns a boolean value. |
| `oper2` | function x:<r> -> bool | Specifies the second function to apply the specified `value` to. The result of applying the specified value of type `<r>` returns a boolean value.|
| `value` | <r> | Specifies the value to apply to both `oper1` and `oper2` functions. |

### Return values

The `and?` function returns a boolean value based on the result of applying `value` to `oper1` and `oper2` with the logical AND operation.

### Examples

The following example demonstrates how to use the `and?` function in the Pact REPL:

```pact
pact> (and? (> 20) (> 10) 15)
false
```

In this example, the `and?` function applies the value 15 to the function `(> 20)`, with the result being `true` because `20 > 15` is true.
The function then applies the value of 15 to the `(> 10)` function, with the result being false because `10 > 15` is false.
The result from the `and?` function, therefore, is `false` because the second condition is false.
