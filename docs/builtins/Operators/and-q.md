## and?

Use `and?` to apply a logical AND operation to the results of applying a specified `value` to application functions `func1` and `func2`, with short-circuit evaluation.

You can use any data type for the `value` argument as long as the two functions take that same data type and return the resulting boolean value for the logical AND operation performed by the `and?` function.

By convention, the data type `<a>` is used if an argument represents a type-bound parameter like the `value` argument in this function: 

(defun logicalAnd and?:bool (func1:(`<a>` -> bool) func2:(`<a>` -> bool) value:`<a>`))

### Basic syntax

To apply a logical AND operation to the results of applying a specified `value` to the functions `func1` and `func2`, use the following syntax:

```pact
(and? func1 func2 value)
```

### Arguments

Use the following arguments to specify the functions and `value` for the `and?` operation.

| Argument | Type | Description |
| --- | --- | --- |
| `func1` | function x:`<a>` -> bool | Specifies the first function to apply the specified `value` to. The result of applying the specified value of type `<a>` returns a boolean value. |
| `func2` | function x:`<a>` -> bool | Specifies the second function to apply the specified `value` to. The result of applying the specified value of type `<a>` returns a boolean value.|
| `value` | `<a>` | Specifies the value to apply to both `func1` and `func2` functions. |

### Return values

The `and?` function returns a boolean value based on the result of applying `value` to `func1` and `func2` with the logical AND operation.

### Examples

The following example demonstrates how to use the `and?` function in the Pact REPL:

```pact
pact> (and? (> 20) (> 10) 15)
false
```

In this example, the `and?` function applies the value 15 to the function `(> 20)`, with the result being `true` because `20 > 15` is true.
The function then applies the value of 15 to the `(> 10)` function, with the result being false because `10 > 15` is false.
The result from the `and?` function, therefore, is `false` because the second condition is false.
