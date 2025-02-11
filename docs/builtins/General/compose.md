## compose

Use `compose` to compose functions where `oper1` performs an operation using the specified `value` and `oper2` takes the results from `oper1` as input to produce the result for the composed function.

You can use any data type for the `value` argument as long as the first `oper1` functions can take that same data type.
By convention, the data type `<a>` is used to represent a type-bound parameter like the `value` argument in this function.

### Basic syntax

To compose a function using `oper1` and `oper2` with the specified `value`, use the following syntax:

```pact
(compose oper1 oper2 value)
```

### Arguments

Use the following arguments to specify the functions `oper1` and `oper2` for composing with `value` using the `compose` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `oper1` | function: `<a> -> <b>` | Specifies the first function to operate on the specified `value` and return a result to provide input to the second function. |
| `oper2` | function: `<b> -> <c>` | Specifies the second function to operate on the results of the `oper1` function. |
| `value` | `<a>` | Specifies the value on which `oper1` operates. |

### Return value

The `compose` function returns the result of applying `oper2` to the result returned by `oper1` for the specified `value`.

### Examples

In the following example, `compose` uses the `contains` function to evaluate the specified list for a string, then uses the `if` function to operate on the results of the `contains` function to display the final result:

```pact
pact> (compose (contains "summer") (if true "Success!") ["spring" "summer" "fall" "winter"]) 
"Success!"
```
