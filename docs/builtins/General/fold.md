## fold

Use `fold` to iteratively reduce a list by applying the `app` function to the last result for each element in the list, starting with the specified `init` initial value.

You can use any data type for the `value` argument as long as the first `oper1` functions can take that same data type.
By convention, data type notation like `<a>` and `<b>` are used to represent  type-bound parameters like the `init` and `list` arguments in this function.

### Basic syntax

To iteratively reduce a list by applying a function to each element, starting with an initial value, use the following syntax:

```pact
(fold app init [list])
```

### Arguments

Use the following arguments to specify the function, initial value, and list for the `fold` Pact function:

| Argument | Type       | Description                                       |
|----------|------------|---------------------------------------------------|
| `app` | function x: `<a>` y: `<b> -> <a>` | Specifies the function to apply to each element and the last result. |
| `init` | `<a>` | Specifies the initial value for the reduction. |
| `list` | [`<b>`] | Specifies the list to iterate over.               |

### Return value

The `fold` function returns the final result of the iterative reduction of the list.

### Examples

The following example demonstrates the `fold` function:

```pact
pact>(fold (+) 0 [100 10 5])
115
```

In this example, `(+)` is used as the function to apply, which is addition in this case. 
The `fold` function starts with an initial value of `0` and iteratively adds each element of the list `[100 10 5]` to the previous result. Here's the breakdown:

- Initial value: `0`
- First iteration: `0 + 100 = 100`
- Second iteration: `100 + 10 = 110`
- Third iteration: `110 + 5 = 115`

The final result of the `fold` operation is `115`. The `fold` function is commonly used for operations that require accumulating results over a list in Pact contracts.
