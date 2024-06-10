## fold

Use `fold` to iterate over a list of values by applying a specified function `app` to an initial value `init`, then to the result value for each element in the list.

### Basic syntax

To iterate over a list by applying a function to each element, starting with an initial value, use the following syntax:

```pact
(fold app init [list])
```

### Arguments

Use the following arguments to specify the function, initial value, and list for the `fold` Pact function:

| Argument | Type       | Description                                       |
|----------|------------|---------------------------------------------------|
| `app`      | function `x:<a> y:<b> -> <a>` | Specifies the function to apply to each element and the last result. |
| `init`     | any | Specifies the initial value for the reduction. |
| `list`     | list | Specifies the list to iterate over. |

### Return values

The `fold` function returns the final result of the iterative reduction of the list.

### Examples

The following example demonstrates how to use the `fold` function with addition `(+)` as the function to apply, `0` as the initial value, and `[100 10 5]` as the list of elements to iteratively add to the previous result:

```pact
pact> (fold (+) 0 [100 10 5])
115
```

In this example,  the `fold` function iterates through the list as follows:

- Initial value: `0`
- First iteration: `0 + 100 = 100`
- Second iteration: `100 + 10 = 110`
- Third iteration: `110 + 5 = 115`

The final result of the `fold` operation is `115`. 
The `fold` function is commonly used for operations that require accumulating results over a list in Pact contracts.
