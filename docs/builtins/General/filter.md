## filter

Use `filter` to filter a list of `elements` by applying the specified `apply` function to each element in the list.
For each element in the list, the `apply` function should return true or flase to identify the elements that should be included in the filtered list.

Each element that returns a result of `true` from the `apply` function is included in the resulting list with its original value. 
With the `filter` function, you can include elements in a list based on a specific condition.

### Basic syntax

To filter a list by applying a function to each element, use the following syntax:

```pact
(filter apply [elements])
```

### Arguments

Use the following arguments to specify the function and list for the `filter` Pact function:

| Argument | Type       | Description                                 |
|----------|------------|---------------------------------------------|
| `apply`    | function | Specifies the function to apply to each element of the list. The return value for the function must be a Boolean (`true` or `false`) to identify elements to be included in the resulting list. |
| `elements` | [any] | Specifies the list of elements to filter. The elements in the list can be any data type. |

### Return values

The `filter` function returns a new list containing elements from the original list for which the `apply` function returns `true`.

### Examples

The following example demonstrates how to use `filter` with a function that evaluates the length of strings to only include the strings with more than two characters:

```pact
(filter (lambda (str) (< 2 (length str))) ["my" "dog" "has" "fleas"])
["dog" "has" "fleas"]
```

In this example, `(lambda (str) (< 2 (length str)))` is used as the function that is applied to each element. 
This function checks the length of each element in the list. 
The `filter` function then filters the list `["my" "dog" "has" "fleas"]` based on this condition. 
The resulting list only contains the elements that returned `true` when the function was applied. In this case, the resulting list is `["dog" "has" "fleas"]`.

In the following example, `(compose (length) (= 3))` is the function applied to each element:

```pact
pact> (filter (compose (length) (= 3)) ["my" "red" "dog" "has" "fleas"])
["red" "dog" "has"]
```

Like the previous example, the `(compose (length) (= 3))` function checks the length of each element in the list. 
The `filter` function then filters the list based on this condition and the resulting list only contains the elements that returned `true` when the function was applied. 
In this case, the resulting list is `["red" "dog" "has"]`.
