Use `filter` to filter a list by applying a function *`APP`* to each element. For each element where *`APP`* returns true, the original value is kept in the resulting list.

### Basic syntax

To filter a list by applying a function to each element, use the following syntax:

filter *app* *list* -> [*result*]

### Arguments

Use the following arguments to specify the function and list for the `filter` Pact function:

| Argument | Type       | Description                                 |
|----------|------------|---------------------------------------------|
| app      | x:\<a>->bool| Specifies the function to apply to each element of the list. |
| list     | [\<a>]     | Specifies the list to filter.               |

### Return values

The `filter` function returns a new list containing elements from the original list for which the *`APP`* function returns true.

### Examples

The following example demonstrates the `filter` function:

```lisp
(filter (compose (length) (< 2)) ["my" "dog" "has" "fleas"])
```

In this example, `(compose (length) (< 2))` is used as the function to apply to each element. This function checks if the length of each element is less than 2. The `filter` function then filters the list `["my" "dog" "has" "fleas"]` based on this condition. The resulting list will contain only elements that have a length less than 2, which in this case is `["my" "dog" "has"]`. The `filter` function provides a way to selectively include elements in a list based on a specified condition.
