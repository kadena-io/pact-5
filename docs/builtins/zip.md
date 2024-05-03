The `zip` function combines two lists using a specified function `f` into a new list. The length of the resulting list is determined by the length of the shortest input list.

### Basic syntax

To combine two lists with a specified function `f` into a new list, use the following syntax:

zip *f* *list1* *list2*

### Arguments

Use the following arguments to specify the function and the lists for combination using the `zip` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| f | x:<a> y:<b> -> <c> | Specifies the function to combine elements from the two lists. |
| list1 | [<a>] | Specifies the first list to be combined. |
| list2 | [<b>] | Specifies the second list to be combined. |

### Return value

The `zip` function returns a new list containing elements combined from the input lists according to the specified function `f`.

### Examples

The following examples demonstrate the usage of the `zip` function within a Pact script. They combine two lists using specified functions `+` and `-` into new lists:

```lisp
(zip (+) [1 2 3 4] [4 5 6 7])
```
```lisp
(zip (-) [1 2 3 4] [4 5 6])
```
```lisp
(zip (+) [1 2 3] [4 5 6 7])
```

These examples illustrate how to use the `zip` function to combine elements from two lists using specified functions in Pact, producing a new list with combined elements. The resulting list's length is determined by the shortest input list.
