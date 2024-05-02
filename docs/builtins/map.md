Use `map` to apply an application function (APP) to each element in a list (LIST), returning a new list of results.

## Basic syntax

To apply an application function to each element in a list, use the following syntax:

map *app list*

## Arguments

Use the following arguments to specify the application function and the list of elements to be mapped using the `map` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| app | x:<b> -> <a> | Specifies the application function to be applied to each element in the list. |
| list | [<b>] | Specifies the list of elements to be mapped. |

## Return value

The `map` function returns a new list containing the results of applying the application function to each element in the input list.

## Examples

The following example demonstrates the use of `map` in the Pact REPL:

```lisp
pact>(map (+ 1) [1 2 3])
[2 3 4]
```

In this example, the application function `(+ 1)` is applied to each element in the list `[1 2 3]`, resulting in a new list `[2 3 4]`.
