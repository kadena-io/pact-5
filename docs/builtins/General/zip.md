## zip

Use the `zip` function to combine two lists using a specified function `func` into a new list. 
The length of the resulting list is determined by the length of the shortest input list.

### Basic syntax

To combine two lists with a specified function `f` into a new list, use the following syntax:

```pact
(zip func list1 list2)
```

### Arguments

Use the following arguments to specify the function and the lists that you want to combine using the `zip` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `func` | function | Specifies the function to combine elements from the two lists. |
| `list1` | [any] | Specifies the first list to be combined. |
| `list2` | [any] | Specifies the second list to be combined. |

### Return value

The `zip` function returns a new list containing elements combined from the input lists according to the specified function `func`.

### Examples

The following examples demonstrate how to use the `zip` function in the Pact REPL. 
This example combines two lists of numbers using the specified addition (+) function a new list:

```pact
pact> (zip (+) [1 2 3 4] [4 5 6 7])
[5, 7, 9, 11]
```

The following example combines two lists of numbers using the specified subtraction (-) function to create a new list:

```pact
pact>(zip (-) [1 2 3 4] [4 5 6])
[-3, -3, -3]
```

The following example illustrate combining two lists using the multiplication (*) function to create a new list with its length determined by input list with the fewest items

```pact
pact> (zip (*) [1 2 3 ] [4 5 6 7 8])
[4 10 18]
```
