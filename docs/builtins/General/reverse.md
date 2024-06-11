## reverse

Use `reverse` to reverse the order of specified `elements` in a list.

### Basic syntax

To reverse a specified list of `elements`, use the following syntax:

```pact
(reverse [elements])
```

### Arguments

Use the following argument to specify the `elements` to be reversed using the `reverse` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `elements` | [any] | Specifies the elements in the list you want to be reversed. |

### Return value

The `reverse` function returns a new list with the elements in reverse order.

### Example

The following example demonstrates how to use the `reverse` function in the Pact REPL. 
This example reverses the order of numbers in the list:

```pact
pact> (reverse [1 2 3])
[3 2 1]
```

In the following example, the `reverse` function reverses the order of strings in a list:

```pact
pact> (reverse ["lastname" "firstname" "age" "occupation"])
["occupation" "age" "firstname" "lastname"]
```

You can also reverse the order of objects in a list. 
For example:

```pact
(reverse [{"lastname":"pistolas","firstname": "lola"} {"lastname":"smith","firstname": "tim"}])
[{"lastname": "smith","firstname": "tim"}
{"lastname": "pistolas","firstname": "lola"}]
```
This example illustrates how to use the `reverse` function to reverse the order of elements in a list in Pact.
