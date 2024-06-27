## add (+)
Use `+` to add numbers, concatenate strings and lists, or merge objects.

### Basic syntax

To add numbers, concatenate strings and lists, or merge objects, use the following syntax:

```pact
(+ oper1 oper2)
```

### Arguments

Use the following arguments to specify the values for addition, concatenation, or merging using the `+` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `oper1` | integer, decimal, string, [list], or object | Specifies the first operand for addition, concatenation, or merging. |
| `oper2` | integer, decimal, string, [list], or object | Specifies the second operand for addition, concatenation, or merging. |

### Return value

The `+` function returns the result of addition for numbers, the concatenated string or list for strings and lists, or the resulting of merging for objects.

### Examples

The following examples demonstrate how to use the `+` function to add two numbers in the Pact REPL:

```pact
pact> (+ 1 2)
3

pact> (+ 5.0 20.5)
25.5
```

The following examples demonstrate how to use the `+` function to concatenate strings and lists in the Pact REPL:

```pact
pact> (+ "every" "body")
"everybody"

pact> (+ [1 2] [3 4])
[1 2 3 4]
```

The following example demonstrates how to use the `+` function to merge objects in the Pact REPL:

```pact
pact> (+ { "foo": 100 } { "foo": 1, "bar": 2 })
{"bar": 2,"foo": 100}
```

In this example, merging the object fields using the `+` function results in the first operand value replacing the corresponding field in the second operand.