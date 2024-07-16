## bitwise-or (|)

Use `|` to compute the bitwise OR operation between the first integer `oper1` value and the second integer `oper2` value.

### Basic syntax

To compute the bitwise OR operation between the `oper1` and `oper2` integer values, use the following syntax:

```pact
(| oper1 oper2)
```

### Arguments

Use the following arguments to specify the integers for the bitwise OR operation using the `|` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `oper1` | integer | Specifies the first integer for the OR operation. |
| `oper2` | integer | Specifies the second integer for the OR operation. |

### Return value

The `|` function returns the result of the bitwise OR operation as an integer.

### Examples

The following examples demonstrate how to use the `|` function to perform bitwise OR manipulation between two integers in a Pact REPL:

```pact
pact> (| 2 3)
3

pact> (| 5 -7)
-3
```
