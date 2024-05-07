## bitwise-or
The `|` function computes the bitwise OR operation between two integers.

### Basic syntax

To compute the bitwise OR operation between two integers `X` and `Y`, use the following syntax:

`(| X Y)`

### Arguments

Use the following arguments to specify the integers for the bitwise OR operation using the `|` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `X` | `integer` | Specifies the first integer for the OR operation. |
| `Y` | `integer` | Specifies the second integer for the OR operation. |

### Return value

The `|` function returns the result of the bitwise OR operation as an integer.

### Examples

The following examples demonstrate the usage of the `|` function within a Pact script. They compute the bitwise OR operation between two integers:

```lisp
pact>(| 2 3)
3
```

```lisp
pact>(| 5 -7)
-3
```

These examples illustrate how to use the `|` function to perform bitwise OR operations on integers in Pact, facilitating bitwise manipulation and logic operations.
