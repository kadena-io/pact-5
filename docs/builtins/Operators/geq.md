## greater-than-equal (>=)

Use `>=` to returns true if the first argument `oper1` is greater than or equal to the second argument `oper2`.

### Basic syntax

To check if `oper1` is greater than or equal to `oper2`, use the following syntax:

```pact
(>= oper1 oper2)
```

### Arguments

Use the following arguments to specify the values for comparison using the `>=` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `oper1` | integer, decimal, string, or time | Specifies the first value for comparison. |
| `oper2` | integer, decimal, string, or time | Specifies the second value for comparison. |

### Return value

The `>=` function returns a boolean value indicating whether `oper1` is greater than or equal to `oper2`.

### Examples

The following example demonstrates how to use the `>=` function to compare two integer values to check if the first value (`1`) is greater than or equal to the second value (`3`):

```pact
pact> (>= 1 3)
false
```

The following example demonstrates how to use the `>=` function to compare two decimal values to check if the first value (`5.24`) is greater than or equal to the second value (`2.52`):

```pact
pact> (>= 5.24 2.52)
true
```

The following example demonstrates how to use the `>=` function to compare two string values to check if the first value (`abc`) is greater than or equal to the second value (`def`):

```pact
pact> (>= "abc" "def")
false
```
