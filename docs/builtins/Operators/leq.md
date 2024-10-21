## less-than-equal (`<=`)

Use `<=` to return true if the first `oper1` argument is less than or equal to the second `oper2` argument.

### Basic syntax

To check if `oper1` is less than or equal to `oper2`, use the following syntax:

```pact
(<= oper1 oper2)
```

### Arguments

Use the following arguments to specify the values for comparison using the `<=` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `oper1` | `<a[integer,decimal,string,time]>` | Specifies the first value for comparison. |
| `oper2` | `<a[integer,decimal,string,time]>` | Specifies the second value for comparison. |

### Return value

The `<=` function returns a boolean value indicating whether `oper1` is less than or equal to `oper2`.

### Examples

The following example demonstrates how to use the `<=` function to compare two integer values to check if the first value (`1`) is less than or equal to the second value (`3`):

```pact
pact> (<= 1 3)
true
```

The following example demonstrates how to use the `>=` function to compare two decimal values to check if the first value (`5.24`) is less than or equal to the second value (`2.52`):

```pact
pact> (<= 5.24 2.52)
false
```

The following example demonstrates how to use the `>=` function to compare two string values to check if the first value (`abc`) is less than or equal to the second value (`def`):

```pact
pact> (<= "abc" "def")
true
```
