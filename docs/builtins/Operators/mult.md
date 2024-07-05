## multiply (*)

Use `*` to multiply the first `oper1` argument by the second `oper12` argument.
Note that you can use this function to multiply integer values or decimal values.
However, you should use the same type for both `oper1` and `oper2` values.

### Basic syntax

To multiply `oper1` by `oper2`, use the following syntax:

```pact
(* oper1 oper2)
```

### Arguments

Use the following arguments to specify the values for multiplication using the `*` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `oper1` | integer or decimal | Specifies the first multiplier. |
| `oper2` | integer or decimal | Specifies the second multiplier. |

### Return value

The `*` function returns the result of multiplying `oper1` by `oper2`.

### Examples

The following example demonstrates how to use the `*` function to multiply two decimal values:

```pact
pact> (* 0.5 10.0)
5.0
```

The following example demonstrates how to use the `*` function to multiply two integer values:

```pact
pact> (* 3 5)
15
```

These examples illustrate how to use the `*` function to perform multiplication operations in Pact, facilitating arithmetic calculations with both integer and decimal values.
