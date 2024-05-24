## &

The `&` function computes the bitwise AND operation between the first argument `x` and the second argument `y`.

### Basic syntax

To compute the bitwise AND operation between `x` and `y`, use the following syntax:

`(& x y)`

### Arguments

Use the following arguments to specify the values for bitwise AND operation using the `&` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `x` | `integer` | Specifies the first operand. |
| `y` | `integer` | Specifies the second operand. |

### Return value

The `&` function returns the result of the bitwise AND operation between `x` and `y`.

### Examples

The following examples demonstrate the usage of the `&` function within a Pact REPL. They perform bitwise AND operations:

```pact
pact>(& 2 3)
2
```

```pact
pact>(& 5 -7)
1
```

These examples illustrate how to use the `&` function to compute bitwise AND operations in Pact, facilitating bitwise manipulation of integer values.
