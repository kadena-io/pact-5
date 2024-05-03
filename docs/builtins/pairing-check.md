Use `pairing-check` to perform pairing and final exponentiation on points in G1 and G2 in BN254, and check if the result is 1.

### Basic syntax

To perform pairing and final exponentiation on points in G1 and G2, and check if the result is 1, use the following syntax:

pairing-check *points-g1 points-g2*

### Arguments

Use the following arguments to specify the lists of points in G1 and G2 for which you want to perform the pairing check using the `pairing-check` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| points-g1 | [<a>] | Specifies the list of points in G1. |
| points-g2 | [<b>] | Specifies the list of points in G2. |

### Return value

The `pairing-check` function returns a boolean value indicating whether the result of the pairing and final exponentiation is 1.

### Examples

The following example demonstrates the use of `pairing-check` in the Pact REPL:

```lisp
pact>(pairing-check [point1_g1 point2_g1] [point1_g2 point2_g2])
```

In this example, `pairing-check` is used to perform pairing and final exponentiation on the specified points in G1 and G2. The function returns a boolean value indicating whether the result is 1.
