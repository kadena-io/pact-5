## pairing-check

Use `pairing-check` to perform pairing and final exponentiation on points in `points-g1` and `points-g2` in the Barreto-Naehrig (BN254) elliptic curve, and check if the result is 1.

### Basic syntax

To perform pairing and final exponentiation on points in `points-g1` and `points-g2`, and check if the result is 1, use the following syntax:

```pact
(pairing-check points-g1 points-g2)
```

### Arguments

Use the following arguments to specify the lists of points in G1 and G2 for which you want to perform the pairing check using the `pairing-check` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `points-g1` | [any] | Specifies the list of points in G1. |
| `points-g2` | [any] | Specifies the list of points in G2. |

### Return value

The `pairing-check` function returns a boolean value indicating whether the result of the pairing and final exponentiation is 1.

### Examples

The following example demonstrates how to use the `pairing-check` function in the Pact REPL:

```pact
pact> (pairing-check [point1_g1 point2_g1] [point1_g2 point2_g2])
```
