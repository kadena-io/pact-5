## scalar-mult
The `scalar-mult` function multiplies a point that lies on the curve BN254 by an integer value.

### Basic syntax

To multiply a point by an integer value, use the following syntax:

scalar-mult *type* *point1* *scalar*

### Arguments

Use the following arguments to specify the type, point, and scalar value for multiplication using the `scalar-mult` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| type | string | Specifies the type of point (e.g., `'g1`). |
| point1 | \<a> | Specifies the point on the curve BN254 to be multiplied. |
| scalar | integer | Specifies the integer value to multiply the point by. |

### Return value

The `scalar-mult` function returns the result of multiplying the specified point by the integer scalar value.

### Examples

The following example demonstrates the usage of the `scalar-mult` function within a Pact script. It multiplies the point `{ 'x: 1, 'y: 2 }` on curve `'g1` by the scalar value `2`:

```lisp
(scalar-mult 'g1 {'x: 1, 'y: 2} 2)
```

This example illustrates how to use the `scalar-mult` function to perform scalar multiplication on points lying on the curve BN254 in Pact.
