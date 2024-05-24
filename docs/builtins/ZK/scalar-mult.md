## scalar-mult
The `scalar-mult` function multiplies a point that lies on the curve BN254 by an `integer` value.

### Basic syntax

To multiply a point by an `integer` value, use the following syntax:

`(scalar-mult TYPE POINT1 SCALAR)`

### Arguments

Use the following arguments to specify the `TYPE`, `POINT`, and `SCALAR` value for multiplication using the `scalar-mult` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `TYPE` | `string` | Specifies the type of point (e.g., `'g1`). |
| `POINT1` | `<a>` | Specifies the point on the curve BN254 to be multiplied. |
| `SCALAR` | `integer` | Specifies the `integer` value to multiply the point by. |

### Return value

The `scalar-mult` function returns the result of multiplying the specified point by the `integer` scalar value.

### Examples

The following example demonstrates the usage of the `scalar-mult` function within a Pact script. It multiplies the point `{ 'x: 1, 'y: 2 }` on curve `'g1` by the scalar value `2`:

```pact
pact>(scalar-mult 'g1 {'x: 1, 'y: 2} 2)
{"x":1368015179489954701390400359078579693043519447331113978918064868415326638035, "y":9918110051302171585080402603319702774565515993150576347155970296011118125764}
```

This example illustrates how to use the `scalar-mult` function to perform scalar multiplication on points lying on the curve BN254 in Pact.
