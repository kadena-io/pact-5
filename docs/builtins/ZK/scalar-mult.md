## scalar-mult

Use `scalar-mult` to multiply a point that lies on the BN254 by a specified `scalar` integer value.

### Basic syntax

To multiply a point by a specified `scalar` integer value, use the following syntax:

```pact
(scalar-mult type point1 scalar)
```

### Arguments

Use the following arguments to specify the `type`, `point`, and `scalar` value for multiplication using the `scalar-mult` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `type` | string | Specifies the type of point to multiply. The valid values are  "g1" and "g2". |
| `point1` | point | Specifies the point on the BN254 curve to be multiplied. |
| `scalar` | integer | Specifies the integer value to multiply the point by. |

### Return value

The `scalar-mult` function returns the result of multiplying the specified point by the `scalar` value.

### Examples

The following example demonstrates how to use the `scalar-mult` function to multiply the point `{ 'x: 1, 'y: 2 }` on curve `'g1` by the scalar value `3`:

```pact
pact> (scalar-mult 'g1 {'x: 1, 'y: 2} 3)
{"x": 3353031288059533942658390886683067124040920775575537747144343083137631628272
,"y": 19321533766552368860946552437480515441416830039777911637913418824951667761761}
```
