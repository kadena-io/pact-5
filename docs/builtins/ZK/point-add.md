## point-add

Use `point-add` to add two points together that lie on the in the Barreto-Naehrig (BN254) elliptic curve. 
The BN254 curve is a pairing-friendly curve tht can be used for verifying on-chain zero knowledge proof schemes such as Groth16 and PlonK.
You can use this function to add point either in Fq or in Fq2.

### Basic syntax

To add two points together that lie on the Barreto-Naehrig (BN254) elliptic curve, use the following syntax:

```pact
(point-add type point1 point2)
```

### Arguments

Use the following arguments to specify the type of addition and the points to be added using the `point-add` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `type` | string | Specifies the type of point addition to perform. The valid values are "g1" and "g2". |
| `point1` | any | Specifies the first point to be added. |
| `point2` | any | Specifies the second point to be added. |

### Return value

The `point-add` function returns the result of adding the specified points together.

### Examples

The following example demonstrates how to use the `point-add` function to add the two specified points together on the BN256 curve in the Pact REPL:

```pact
pact> (point-add 'g1 {'x': 1, 'y': 2} {'x': 1, 'y': 2})
{"x": 1368015179489954701390400359078579693043519447331113978918064868415326638035
,"y": 9918110051302171585080402603319702774565515993150576347155970296011118125764}
```

In this example, the `type` of addition is `g1` and the points are provided as objects with `x` and `y` coordinates.
