## point-add
Use `point-add` to add two points together that lie on the curve BN254. Point addition can be performed either in Fq or in Fq2.

### Basic syntax

To add two points together that lie on the curve BN254, use the following syntax:

point-add *type point1 point2*

### Arguments

Use the following arguments to specify the type of addition and the points to be added using the `point-add` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| type | string | Specifies the type of point addition (either 'g1' or 'g2'). |
| point1 | <a> | Specifies the first point to be added. |
| point2 | <a> | Specifies the second point to be added. |

### Return value

The `point-add` function returns the result of adding the specified points together.

### Examples

The following example demonstrates the use of `point-add` in the Pact REPL:

```pact
pact>(point-add 'g1 {'x': 1, 'y': 2} {'x': 1, 'y': 2})
```

In this example, `point-add` is used to add the two specified points together on the curve BN254. The type of addition is 'g1', and the points are provided as dictionaries with 'x' and 'y' coordinates.
