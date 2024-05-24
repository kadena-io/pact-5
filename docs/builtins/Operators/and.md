## and
Use `and` to perform a boolean logic AND operation with short-circuiting.

### Basic syntax

To perform a boolean logic AND operation between two boolean values `x` and `y`, use the following syntax:

`(and x y)`

### Arguments

Use the following arguments to specify the boolean values for the `and` operation.

| Argument | Type | Description |
| --- | --- | --- |
| `x` | `bool` | Specifies the first boolean value for the AND operation. |
| `y` | `bool` | Specifies the second boolean value for the AND operation. |

### Return values

The `and` function returns a boolean value based on the result of the AND operation between the input boolean values.

### Examples

The following example demonstrates the `and` operation in the Pact REPL:

```pact
pact>(and true false)
false
```

In this example, the `and` function performs a boolean AND operation between the values `true` and `false`, resulting in `false`.
