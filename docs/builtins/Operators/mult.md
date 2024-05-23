## *

The `*` function multiplies the first argument `x` by the second argument `y`.

### Basic syntax

To multiply `x` by `y`, use the following syntax:

`(* x y)`

### Arguments

Use the following arguments to specify the values for multiplication using the `*` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `x` | `<a[integer,decimal]>` | Specifies the first multiplier. |
| `y` | `<a[integer,decimal]>` | Specifies the second multiplier. |

### Return value

The `*` function returns the result of multiplying `x` by `y`.

### Examples

The following examples demonstrate the usage of the `*` function within a Pact script. They perform multiplication:

```lisp
pact>(* 0.5 10.0)
5.0
```

```lisp
pact>(* 3 5)
15
```

These examples illustrate how to use the `*` function to perform multiplication operations in Pact, facilitating arithmetic calculations with both integer and decimal values.
