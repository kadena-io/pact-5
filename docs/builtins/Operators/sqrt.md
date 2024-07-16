## sqrt

Use `sqrt` to compute the square root of the given `value`.

### Basic syntax

To calculate the square root of a value, use the following syntax:

```pact
(sqrt value)
```

### Arguments

Use the following argument to specify the value for which to compute the square root using the `sqrt` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `value` | integer or decimal | Specifies the value that you want to compute the square root for. |

### Return value

The `sqrt` function returns the square root of the specified value. 
The return type depends on the type of the input value and whether the square root for the input value is a whole number.

### Examples

The following example demonstrates how to use the `sqrt` function to calculate the square root of the integer value 25 that returns an integer value:

```pact
pact> (sqrt 25)
5
```

The following example calculates the square root of the decimal value 144.0 that returns a decimal value:

```pact
(sqrt 144.0)
12.0
```

The following example calculates the square root for 48 and rounds the result to four decimal places:

```pact
(round (sqrt 48) 4)
6.9282
```

This example illustrates how to use the `sqrt` function to compute the square root of a value in Pact, producing either an `integer` or a `decimal` result.
