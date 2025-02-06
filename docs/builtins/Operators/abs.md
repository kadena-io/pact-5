## abs

Use `abs` to calculate the absolute value of a given `number`.

### Basic syntax

To calculate the absolute value of a `number`, use the following syntax:

```pact
(abs number)
```

### Arguments

Use the following argument to specify the `number` for which you want to calculate the absolute value using the `abs` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `number` | decimal or integer | Specifies the number for which to calculate the absolute value. |

### Return value

The `abs` function returns the absolute value of the `number` as a `decimal` or `integer`, depending on the input type.

### Examples

The following example calculates the absolute value of a decimal number in the Pact REPL:

```pact
pact> (abs (- 10.5 23.7))
13.2
```

The following example calculates the absolute value of an integer:

```pact
pact> (abs (- 10 23))
13
```
