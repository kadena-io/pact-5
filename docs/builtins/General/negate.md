## negate

Use `negate` to negate a specified integer or decimal value.
Note that the integer can be a positive or negative value.

### Basic syntax

To negate a specified `value`, use the following syntax:

```pact
(negate value)
```

### Arguments

Use the following argument to specify the value for negation.

| Argument | Type | Description |
| --- | --- | --- |
| `value` | integer or decimal | Specifies the value to be negated. |

### Return value

The `negate` function returns the negation of the specified `value`.

### Examples

The following example demonstrates how to use the `negate` function to negate a positive decimal value in a Pact REPL:

```pact
pact> (negate 1.0)
-1.0
```

The following example demonstrates how to use the `negate` function to negate a negative decimal value in a Pact REPL:

```pact
pact> (negate -3.0)
3.0
```