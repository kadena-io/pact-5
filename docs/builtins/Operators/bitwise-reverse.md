## bitwise-reverse (~)

Use `~` to reverse all bits in the provided integer.

### Basic syntax

To reverse all bits in an integer `x`, use the following syntax:

```pact
(~ x)
```

### Arguments

Use the following argument to specify the integer for bit reversal using the `~` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `x` | integer | Specifies the integer for which to reverse all bits. |

### Return value

The `~` function returns the result of reversing all bits in the provided integer.

### Examples

The following example demonstrates how to use the `~` function to reverse all bits in the integer `15`:

```pact
pact> (~ 15)
-16
```
