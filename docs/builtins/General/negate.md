## negate

Use `negate` to negate a `value`

### Basic syntax

To negate `value`, use the following syntax:

```pact
(negate value)
```


### Arguments

Use the following arguments to specify the values for negation or subtraction using the `-` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `value` | integer or decimal | Specifies the value to be negated. |


### Return value

The `negate` function returns the negation of the specified `value`

### Examples

The following example demonstrates how to use the `negate` function to negate a value in a Pact REPL:

```pact
pact> (negate 1.0)
-1.0
```
