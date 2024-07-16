## subtract (-)

Use `-` to negate a `value` or to subtract `oper2` from `oper1`.

### Basic syntax

To negate `value`, use the following syntax:

```pact
(- value)
```

To subtract `oper2` from `oper1`, use the following syntax:

```pact
(- oper1 oper2)
```

### Arguments

Use the following arguments to specify the values for negation or subtraction using the `-` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `value` | integer or decimal | Specifies the value to be negated. |
| `oper1` | integer or decimal | Specifies the value to be subtracted from. |
| `oper2` | integer or decimal | Specifies the value to subtract from `oper1`. |

### Return value

The `-` function returns the negation of the specified `value`, or the result of subtracting `oper2` from `oper1`.

### Examples

The following example demonstrates how to use the `-` function to negate a value in a Pact REPL:

```pact
pact> (- 1.0)
-1.0
```

The following example demonstrates how to use the `-` function to subtract integer values in a Pact REPL:

```pact
pact> (- 3 2)
1
```
