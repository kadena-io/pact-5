## and

Use `and` to perform a boolean logic AND operation with short-circuiting.

### Basic syntax

To perform a boolean logic AND operation between two boolean values `oper1` and `oper2`, use the following syntax:

```pact
(and oper1 oper2)
```

### Arguments

Use the following arguments to specify the boolean values for the `and` operation.

| Argument | Type | Description |
| --- | --- | --- |
| `oper1` | bool | Specifies the first boolean value for the AND operation. |
| `oper2` | bool | Specifies the second boolean value for the AND operation. |

### Return value

The `and` function returns a boolean value based on the result of the AND operation between the input boolean values.

### Examples

The following example demonstrates how to use the `and` function to perform a boolean AND operation between the values `true` and `false` in the Pact REPL:

```pact
pact> (and true false)
false
```

The following example illustrates using the `and` function to evaluate two expressions to determine whether an account string is valid:

```pact
(and
    (>= (length account) 3)
    (<= (length account) 256))
```

In this example, both expressions must evaluate to true for an account string to be valid.