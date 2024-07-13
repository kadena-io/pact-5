## not-equal (!=)

Use `!=` to return true if the first `oper1` argument does not equal the second `oper2` argument.
This function allows you to write conditional logic based on whether two values are not equal.

### Basic syntax

To check if `oper1` does not equal `oper2`, use the following syntax:

```pact
(!= oper1 oper2)
```

### Arguments

Use the following arguments to specify the values for comparison using the `!=` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `oper1` | integer, string, time, decimal, bool, list, object, keyset, guard, or module | Specifies the first value for comparison. |
| `oper2` | integer, string, time, decimal, bool, list, object, keyset, guard, or module | Specifies the second value for comparison. |

### Return value

The `!=` function returns true if `oper1` does not equal `oper2`, otherwise false.

### Examples

The following example demonstrates how to use the `!=` function to check whether two strings are not equal:

```pact
pact> (!= "hello" "goodbye")
true
```

In the following example, the `!=` function ensures that the `sender` and `receiver` accounts are not equal to prevent the sender initiating a transfer from also being the receiver of the transfer:

```pact
(enforce (!= sender receiver) "sender cannot be the receiver of a transfer")
```