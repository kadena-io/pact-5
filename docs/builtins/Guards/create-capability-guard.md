## create-capability-guard
Use `create-capability-guard` to create a guard that will enforce that a specified `CAPABILITY` is acquired.

### Basic syntax

To create a guard that enforces the acquisition of a `CAPABILITY`, use the following syntax:

`(create-capability-guard CAPABILITY)`

### Arguments

Use the following argument to specify the `CAPABILITY` for the `create-capability-guard` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `CAPABILITY` | `capability` | Specifies the capability that the guard will enforce acquisition for. |

### Return values

The `create-capability-guard` function returns a guard that enforces the acquisition of the specified `CAPABILITY`.

### Examples

The following example demonstrates the `create-capability-guard` function:

```lisp
(create-capability-guard (BANK_DEBIT 10.0))
```

In this example, `(create-capability-guard (BANK_DEBIT 10.0))` is used to create a guard that enforces the acquisition of the `BANK_DEBIT 10.0` capability. This guard can then be used to enforce the presence of this capability in Pact code.
