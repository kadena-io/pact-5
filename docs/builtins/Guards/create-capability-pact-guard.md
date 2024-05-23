## create-capability-pact-guard
Use `create-capability-pact-guard` to create a guard that enforces that a specified `CAPABILITY` is acquired and that the currently-executing `defpact` is operational.

### Basic syntax

To create a guard that enforces the acquisition of a `CAPABILITY` and checks for an operational `defpact`, use the following syntax:

`(create-capability-pact-guard CAPABILITY)`

### Arguments

Use the following argument to specify the `CAPABILITY` for the `create-capability-pact-guard` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `CAPABILITY` | `capability` | Specifies the capability that the guard will enforce acquisition for. |

### Return values

The `create-capability-pact-guard` function returns a guard that enforces the acquisition of the specified `CAPABILITY` and checks for an operational `defpact`.

### Examples

The following example demonstrates the `create-capability-pact-guard` function:

```lisp
(create-capability-pact-guard (ESCROW owner))
```

In this example, `(create-capability-pact-guard (ESCROW owner))` is used to create a guard that enforces the acquisition of the `ESCROW owner` capability and checks for an operational `defpact`. This guard ensures that the `ESCROW owner` capability is acquired and that the current `defpact` is operational before proceeding with Pact code execution.
