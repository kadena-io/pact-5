## create-capability-guard

Use `create-capability-guard` to create a predicate function that ensures that specific conditions are true and can be enforced to grant the specified `CAPABILITY`.

By convention, capabilities are defined using all uppercase letters.

### Basic syntax

To create a predicate function that guards the specified `CAPABILITY`, use the following syntax:

```pact
(create-capability-guard CAPABILITY)
```

### Arguments

Use the following argument to specify the `CAPABILITY` for the `create-capability-guard` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `CAPABILITY` | capability | Specifies the capability that the predicate function guards. |

### Return values

The `create-capability-guard` function returns a guard that enforces the acquisition of the specified `CAPABILITY`.

### Examples

The following example demonstrates how to use the `create-capability-guard` function to create a guard for the GOVERNANCE capability:

```pact
(defun create-gas-payer-guard:guard()
    (create-capability-guard (GOVERNANCE))
)
```

The conditions specified for the GOVERNANCE capability must evaluate to true for the capability to be acquired and related code where the capability is required to be executed.

The following example illustrates how to create a guard for an ESCROW_MANAGEMENT account:

```pact
(defconst ESCROW_ID
    (create-principal
      (create-capability-guard (ESCROW_MANAGEMENT))
    )
    "The escrow will hold all KDA in circulation on the chain"
)
```
