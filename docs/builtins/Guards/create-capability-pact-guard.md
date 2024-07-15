## create-capability-pact-guard

Use `create-capability-pact-guard` to create a predicate function that ensures that specific conditions are true and can be enforced to grant the specified `CAPABILITY` for steps defined in a `defpact` multi-step transaction.

By convention, capabilities are defined using all uppercase letters.

### Basic syntax

To create a predicate function that guards the specified `CAPABILITY` in a `defpact` multi-step transaction, use the following syntax:

```pact
(create-capability-pact-guard CAPABILITY)
```

### Arguments

Use the following argument to specify the `CAPABILITY` for the `create-capability-pact-guard` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `CAPABILITY` | capability | Specifies the capability that the predicate function guards. |

### Return values

The `create-capability-pact-guard` function returns a guard that enables the code associated with the specified `CAPABILITY` to be executed in the context of a `defpact` multi-step transaction.

### Examples

The following example demonstrates how to use the `create-capability-pact-guard` function to create a guard for the `ESCROW owner` capability in a`defpact` step:

```pact
(create-capability-pact-guard (ESCROW owner))
```

The following example creates a guard for the `SALE_PRIVATE` capability associated with the `pact-id` for the sales contract being executed:

```pact
  (defun offer:bool
    ( id:string
      seller:string
      amount:decimal
    )
    @doc "Initiate sale with by SELLER by escrowing AMOUNT of TOKEN until TIMEOUT."
    @model
      [ (property (!= id ""))
        (property (!= seller ""))
        (property (>= amount 0.0))
      ]
    (require-capability (SALE_PRIVATE (pact-id)))
    (let
      (
        (sender (debit id seller amount))
        (receiver (credit id (sale-account) (create-capability-pact-guard (SALE_PRIVATE (pact-id))) amount))
      )
      (emit-event (TRANSFER id seller (sale-account) amount))
      (emit-event (RECONCILE id amount sender receiver)))
  )

```