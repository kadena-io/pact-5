## continue

Use `continue` to continue a previously-started multi-step transaction.
Transactions that have multiple steps executed in a sequence are called pacts and are defined using the `defpact` keyword.
Steps can be nested in `defpact` structures and the `continue` function enables you to continue execution with a specified value.

### Basic syntax

To continue a previously-started `defpact` transaction, use the following syntax:

```pact
(continue value)
```

### Arguments

Use the following argument to specify the `value` to continue the nested `defpact`.

| Argument | Type | Description |
| --- | --- | --- |
| `value` | `any` | Specifies the value to continue the nested `defpact`. |

### Return values

The `continue` function continues the execution of the nested `defpact` with the specified `value`.

### Examples

The following example demonstrates the use of `continue` within the context of a `defpact` to resume its execution with a specified value.

```pact
  (defpact transfer-crosschain:string
    ( sender:string
      receiver:string
      receiver-guard:guard
      target-chain:string
      amount:decimal
    )
    (step 
      (with-capability (TRANSFER_XCHAIN sender receiver amount target-chain)
        (install-capability (coin.TRANSFER sender receiver amount))
        (coin.transfer-crosschain sender receiver receiver-guard target-chain amount)
      )
    )
    (step
      (continue (coin.transfer-crosschain sender receiver receiver-guard target-chain amount))
    )
  )
```
