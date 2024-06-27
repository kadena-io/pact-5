## validate-principal

Use `validate-principal` to validate that a principal unambiguously identifies a specified guard.

### Basic syntax

To validate a `principal` against `guard`, use the following syntax:

`(validate-principal guard principal)`

### Arguments

Use the following arguments to specify the guard and the principal that you want to validate using the `validate-principal` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `guard` | guard | Specifies the guard to validate against. |
| `principal` | string | Specifies the principal to be validated. |

### Return value

The `validate-principal` function returns a boolean value indicating whether the provided principal unambiguously identifies the specified guard.

### Examples

The following example demonstrates how to use the `validate-principal` function to ensure that the principal obtained from reading a keyset matches the specified principal account:

```pact
(enforce (validate-principal (read-keyset 'keyset) account) "Invalid account ID")
```

The following example uses `validate-principal` in a function to ensure the merchant and buyer accounts are guarded principal account:

```pact
  (defun create-order (
    order-id       : string
    merchant       : string
    merchant-guard : guard
    buyer          : string
    buyer-guard    : guard
    order-price    : decimal
  )
    @doc "Creates an order in the order table and reserves funds to the escrow account"
    (enforce (!= order-id "") "Order id can not be empty")
    (enforce (< 0.0 order-price) "Order price is not a positive number")

    (enforce (validate-principal merchant-guard merchant) "Invalid merchant guard")
    (enforce (validate-principal buyer-guard buyer) "Invalid buyer guard")

    (with-capability (CREATE_ORDER order-id)
      (enforce-order-lines
        order-id
        buyer
        buyer-guard
        merchant
        merchant-guard
        order-price
      )
      (insert order-table order-id 
        { 'order-status   : CREATED
        , 'merchant       : merchant
        , 'merchant-guard : merchant-guard
        , 'buyer          : buyer
        , 'buyer-guard    : buyer-guard
        , 'order-price    : order-price
        }
      )
      (reserve-funds order-id buyer (+ order-price))
      (reserve-funds order-id merchant (get-merchant-deposit-amount order-price))
    )
  )
```
