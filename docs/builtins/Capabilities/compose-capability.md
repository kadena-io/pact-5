## compose-capability

Use `compose-capability` to compose and grant capabilities in a nested structure to control the scope of how the capabilities are applied.
By convention, capabilities are defined using all uppercase letters.

With this function, you can define the specified `CAPABILITY` within the context of an outer `defcap` declaration.
The function is only valid within the distinct `defcap` body of its outer capability, such that the `CAPABILITY` you specify for the `compose-capability` function is included when you call the `with-capability` function for its outer capability. 

For example, if you call `(with-capability (OUTER-CAP) OUTER-BODY)` and the `OUTER-CAP` declaration includes the `(compose-capability (INNER-CAP))` function, the `INNER-CAP` capability is granted in the scope of the `OUTER-BODY` logic.

### Basic syntax

To compose and grant a specified `CAPABILITY` within an outer capability body, use the following syntax:

```pact
(compose-capability CAPABILITY)
```

### Arguments

Use the following argument to specify the `CAPABILITY` for the `compose-capability` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `CAPABILITY` | capability | Specifies the capability to include in the scope of an outer capability. |

### Return value

The `compose-capability` function returns a boolean value to indicate success or failure in requesting the grant of the specified `CAPABILITY`.

### Examples

The following example demonstrates how to use the `compose-capability` function within the body of the `TRANSFER` capability and include the DEBIT and CREDIT capabilities when the `with-capability` function is called:

```pact
(defcap TRANSFER:bool
    ( id:string
      sender:string
      receiver:string
      amount:decimal
    )
    @managed amount TRANSFER-mgr
    (enforce-unit id amount)
    (enforce (> amount 0.0) "Amount must be positive")
    (compose-capability (DEBIT id sender))
    (compose-capability (CREDIT id receiver))
)
...
    (with-capability (TRANSFER id sender receiver amount)
     ...
    )
```
