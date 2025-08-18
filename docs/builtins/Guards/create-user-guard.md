## create-user-guard

Use `create-user-guard` to define a custom guard function identified as the `closure` expression with arguments that are evaluated when the guard is defined and supplied to the `closure` function when the guard is enforced.

Beginning with Pact 5.3, the `create-user-guard` function can access user tables in read-only mode.

### Basic syntax

To define a custom guard `closure` for use in Pact, use the following syntax:

```pact
(create-user-guard closure)
```

### Arguments

Use the following argument to specify the `closure` for the `create-user-guard` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `closure` | closure | Specifies the custom guard closure to define. The closure is a function that takes no arguments and returns a boolean value. |

### Return value

The `create-user-guard` function returns a guard that uses the specified custom `closure` function that returns a boolean value.

### Example

The following example demonstrates how to use the `create-user-guard` function to obtain a keyset, then use the keyset as a custom guard closure function:

```pact
(create-user-guard (read-keyset 'my-keyset))
```

In this example, `(read-keyset 'my-keyset)` is used as the closure function to capture a keyset to use when the user guard is enforced. 
This code allows you to define custom user guards based on specific keysets or conditions.

The following example defines a user guard for the `enforce-fungible-transfer` function that must evaluate to true to allow a fungible transfer:

```pact
(defun escrow-guard(sale-id:string)
   (util.guards1.guard-any [
      (create-capability-guard (REFUND_CAP sale-id))
      (create-user-guard (enforce-fungible-transfer sale-id))
   ])
)

(defun enforce-fungible-transfer:bool (sale-id:string)
   (require-capability (FUNGIBLE-TRANSFER-CALL sale-id) )
)
```