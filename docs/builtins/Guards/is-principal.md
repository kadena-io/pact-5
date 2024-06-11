## is-principal

Use `is-principal` to determine whether a principal string conforms to the principal format *without* proving its validity.

### Basic syntax

To check whether a `principal` string conforms to the principal format, use the following syntax:

```pact
(is-principal principal)
```

### Arguments

Use the following argument to specify the `principal` string you want to check using the `is-principal` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `principal` | string | Specifies the principal string to be checked. |

### Return value

The `is-principal` function returns a boolean value indicating whether the specified `principal` string conforms to the principal format.

### Examples

The following example demonstrates how to use the `is-principal` function in the Pact REPL to check whether the specified string conforms to the principal format:

```pact
pact> (is-principal "k:58705e8699678bd15bbda2cf40fa236694895db614aafc82cf1c06c014ca963c")
true
```

The following example demonstrates how to use the `is-principal` function in an `enforce` statement:

```pact
(defcap LOCK_DEPOSIT(sender:string)
   (enforce (is-principal sender) "Sender must be a principal account")
)
```

In this example, the `is-principal` function ensures that the `sender` account conforms to the format for principal accounts.
If the format is valid, the `enforce` statement returns true and the LOCK_DEPOSIT capability is granted.
If the `sender` isn't a valid principal format, the enforce statement returns the "Sender must be a principal account" error message.


The following example checks the format of a principal account associated with a capability guard in an `enforce` statement:

```pact
(enforce (is-principal "c:bF51UeSqhrSjEET1yUWBYabDTfujlAZke4R70I4rrH") "Invalid account structure: non-principal account")
<interactive>:0:0:Error: Invalid account structure: non-principal account
```

In this example, the length of the specified string doesn't conform to the format for a principal account, so the `enforce` statement returns the error message. 