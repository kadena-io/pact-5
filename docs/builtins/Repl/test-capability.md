## test-capability

Use `test-capability` to acquire or install the `capability` specified.
You can use this function to acquire any capability that's not managed or to install any managed capability.
The specified capability and any composed capabilities it encompasses are in scope for the rest of the transaction.

### Basic syntax

To acquire or install the `capability` specified, use the following syntax:

```pact
(test-capability (capability))
```

## Arguments

Use the following argument when using the `test-capabilities` Pact function.

| Argument | Type | Description |
|----------|------|------------ |
| `capability` | capability-token | Specifies the capability and scope to test. |

### Return value

The `test-capability` function returns a string that indicates whether the capability has been installed or acquired.

### Example

The following example demonstrates how to use `test-capability` to acquire a capability for the scope of a REPL transaction:

```pact
pact> (module m g (defcap g () true))
"Loaded module m, hash c5kiBbxH0zDIMPpAlJdXwMNsNiwnZY4YRzkJvvagn1c"
pact> (test-capability (g))
"Capability acquired"
```

In this example, the capability isn't a managed capability and doesn't require any arguments.
Managed capabilities define a resource that the capability controls access to and a management function that modifies the resource.
For example, you might define a managed capability and management function similar to the following:

```pact
   (defcap PAY (sender:string receiver:string amount:decimal)
     @managed amount manage-PAY
     (compose-capability (USER_GUARD sender)))
 
   (defun manage-PAY (mgd recd)
     (let ((bal:decimal (- mgd recd)))
       (enforce (>= bal 0.0) "insufficient balance")
       bal))
 
   (defun pay (sender:string receiver:string amount:decimal)
     (with-capability (PAY sender receiver amount) (transfer sender receiver amount))
   )
```

For this example, the `PAY` managed capability requires the `sender`, `receiver`, and `amount` arguments:

```pact
(begin-tx "Set capability")
   (test-capability (cap-role.PAY "Alice" "Bob" 3.0))
(commit-tx)
```