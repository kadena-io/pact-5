## install-capability

Use `install-capability` to provision a specific, previously-defined capability as the context for performing contract operations.
For example, the `install-capability` function is frequently used set the capability state with appropriate permissions before performing operations that involve an escrow type of account.

You should note that you should only specify managed capabilities when you call the `install-capability` function.
Managed capabilities rely on management functions or one-time-use rules to set limits on how the capability is used.

After a capability is installed, the permissions associated with the specified capability are available but the capability must still be brought into scope using the `with-capability` function.

For more information about defining and using managed capabilities, management functions, and the `with-capability` function, see [Managed capabilities](/smart-contracts/capabilities#managed-capabilities).

By convention, capabilities are defined using all uppercase letters.

### Basic syntax

To provision a specific managed capability, use the following syntax:

```pact
(install-capability CAPABILITY)
```

### Arguments

Use the following argument to specify the capability you want to install using the `install-capability` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `CAPABILITY` | any | Specifies the name of the capability to be installed. |

### Return value

The `install-capability` function returns a boolean value indicating the success or failure of the installation, along with a string message providing additional information.

### Examples

The following example demonstrates how to use the `install-capability` to install a capability named `coin.TRANSFER` with specified parameters to authorize a `transfer` operation from an escrow account:

```pact
(defun escrow-admin (escrow:string receiver:string amount:decimal) 
  (install-capability (coin.TRANSFER escrow receiver amount)) 
    (with-capability (PAY-AUTHORIZATION)
    (coin.transfer escrow receiver amount)))
```

The following example illustrates the definition for the `TRANSFER` managed capability with the `TRANFER-mgr` management function, and the `amount` managed resource parameter:

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

  (defun TRANSFER-mgr:decimal
    ( managed:decimal
      requested:decimal
    )
    (let ((newbal (- managed requested)))
      (enforce (>= newbal 0.0)
        (format "TRANSFER exceeded for balance {}" [managed]))
      newbal)
  )
```

The following example demonstrates the use of `install-capability` in the Pact REPL to install a capability named `PAY` with specified parameters:

```pact
pact> (install-capability (PAY "alice" "bob" 10.0))
```

If the `PAY` capability is installed successfully, the function returns a boolean value indicating success.
If the capability isn't installed, the function returns an error message indicating the reason for failure.

The following example demonstrates using the `install-capability` function in an internal contract function.

```pact
...

(defcap USER-PROOF (userId:string questId:string)
  (with-read users (user-key userId questId)
    { 'guard := g }
  (enforce-keyset g)))

(defcap CLAIMABLE (userId:string questId:string)
  @doc "Tests the guard through USER-PROOF"
  (compose-capability (USER-PROOF userId questId))
  (compose-capability (CLAIM)))

...

(defcap CLAIM () true)
(defconst CLAIM-GUARD (create-capability-guard (CLAIM)))
(defconst CLAIM-ACCOUNT (create-principal CLAIM-GUARD))

...

; Internal function

(defun claim:string (userId:string questId:string)
  @doc "Claims rewards for a user"

  ;; Validates if there is enough reward to pay out
  (validate-winning userId questId)

  (with-read users (user-key userId questId) {'reward:= r, 'guard:= g }

  (with-capability (CLAIMABLE userId questId)
    (install-capability (coin.TRANSFER CLAIM-ACCOUNT userId r))
    (coin.transfer-create CLAIM-ACCOUNT userId g r)
    (increment-user-stats userId questId r))
    ;; remove claimed funds from global funds, protected with an internal capability
    (with-capability (DEBIT-GLOBAL)
    (global-funds-debit r))

    (update users (user-key userId questId) { 'reward: 0.0, 'claimed: true })

    (emit-event (CLAIM-INFO userId questId r))
    (format "{} claimed {} rewards from quest {}" [userId r questId])))
```
