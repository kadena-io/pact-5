## install-capability

Use `install-capability` to specify and provision a managed capability. 
Managed capabilities are defined in `defcap` declarations that include the `@managed` keyword. 
The `@managed` keyword can be used two ways:

- To specify a specific **resource** using a single parameter to be managed by a specific **management function**.
- To specify a restricted permission for an operation that can only be performed once.

The `install-capability` function is most commonly used to perform contract operations that are not called directly by contract users.
For example, the `install-capability` function is frequently used for operations involving an escrow type of account to grant the permissions required to perform restricted operations.

After a capability is installed, it must be brought into scope using the `with-capability` function.
When the installed managed capability is brought into scope, its management function is invoked to validate the request.

The management function takes the type of the managed parameter, executes the logic required to validate the requested capability or perform the managed operation, and returns the new managed value that results from the request.

The type signature for the management function is `managed:<type> requested:<type> -> <type>`, where `<type>` indicates the type of the managed parameter. 
For example, assume you define a managed capability as: 

```pact
(defcap FOO (bar:string baz:integer) @managed baz FOO-mgr ...)
```

The management function for this capability would be:

```pact
(defun FOO-mgr:integer (managed:integer requested:integer) ...)
``` 

Any capability that has static unmanaged parameters will invoke the management function with the current managed value and that of the requested capability. 
The function should perform whatever logic, presumably linear, to validate the request, and return the new managed value representing the `balance` of the request.

Note that signatures scoped to a managed capability cause the capability to be automatically provisioned in a manner similar to how capabilities are installed with this function.

By convention, capabilities are defined using all uppercase letters.

### Basic syntax

To specify and provision a managed capability, use the following syntax:

```pact
(install-capability CAPABILITY)
```

### Arguments

Use the following argument to specify the capability you want to install using the `install-capability` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `CAPABILITY` | any | Specifies the capability to be installed. |

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
