
Use `install-capability` to specify and provision a managed capability. 
Managed capabilities are defined in `defcap` declarations that include the `@managed` keyword. The `@managed` keyword designates a single parameter to be managed by a specified management function.
After a capability is installed, it must still be brought into scope using the `with-capability` function.
When the capability is brought into scope, its management function is invoked to validate the request.

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

## Basic syntax

To specify and provision a managed capability, use the following syntax:

```pact
(install-capability CAPABILITY)
```

## Arguments

Use the following argument to specify the capability you want to install using the `install-capability` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `CAPABILITY` | any | Specifies the capability to be installed. |

## Return value

The `install-capability` function returns a boolean value indicating the success or failure of the installation, along with a string message providing additional information.

## Examples

The following example demonstrates how to use the `install-capability` to install a capability named `coin.TRANSFER` with specified parameters:

```pact
(install-capability (coin.TRANSFER ESCROW_ID merchant merchant-payout))
```

The following example illustrates the definition for a capability with a management function and a managed parameter.

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