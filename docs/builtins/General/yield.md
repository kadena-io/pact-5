## yield

Use `yield` to yield an object for use with the `resume` function in the subsequent Pact step. 
Optionally, you can specify a target chain for executing the next step using automated a simplified payment verification (spv) endorsement-based dispatch.

### Basic syntax

To yield an `object` for use with the `resume` function, use the following syntax:

```pact
(yield object [target-chain])
```

### Arguments

Use the following arguments to specify the object and, optionally, the target chain for executing the subsequent step using the `yield` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `object` | object | Specifies the object to be yielded for use with 'resume'. |
| `target-chain` | string | (Optional) Specifies the chain ID on which the subsequent step should execute. |

### Return value

The `yield` function returns the yielded object.

### Examples

The following examples demonstrate how to use the `yield` function in a Pact script. 

In the following example, the `yield` function creates an object with one key and value that can be passed to the `resume` function,

```pact
(yield { "amount": 100.0 })
```

Optionally, you can specify a target chain for resuming the transaction.
For example, to set the target chain to chain 8:

```pact
(yield { "amount": 100.0 } "8")
```

The following example illustrates using `yield` and `resume` functions in `defpact` steps:

```pact
  (defpact copy-account:string(account:string target:string)
    (step
      (with-capability (COPY_ACCOUNT account)
        (with-read guard-lookup-table account
          { 'webauthn-guard-name := guard-name }
          (webauthn-guard.copy-account guard-name target)

          (let ((yield-data:object{copy-account-schema} { 'guard-name : guard-name }))
            (yield yield-data target)
          )
        )
      )
    )

    (step
      (resume 
        { 'guard-name := guard-name }
        (continue (webauthn-guard.copy-account guard-name target))
        (write guard-lookup-table target
          { 'webauthn-guard-name : guard-name }
        )
        (coin.create-account (get-account-name guard-name) (get-account-guard guard-name))
      )
    )
  )
```