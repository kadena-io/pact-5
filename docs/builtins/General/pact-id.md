## pact-id

Use `pact-id` to return the identifier associated with the execution of a `defpact` step if called during the execution of a current multi-step transaction, failing if not.

### Basic syntax

To return the identifier for a pact execution, use the following syntax:

```pact
(pact-id)
```

### Return value

The `pact-id` function returns a string representing the identifier for the current pact step being executed.

### Examples

The `pact-id` function is called without any arguments. 
The following examples illustrates how to use the `pact-id` function i a `defpact` definition:

```pact
(defpact sale:string
    ( id:string
      seller:string
      amount:decimal
      timeout:integer
    )
    (step-with-rollback
      ;; Step 0: offer
      (let ((token-info (get-token-info id)))
        (with-capability (OFFER-CALL id seller amount timeout (pact-id))
          (marmalade-v2.policy-manager.enforce-offer token-info seller amount timeout (pact-id)))
        (with-capability (SALE id seller amount timeout (pact-id))
          (offer id seller amount))
        (pact-id)
      )
      ;;Step 0, rollback: withdraw
      (let ((token-info (get-token-info id)))
        (with-capability (WITHDRAW-CALL id seller amount timeout (pact-id))
          (marmalade-v2.policy-manager.enforce-withdraw token-info seller amount timeout (pact-id)))
        (with-capability (WITHDRAW id seller amount timeout (pact-id))
          (withdraw id seller amount))
        (pact-id)
      )
    )
```
