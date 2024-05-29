(namespace (read-msg 'ns))

(module soul-bound-policy-v1 GOVERNANCE

  (defconst ADMIN-KS:string "marmalade-examples.soul-bound-policy")

  (defcap GOVERNANCE ()
    (enforce-guard ADMIN-KS))

  (defconst POLICY:string (format "{}" [soul-bound-policy-v1]))

  (implements kip.token-policy-v2)
  (use kip.token-policy-v2 [token-info])
  (use marmalade-v2.policy-manager)
  (use marmalade-v2.guard-policy-v1 [MINT-GUARD-MSG-KEY BURN-GUARD-MSG-KEY GUARD_SUCCESS])

  (defschema mint-record 
    account:string
  )

  (deftable records:{mint-record})

  (defun has-guard-policy:bool (policies)
    (> (length (filter (lambda (policy) (= policy marmalade-v2.guard-policy-v1)) policies)) 0))

  (defun enforce-init:bool
    ( token:object{token-info}
    )
    @doc "The function is run at `create-token` step of marmalade-v2.ledger."

    (require-capability (INIT-CALL (at "id" token) (at "precision" token) (at "uri" token) POLICY))

    (enforce (= (at 'precision token) 0) "Precision must be 0 for soul-bound tokens")

    (read-msg MINT-GUARD-MSG-KEY)
    (read-msg BURN-GUARD-MSG-KEY)

    (enforce (has-guard-policy (at 'policies token)) "Guard policy is required for soul-bound tokens")

    true
  )

  (defun enforce-mint:bool
    ( token:object{token-info}
      account:string
      guard:guard
      amount:decimal
    )
    (require-capability (MINT-CALL (at "id" token) account amount POLICY))

    (enforce (= amount 1.0) "Amount must be 1.0 for soul-bound tokens")

    (let* ((mint-record:object{mint-record} (try { 'account: "" } (read records (at 'id token))))
          (has-mint-record:bool (!= (at 'account mint-record) "")))
      (enforce (not has-mint-record) "Token was already minted")
    )

    (insert records (at 'id token) { 'account: account })

    true
  )

  (defun enforce-burn:bool
    ( token:object{token-info}
      account:string
      amount:decimal
    )
    (require-capability (BURN-CALL (at "id" token) account amount POLICY))

    (enforce (= amount 1.0) "Amount must be 1.0 for soul-bound tokens")

    true
  )

  (defun enforce-offer:bool
    ( token:object{token-info}
      seller:string
      amount:decimal
      timeout:integer
      sale-id:string )
    (enforce false "Sale is not allowed!")
  )

  (defun enforce-buy:bool
    ( token:object{token-info}
      seller:string
      buyer:string
      buyer-guard:guard
      amount:decimal
      sale-id:string )
    (enforce false "Sale is not allowed!")
  )

  (defun enforce-withdraw:bool
    ( token:object{token-info}
      seller:string
      amount:decimal
      timeout:integer
      sale-id:string )
    (enforce false "Sale is not allowed!")
  )

  (defun enforce-transfer:bool
    ( token:object{token-info}
      sender:string
      guard:guard
      receiver:string
      amount:decimal )
    (enforce false "Transfer is not allowed!")
  )
)

(if (read-msg 'upgrade)
  true
  (create-table records)
)

(enforce-guard ADMIN-KS)