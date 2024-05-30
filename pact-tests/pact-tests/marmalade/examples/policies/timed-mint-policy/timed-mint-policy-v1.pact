(namespace (read-msg 'ns))

(module timed-mint-policy-v1 GOVERNANCE

  @doc "Policy for timed-mint tokens with royalty and quoted sale in coin."

  (defconst ADMIN-KS:string "marmalade-examples.timed-mint-policy")

  (defcap GOVERNANCE ()
    (enforce-guard ADMIN-KS))

  (defconst POLICY:string (format "{}" [timed-mint-policy-v1]))

  (implements kip.token-policy-v2)
  (use kip.token-policy-v2 [token-info])
  (use marmalade-v2.policy-manager)
  (use marmalade-v2.util-v1 [curr-time])

  (defschema timed-mint-schema
    max-supply:decimal
    mint-start-time:integer
    mint-end-time:integer
  )

  (deftable timed-mint:{timed-mint-schema})

  (defconst TIMED-MINT-SPEC:string "timed_mint_spec")

  (defun validate-specs (spec:object{timed-mint-schema})
    @doc "Enforce that the time windows and max supply conforms \
    \to the contract requirements."

    (let* ( (max-supply:decimal (at 'max-supply spec))
            (mint-start-time:integer (at 'mint-start-time spec))
            (mint-end-time:integer (at 'mint-end-time spec)) )

      (enforce (>= max-supply 0.0) "Max supply must be non-negative")
      (enforce (>= mint-end-time mint-start-time) "Mint end time must be after mint start time")
      (enforce (>= mint-start-time (curr-time)) "Mint start time must be in the future") ))

   (defun enforce-init:bool
     ( token:object{token-info}
     )
     (require-capability (INIT-CALL (at "id" token) (at "precision" token) (at "uri" token) POLICY))
     (let* ( (spec:object{timed-mint-schema} (read-msg TIMED-MINT-SPEC))
             (max-supply:decimal (at 'max-supply spec))
             (mint-start-time:integer (at 'mint-start-time spec))
             (mint-end-time:integer (at 'mint-end-time spec))
           )
       (validate-specs spec)
       (insert timed-mint (at 'id token)
         { 'max-supply: max-supply
         , 'mint-start-time:mint-start-time
         , 'mint-end-time:mint-end-time
         }))
     true
   )

  (defun enforce-mint:bool
    ( token:object{token-info}
      account:string
      guard:guard
      amount:decimal
    )
    (require-capability (MINT-CALL (at "id" token) account amount POLICY))
    (let* ( (account-bal:decimal (try 0.0 (at 'balance (marmalade-v2.ledger.details (at 'id token) account))))
            (total-supply:decimal (try 0.0 (marmalade-v2.ledger.total-supply (at 'id token))))
            (timed-mint:object{timed-mint-schema} (get-timed-mint token))
            (max-supply:decimal (at 'max-supply timed-mint))
            (mint-start-time:integer (at 'mint-start-time timed-mint))
            (mint-end-time:integer (at 'mint-end-time timed-mint)) )
      (enforce (= account-bal 0.0) "Account has already minted")
      (enforce (>= (curr-time) mint-start-time) "Mint has not started yet")
      (enforce (< (curr-time) mint-end-time) "Mint has ended")
      (enforce (>= amount 0.0) "Mint amount must be positive")
      (if (= max-supply 0.0) true [
        (if (> total-supply 0.0) (enforce (!= max-supply total-supply) "Mint has reached max supply") true)
        (enforce (<= (+ total-supply amount) max-supply) "Exceeds max supply")
      ])
      true
    )
  )

  (defun get-timed-mint:object{timed-mint-schema} (token:object{token-info})
    (read timed-mint (at 'id token))
  )

  (defun enforce-burn:bool
    ( token:object{token-info}
      account:string
      amount:decimal
    )
    true
  )

  (defun enforce-offer:bool
    ( token:object{token-info}
      seller:string
      amount:decimal
      timeout:integer
      sale-id:string )
    true
  )

  (defun enforce-buy:bool
    ( token:object{token-info}
      seller:string
      buyer:string
      buyer-guard:guard
      amount:decimal
      sale-id:string )
    true
  )

  (defun enforce-withdraw:bool
    ( token:object{token-info}
      seller:string
      amount:decimal
      timeout:integer
      sale-id:string )
    true
  )

  (defun enforce-transfer:bool
    ( token:object{token-info}
      sender:string
      guard:guard
      receiver:string
      amount:decimal )
    (enforce false "Transfer prohibited")
  )
)

(if (read-msg 'upgrade)
  true
  (create-table timed-mint)
)

(enforce-guard ADMIN-KS)