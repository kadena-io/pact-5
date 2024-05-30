(namespace (read-msg 'ns))

(module private-token-policy-v1 GOVERNANCE

  (defconst ADMIN-KS:string "marmalade-examples.private-token-policy")

  (defcap GOVERNANCE ()
    (enforce-guard ADMIN-KS))

  (implements kip.token-policy-v2)
  (implements kip.updatable-uri-policy-v1)
  (use kip.token-policy-v2 [token-info])
  (use marmalade-v2.guard-policy-v1 [URI-GUARD-MSG-KEY])

  (defschema revealed-tokens-schema
    revealed:bool  
  )

  (deftable revealed-tokens:{revealed-tokens-schema})

  (defcap TOKEN_REVEALED (token-id:string uri:string)
    @doc "Emitted when the token URI has been revealed"
    @event
    true
  )

  (defun has-guard-policy:bool (policies)
    (> (length (filter (lambda (policy) (= (format "{}" [policy]) "marmalade-v2.guard-policy-v1")) policies)) 0))

  (defun enforce-init:bool
    ( token:object{token-info}
    )

    (enforce (has-guard-policy (at 'policies token)) "Guard policy is required for private tokens")

    (read-msg URI-GUARD-MSG-KEY)

    true
  )

  (defun enforce-mint:bool
    ( token:object{token-info}
      account:string
      guard:guard
      amount:decimal
    )
    true
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
    true
  )

  (defun enforce-update-uri:bool
    ( token:object{kip.token-policy-v2.token-info}
      new-uri:string
    )
    (let ((revealed:bool (is-revealed (at 'id token))))
      (enforce revealed "Update disabled prior to revealing")
    )
  )

  (defun reveal-uri:bool (token-id:string new-uri:string)
    (let* (
      (token-info:object{kip.token-policy-v2.token-info} (marmalade-v2.ledger.get-token-info token-id))
      (token-uri-hash:string (at 'uri token-info))
      (already-revealed:bool (is-revealed token-id))
    )
      (enforce (not already-revealed) "Token URI already revealed")

      (enforce (not (= new-uri "")) "URI cannot be empty")

      (enforce (= token-uri-hash (hash new-uri)) "URI does not match the hash")
      
      (insert revealed-tokens token-id { 'revealed: true })

      (marmalade-v2.ledger.update-uri token-id new-uri)

      (emit-event (TOKEN_REVEALED token-id new-uri))
     
      true
    )
  )

  (defun is-revealed:bool (token-id:string)
    (with-default-read revealed-tokens token-id 
      { 'revealed : false } 
      { 'revealed := revealed }
      revealed
    )
  )
)

(if (read-msg 'upgrade)
  true
  (create-table revealed-tokens)
)

(enforce-guard ADMIN-KS)