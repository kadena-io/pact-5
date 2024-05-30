
(namespace (read-string 'ns))

(module guard-policy-v1 GOVERNANCE

  (defconst ADMIN-KS:string "marmalade-v2.marmalade-contract-admin")
  (defconst POLICY:string (format "{}" [guard-policy-v1]))

  (defcap GOVERNANCE ()
    (enforce-guard ADMIN-KS))

  (implements kip.token-policy-v2)
  (implements kip.updatable-uri-policy-v1)

  (use kip.token-policy-v2 [token-info])
  (use marmalade-v2.policy-manager)

  (defschema all-guards
    mint-guard:guard
    burn-guard:guard
    sale-guard:guard
    transfer-guard:guard
    uri-guard:guard
  )

  (defschema guards
    mint-guard:guard
    burn-guard:guard
    sale-guard:guard
    transfer-guard:guard
  )

  (defschema uri-guard
    uri-guard:guard
  )

  (deftable policy-guards:{guards})
  (deftable uri-guards:{uri-guard})

  (defconst MINT-GUARD-MSG-KEY:string "mint_guard")
  (defconst BURN-GUARD-MSG-KEY:string "burn_guard")
  (defconst SALE-GUARD-MSG-KEY:string "sale_guard")
  (defconst TRANSFER-GUARD-MSG-KEY:string "transfer_guard")
  (defconst URI-GUARD-MSG-KEY:string "uri_guard")

  (defconst GUARD_SUCCESS:guard (create-user-guard (success)))
  (defconst GUARD_FAILURE:guard (create-user-guard (failure)))

  (defcap GUARDS:bool (token-id:string guards:object{all-guards})
    @doc "Emits event for discovery"
    @event
    true
  )

  (defcap MINT (token-id:string account:string amount:decimal)
    (enforce-guard (get-mint-guard token-id))
  )

  (defcap BURN (token-id:string account:string amount:decimal)
    (enforce-guard (get-burn-guard token-id))
  )

  (defcap SALE (token-id:string seller:string amount:decimal)
    (enforce-guard (get-sale-guard token-id))
  )

  (defcap TRANSFER (token-id:string sender:string receiver:string amount:decimal)
    (enforce-guard (get-transfer-guard token-id))
  )

  (defcap UPDATE-URI (token-id:string new-uri:string)
    (enforce-guard (get-uri-guard token-id))
  )

  (defun success:bool ()
    true
  )

  (defun failure:bool ()
    (enforce false "Disabled")
    true
  )

  (defun get-mint-guard:guard (token-id:string)
    (with-read policy-guards token-id {
      'mint-guard:= mint-guard
    }
    mint-guard
    )
  )

  (defun get-burn-guard:guard (token-id:string)
    (with-read policy-guards token-id {
      "burn-guard":= burn-guard
    }
    burn-guard
    )
  )

  (defun get-sale-guard:guard (token-id:string)
    (with-read policy-guards token-id {
      "sale-guard":= sale-guard
    }
    sale-guard
    )
  )

  (defun get-transfer-guard:guard (token-id:string)
    (with-read policy-guards token-id {
      "transfer-guard":= transfer-guard
    }
    transfer-guard
    )
  )

  (defun get-uri-guard:guard (token-id:string)
    (let ((version:integer (marmalade-v2.ledger.get-version token-id)))
      (if (= version 0)
        GUARD_FAILURE
        (with-read uri-guards token-id
          {
            "uri-guard":= uri-guard
          }
          uri-guard
        )
      )
    )
  )

  (defun get-guards:object{all-guards} (token:object{token-info})
    (with-read policy-guards (at 'id token) {
       'mint-guard:= mint-guard
      ,'burn-guard:= burn-guard
      ,'sale-guard:= sale-guard
      ,'transfer-guard:= transfer-guard
      }
      (with-default-read uri-guards (at 'id token)
        {'uri-guard: GUARD_FAILURE}
        {'uri-guard:= uri-guard}
      {'uri-guard: uri-guard
      ,'mint-guard: mint-guard
      ,'burn-guard: burn-guard
      ,'sale-guard: sale-guard
      ,'transfer-guard: transfer-guard
      })
    )
  )

  (defun enforce-init:bool
    ( token:object{token-info}
    )
    @doc "Executed at `create-token` step of marmalade.ledger. Registers  guards for \
    \ 'mint', 'burn', 'sale', 'transfer' operations of the created token.            \
    \ Required msg-data keys:                                                        \
    \ * (optional) uri-guard:string -  uri-guard and adds failure guard if absent. \
    \ * (optional) mint_guard:string -  mint-guard and adds success guard if absent. \
    \ * (optional) burn_guard:string -  burn-guard and adds success guard if absent. \
    \ * (optional) sale_guard:string -  sale-guard and adds success guard if absent. \
    \ * (optional) transfer_guard:string -  transfer-guard and adds success guard if absent. \
    \ the created token"
    (require-capability (INIT-CALL (at "id" token) (at "precision" token) (at "uri" token) POLICY))
    (let ((guards:object{guards}
          { 'mint-guard: (try GUARD_SUCCESS (read-msg MINT-GUARD-MSG-KEY) )
          , 'burn-guard: (try GUARD_SUCCESS (read-msg BURN-GUARD-MSG-KEY) )
          , 'sale-guard: (try GUARD_SUCCESS (read-msg SALE-GUARD-MSG-KEY) )
          , 'transfer-guard: (try GUARD_SUCCESS (read-msg TRANSFER-GUARD-MSG-KEY) ) } )
          (uri-guard:object{uri-guard}
            { 'uri-guard: (try GUARD_SUCCESS (read-msg URI-GUARD-MSG-KEY) ) }))
      (insert policy-guards (at 'id token)
        guards)
      (insert uri-guards (at 'id token)
        uri-guard)
      (emit-event (GUARDS (at "id" token) (+ uri-guard guards))) )
      true
  )

  (defun enforce-mint:bool
    ( token:object{token-info}
      account:string
      guard:guard
      amount:decimal
    )
    (require-capability (MINT-CALL (at "id" token) account amount POLICY))
    (with-capability (MINT (at 'id token) account amount)
      true
    )
  )

  (defun enforce-burn:bool
    ( token:object{token-info}
      account:string
      amount:decimal
    )
    (require-capability (BURN-CALL (at "id" token) account amount POLICY))
    (with-capability (BURN (at 'id token) account amount)
      true
    )
  )

  (defun enforce-offer:bool
    ( token:object{token-info}
      seller:string
      amount:decimal
      timeout:integer
      sale-id:string )
    (require-capability (OFFER-CALL (at "id" token) seller amount sale-id timeout POLICY))
    (enforce-sale-pact sale-id)
    (with-capability (SALE (at 'id token) seller amount)
      true
    )
  )

  (defun enforce-buy:bool
    ( token:object{token-info}
      seller:string
      buyer:string
      buyer-guard:guard
      amount:decimal
      sale-id:string )
    (require-capability (BUY-CALL (at "id" token) seller buyer amount sale-id POLICY))
    (enforce-sale-pact sale-id)
    (with-capability (SALE (at 'id token) seller amount)
      true
    )
  )

  (defun enforce-withdraw:bool
    ( token:object{token-info}
      seller:string
      amount:decimal
      timeout:integer
      sale-id:string )
    (require-capability (WITHDRAW-CALL (at "id" token) seller amount sale-id timeout POLICY))
    (enforce-sale-pact sale-id)
    (with-capability (SALE (at 'id token) seller amount)
      true
    )
  )

  (defun enforce-transfer:bool
    ( token:object{token-info}
      sender:string
      guard:guard
      receiver:string
      amount:decimal )
    (require-capability (TRANSFER-CALL (at "id" token) sender receiver amount POLICY))
    (with-capability (TRANSFER (at 'id token) sender receiver amount)
      true
    )
  )

  (defun enforce-update-uri:bool
    ( token:object{token-info}
      new-uri:string )
    (require-capability (UPDATE-URI-CALL (at "id" token) new-uri POLICY))
    (with-capability (UPDATE-URI (at 'id token) new-uri)
      true
    )
  )

  (defun enforce-sale-pact:bool (sale-id:string)
    "Enforces that SALE is id for currently executing pact"
    (enforce (= sale-id (pact-id)) "Invalid pact/sale id")
  )
)

(if (read-msg 'upgrade )
  (if (read-msg 'upgrade_version_1 )
    [ (create-table uri-guards) ]
    ["upgrade complete"]
  )
  [ (create-table policy-guards) ]
)

(enforce-guard ADMIN-KS)
