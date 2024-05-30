(namespace (read-msg 'ns))

(module multi-asset-policy GOVERNANCE

  @doc "Policy for context-dependant multi-asset tokens."

  (implements kip.token-policy-v2)

  (use kip.token-manifest)
  (use kip.token-policy-v2 [token-info])
  (use marmalade-v2.policy-manager)
  (use marmalade-v2.non-fungible-policy-v1)

  (defconst ADMIN-KS:string "marmalade-examples.multi-asset-policy")

  (defcap GOVERNANCE ()
    (enforce-guard ADMIN-KS))

  (defconst OPERATOR-GUARD-MSG-KEY:string "operator_guard")
  (defconst ASSETS-MSG-KEY:string "assets")

  ; PROTOCOL CAPABILITIES

  (defcap PROPOSE_ASSET (token-id:string uri:string)
    (enforce-guard (at 'guard (read token-operators token-id)))
  )
  
  (defcap REJECT_ASSET (token-id:string asset-id:integer owner:string)
    (enforce-guard (marmalade-v2.ledger.account-guard token-id owner))
  )  

  (defcap REJECT_ALL_ASSETS (token-id:string owner:string)
    (enforce-guard (marmalade-v2.ledger.account-guard token-id owner))
  )
  
  (defcap ACCEPT_ASSET (token-id:string asset-id:integer owner:string)
    (enforce-guard (marmalade-v2.ledger.account-guard token-id owner))
  )

  (defcap SET_ASSET_PRIORITY (token-id:string asset-id:integer priority:integer owner:string)
    (enforce-guard (marmalade-v2.ledger.account-guard token-id owner))
  )

  ; PROTOCOL EVENTS

  (defcap ASSET_PROPOSED (token-id:string asset-id:integer uri:string)
    @doc "Emitted when new asset is proposed"
    @event
    true
  )

  (defcap ASSET_REJECTED (token-id:string asset-id:integer uri:string owner:string)
    @doc "Emitted when the asset has been rejected"
    @event
    true
  )
    
  (defcap ASSET_ACCEPTED (token-id:string asset-id:integer uri:string owner:string)
    @doc "Emitted when the asset has been accepted"
    @event
    true
  )

  (defcap ASSET_SET (token-id:string asset-id:integer uri:string)
    @doc "Emitted at init for fungible tokens"
    @event

    true
  )

  (defcap ASSET_PRIORITY_SET (token-id:string asset-id:integer priority:integer owner:string)
    @doc "Emitted when new asset priority is set"
    @event
    true
  )

  (defschema token-asset-schema 
    assets:[string]
  )

  (defschema token-operators-schema
    guard:guard
  )

  (deftable token-assets:{token-asset-schema})
  (deftable proposed-assets:{token-asset-schema})
  (deftable token-operators:{token-operators-schema})

  ; PROTOCOL FUNCTIONS

  (defun get-asset:string (token-id:string asset-id:integer)
    (at asset-id (at 'assets (read token-assets token-id)))
  )  
  
  (defun get-assets:[string] (token-id:string)
    (with-default-read token-assets token-id 
      { 'assets: [] } 
      { 'assets := assets } 
      assets
    )
  )

  (defun propose-asset:bool (token-id:string uri:string)
    (enforce-non-fungible-policy token-id)
    (enforce (not (= uri "")) "URI cannot be empty")
    (let* (
      (assets:[string] (+ (get-proposed-assets token-id) [uri]))
      (asset-id:integer (- (length assets) 1))
    )
      (with-capability (PROPOSE_ASSET token-id uri)
        (write proposed-assets token-id { 'assets: assets })
        (emit-event (ASSET_PROPOSED token-id asset-id uri))
        true
      )
    )
  )

  (defun replace-proposed-asset:bool (token-id:string asset-id:integer uri:string)
    (enforce (not (= uri "")) "URI cannot be empty")

    (let (
      (assets:[string] (get-proposed-assets token-id))
      (operator-guard:guard (at 'guard (read token-operators token-id)))
    )
      (enforce (> (length assets) asset-id) "Invalid asset ID")
      (enforce (not (= uri (at asset-id assets))) "Must be different URI")

      (with-capability (ASSET_PROPOSED token-id asset-id uri)
        (write proposed-assets token-id { 'assets: (update-array assets asset-id uri) })
        true
      )
    )
  )

  (defun get-proposed-assets:[string] (token-id:string)
    (with-default-read proposed-assets token-id 
      { 'assets: [] } 
      { 'assets := assets } 
      assets
    )
  )

  (defun get-proposed-asset:string (token-id:string asset-id:integer)
    (at asset-id (get-proposed-assets token-id))
  )

  (defun reject-proposed-asset:bool (token-id:string asset-id:integer owner:string)
    (let ((balance:decimal (marmalade-v2.ledger.get-balance token-id owner)))
      (enforce (= balance 1.0) "Token not owned")
    )

    (let ((assets:[string] (get-proposed-assets token-id)))
      (enforce (>= (- (length assets) 1) asset-id) "Invalid asset ID")
      (with-capability (REJECT_ASSET token-id asset-id owner)
        (let ((updated-assets:[string] (filter (lambda (index:integer) (not (= index asset-id))) (enumerate 0 (- (length assets) 1)))))
          (write proposed-assets token-id { 'assets: updated-assets })
          true
        )
      )
      (emit-event (ASSET_REJECTED token-id asset-id (at asset-id assets) owner))
    )
  )

  (defun reject-all-proposed-assets:bool (token-id:string owner:string)
    (let (
      (balance:decimal (marmalade-v2.ledger.get-balance token-id owner))
      (assets:[string] (get-proposed-assets token-id))
    )
      (enforce (= balance 1.0) "Token not owned")

      (with-capability (REJECT_ALL_ASSETS token-id owner)
        (write proposed-assets token-id { 'assets: [] })
      )

      (map 
        (lambda (asset-id:integer)
          (emit-event (ASSET_REJECTED token-id asset-id (at asset-id assets) owner))
        ) 
      (enumerate 0 (- (length assets) 1)))

      true
    )
  )

  (defun accept-asset:bool (token-id:string asset-id:integer owner:string)
    (let (
      (balance:decimal (marmalade-v2.ledger.get-balance token-id owner))
      (uri:string (get-proposed-asset token-id asset-id))
      (assets:[string] (get-assets token-id))
    )
      (enforce (= balance 1.0) "Token not owned")

      (with-capability (ACCEPT_ASSET token-id asset-id owner)
        (write token-assets token-id { 'assets: (+ assets [uri]) })
        (emit-event (ASSET_ACCEPTED token-id asset-id uri owner))
        true  
      )
    )
  )

  (defun set-asset-priority (token-id:string asset-id:integer priority:integer owner:string)
    (let* (
      (balance:decimal (marmalade-v2.ledger.get-balance token-id owner))
      (assets:[string] (get-assets token-id))
      (target-asset:string (at asset-id assets))
      (asset-to-be-replaced:string (at priority assets))
      (updated-assets:[string] (update-array assets priority target-asset))
      (truncated-assets:[string] (remove-item updated-assets asset-id))
    )
      (enforce (= balance 1.0) "Token not owned")

      (with-capability (SET_ASSET_PRIORITY token-id asset-id priority owner)
        (write token-assets token-id { 'assets: (+ truncated-assets [asset-to-be-replaced]) })
        (emit-event (ASSET_PRIORITY_SET token-id asset-id priority owner))
        true
      )
    )
  
  )

  ; POLICY FUNCTIONS

  (defun enforce-init:bool
    ( token:object{token-info}
    )
    (if (has-non-fungible-policy (at 'policies token))
      (enforce-operator-at-init token)
      (enforce-assets-at-init token)
    )
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

  ; UTILITY FUNCTIONS

  (defun has-non-fungible-policy:bool (policies)
    (> (length (filter (lambda (policy) (= policy marmalade-v2.non-fungible-policy-v1)) policies)) 0))

  (defun enforce-non-fungible-policy (token-id:string)
    (let ((policies (at 'policies (marmalade-v2.ledger.get-token-info token-id))))
      (enforce (has-non-fungible-policy policies) "Token does not have non-fungible policy")
    )
  )
  
  (defun enforce-assets-at-init:bool
    ( token:object{token-info}
    )
    (let ((assets:[string] (read-msg ASSETS-MSG-KEY)))

      (enforce (> (length assets) 0) "At least one asset must be provided")

      (insert token-assets (at 'id token) { 'assets: assets } )

      (map
        (lambda (asset-id:integer)
          (emit-event (ASSET_SET (at 'id token) asset-id (at asset-id assets)))
        ) 
      (enumerate 0 (- (length assets) 1)))
      true
    )
  )

  (defun enforce-operator-at-init:bool
    ( token:object{token-info}
    )
    (let ((operator-guard:guard (read-msg OPERATOR-GUARD-MSG-KEY)))
      (enforce-guard operator-guard)
      (insert token-operators (at 'id token) { 'guard: operator-guard })
      true
    )
  )

  (defun update-array:[string] (array:[string] index:integer value:string)
    (map (lambda (i) 
      (if (= i index)
        value
        (at i array)))
    (enumerate 0 (- (length array) 1)))
  )

  (defun remove-item:[string] (array:[string] index:integer)
    (+ 
      (take index array)
      (take (- 1 (- (length array) index)) array)
    )
  )
)

(if (read-msg 'upgrade)
  true
  [
    (create-table token-assets)
    (create-table proposed-assets)
    (create-table token-operators)
  ]
)

(enforce-guard ADMIN-KS)