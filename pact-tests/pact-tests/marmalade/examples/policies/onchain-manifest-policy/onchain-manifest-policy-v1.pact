(namespace (read-msg 'ns))

(module onchain-manifest-policy-v1 GOVERNANCE

  @doc "onchain manifest storage for marmalade-v2"
  (implements kip.token-policy-v2)

  (use kip.token-manifest)
  (use kip.token-policy-v2 [token-info])
  (use marmalade-v2.policy-manager)

  (defconst ADMIN-KS:string "marmalade-examples.onchain-manifest-policy")

  (defcap GOVERNANCE ()
    (enforce-guard ADMIN-KS))

  (defconst POLICY:string (format "{}" [onchain-manifest-policy-v1]))
    
  (defconst MANIFEST-SPEC-MSG-KEY:string "manifest_spec")

  (defschema manifest-spec
    manifest:object{manifest}
    guard:guard
  )

  (deftable manifests:{manifest-spec})

  (defcap UPGRADE (token-id:string)
    @managed
    (with-read manifests token-id {
      "guard":=manifest-guard
      }
      (enforce-guard manifest-guard)
    )
  )

  (defun get-manifest:object{manifest} (token-id:string)
    (with-read manifests token-id {
      "manifest":= manifest
    }
    manifest
  ))

  (defun upgrade-manifest
    ( token-id:string
      manifest:object{manifest}
    )
    (with-capability (UPGRADE token-id)
      (enforce-verify-manifest manifest)
      (update manifests token-id {
        "manifest": manifest
      })
    )
  )

  (defun enforce-init:bool
    ( token:object{token-info}
    )
    @doc "Executed at `create-token` step of marmalade.ledger.                  \
    \ Required msg-data keys:                                                  \
    \ * manifest_spec:object{manifest-spec} - registers the manifest object of \
    \ the token and the guard that manages the upgrade of the manifest on chain"
    (require-capability (INIT-CALL (at "id" token) (at "precision" token) (at "uri" token) POLICY))
    (let ( (manifest:object{manifest-spec} (read-msg MANIFEST-SPEC-MSG-KEY )) )
      (enforce-verify-manifest (at 'manifest manifest))
      (insert manifests (at 'id token) manifest)
    )
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
      sale-id:string
    )
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


  (defun enforce-transfer:bool
    ( token:object{token-info}
      sender:string
      guard:guard
      receiver:string
      amount:decimal )
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
)

(if (read-msg 'upgrade)
  ["upgrade complete"]
  [ (create-table manifests)
])

(enforce-guard ADMIN-KS)