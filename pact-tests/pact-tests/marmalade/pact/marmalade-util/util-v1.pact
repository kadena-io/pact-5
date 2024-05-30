(namespace (read-string 'ns))

(module util-v1 GOVERNANCE
  (use kip.token-policy-v2)
  (use marmalade-v2.ledger)
  (use marmalade-v2.policy-manager)
  (use marmalade-v2.policy-manager [CONCRETE_POLICY_LIST NON_FUNGIBLE_POLICY ROYALTY_POLICY COLLECTION_POLICY GUARD_POLICY])

  (defconst ADMIN-KS:string "marmalade-v2.marmalade-contract-admin")

  (defcap GOVERNANCE ()
    (enforce-guard ADMIN-KS))

  (defcap UTIL-SIGN ()
    @doc "Capabiltiy to easily scope signatures"
    true
  )

  (defschema concrete-policy-bool
    non-fungible-policy:bool
    royalty-policy:bool
    collection-policy:bool
    guard-policy:bool
  )

  (defconst DEFAULT:object{concrete-policy-bool}
    { 'non-fungible-policy: true
     ,'royalty-policy: false
     ,'collection-policy:false
     ,'guard-policy: true
     })

  (defconst DEFAULT_ROYALTY:object{concrete-policy-bool}
    { 'non-fungible-policy: true
     ,'royalty-policy: true
     ,'collection-policy:false
     ,'guard-policy: true
    }
  )

  (defconst DEFAULT_COLLECTION:object{concrete-policy-bool}
    { 'non-fungible-policy: true
     ,'royalty-policy: false
     ,'collection-policy: true
     ,'guard-policy: true
    }
  )

  (defconst DEFAULT_COLLECTION_ROYALTY:object{concrete-policy-bool}
    { 'non-fungible-policy: true
     ,'royalty-policy: true
     ,'collection-policy: true
     ,'guard-policy: true
    }
  )

  (defconst EMPTY:object{concrete-policy-bool}
    { 'non-fungible-policy: false
     ,'royalty-policy: false
     ,'collection-policy:false
     ,'guard-policy: false
    }
  )

  (defun create-policies (concrete-policy:object{concrete-policy-bool})
    (let* ( (is-used-policy (lambda (policy-field:string) (at policy-field concrete-policy)))
            (used-policies:[string] (filter (is-used-policy) CONCRETE_POLICY_LIST)))
          (map (get-concrete-policy) used-policies))
  )

  (defun create-concrete-policy:object{concrete-policy-bool} (policies:[module{kip.token-policy-v2}])
    { 'non-fungible-policy: (contains (get-concrete-policy NON_FUNGIBLE_POLICY) policies)
     ,'royalty-policy: (contains (get-concrete-policy ROYALTY_POLICY) policies)
     ,'collection-policy: (contains (get-concrete-policy COLLECTION_POLICY) policies)
     ,'guard-policy: (contains (get-concrete-policy GUARD_POLICY) policies)
    }
  )

  (defun contains-concrete-policy:bool (concrete-policy:string policies:[module{kip.token-policy-v2}])
    (let* ( (policy-modref:module{kip.token-policy-v2} (get-concrete-policy concrete-policy) )
            (policy-str:string (format "{}" [policy-modref]) )
            (policies-str:[string] (map (lambda (policy:module{kip.token-policy-v2})
                                      (format "{}" [policy])) policies)) )
    (enforce (contains policy-str policies-str) (format "{} is required" [concrete-policy])))
    true
  )

  (defun to-timestamp:integer (input:time)
    "Computes an Unix timestamp of the input date"
    (floor (diff-time input (time "1970-01-01T00:00:00Z")))
  )

  (defun curr-time:integer ()
    "Computes an Unix timestamp of the block time"
    (to-timestamp (at 'block-time (chain-data)))
  )

  (defun mint-NFT (uri:string policies:[module{kip.token-policy-v2}] guard:guard)
    @doc "Mints a NON-FUNGIBLE-TOKEN with policies and creation-guard"
    (let* ( (nfp-precision:integer 0)
            (account:string (create-principal guard))
            (nfp-amount:decimal 1.0)
            (token-id:string (create-token-id {'uri: uri, 'precision: nfp-precision, 'policies: policies} guard)) )
      (contains-concrete-policy NON_FUNGIBLE_POLICY policies)
      (with-capability (UTIL-SIGN)
        (create-token token-id nfp-precision uri policies guard)
      )
      (install-capability (MINT token-id account nfp-amount))
      (mint token-id account guard nfp-amount)
    )
  )

  (defun mint-basic-NFT (uri:string guard:guard)
    @doc "Mints a NON-FUNGIBLE-TOKEN without any configuration"
    (mint-NFT uri [(get-concrete-policy NON_FUNGIBLE_POLICY)] guard)
  )

  (defun create-token-with-mint-guard (uri:string precision:integer policies:[module{kip.token-policy-v2}])
    @doc "Creates a token, enforce that MINT-GUARD is registered"
    (let* ( (mint-guard:guard (read-keyset "mint_guard"))
            (token-id:string (create-token-id {'uri: uri, 'precision: precision, 'policies: policies} mint-guard))
            )
      (contains-concrete-policy GUARD_POLICY policies)
      (with-capability (UTIL-SIGN)
        (create-token token-id precision uri policies mint-guard)
      )
    )
  )

)

(enforce-guard ADMIN-KS)
