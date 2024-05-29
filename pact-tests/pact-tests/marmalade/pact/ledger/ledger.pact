(namespace (read-string 'ns))

(module ledger GOVERNANCE

  @model
    [
      (defproperty valid-account (account:string)
          (> (length account) 2))
    ]

  (implements marmalade-v2.ledger-v2)
  (implements kip.poly-fungible-v3)

  (use kip.poly-fungible-v3 [account-details sender-balance-change receiver-balance-change])
  (use kip.token-policy-v2 [token-info])
  (use util.fungible-util)
  (use marmalade-v2.policy-manager)

  ;; Version

  (defconst VERSION:integer 1)

  ;;
  ;; Tables/Schemas
  ;;

  (deftable ledger:{account-details})

  (defschema token-schema
    id:string
    uri:string
    precision:integer
    supply:decimal
    policies:[module{kip.token-policy-v2}]
  )

  (defschema token-details
    uri:string
    precision:integer
    policies:[module{kip.token-policy-v2}]
  )

  (defschema version
    version:integer
  )

  (deftable tokens:{token-schema})
  (deftable versions:{version})

  ;;
  ;; Governance
  ;;

  (defconst ADMIN-KS:string "marmalade-v2.marmalade-contract-admin")

  (defcap GOVERNANCE ()
    (enforce-guard ADMIN-KS))

  ;;
  ;; poly-fungible-v3 caps
  ;;

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

  (defcap XTRANSFER:bool
    ( id:string
      sender:string
      receiver:string
      target-chain:string
      amount:decimal
    )
    @managed amount TRANSFER-mgr
    (enforce false "cross chain not supported")
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

  (defcap SUPPLY:bool (id:string supply:decimal)
    @doc " Emitted when supply is updated, if supported."
    @event true
  )

  (defcap CREATE-TOKEN:bool (id:string)
    true
  )

  (defcap TOKEN:bool (id:string precision:integer policies:[module{kip.token-policy-v2}] uri:string creation-guard:guard)
    @event
    true
  )

  (defcap RECONCILE:bool
    ( token-id:string
      amount:decimal
      sender:object{sender-balance-change}
      receiver:object{receiver-balance-change}
    )
    @doc " For accounting via events. \
         \ sender = {account: '', previous: 0.0, current: 0.0} for mint \
         \ receiver = {account: '', previous: 0.0, current: 0.0} for burn"
    @event
    true
  )

  (defcap ACCOUNT_GUARD:bool (id:string account:string guard:guard)
    @doc " Emitted when ACCOUNT guard is updated."
    @event
    true
  )

  ;;
  ;; ledger-v2 caps to be able to validate access to policy operations.
  ;;

  (defcap INIT-CALL:bool (id:string precision:integer uri:string)
    true
  )

  (defcap TRANSFER-CALL:bool (id:string sender:string receiver:string amount:decimal)
    true
  )

  (defcap MINT-CALL:bool (id:string account:string amount:decimal)
    true
  )

  (defcap BURN-CALL:bool (id:string account:string amount:decimal)
    true
  )

  (defcap UPDATE-URI-CALL:bool (id:string new-uri:string)
    true
  )

  (defcap OFFER-CALL:bool (id:string seller:string amount:decimal timeout:integer sale-id:string)
    true
  )

  (defcap WITHDRAW-CALL:bool (id:string seller:string amount:decimal timeout:integer sale-id:string)
    true
  )

  (defcap BUY-CALL:bool (id:string seller:string buyer:string amount:decimal sale-id:string)
    true
  )

  ;;
  ;; Implementation caps
  ;;

  (defcap DEBIT (id:string sender:string)
    (enforce-guard (account-guard id sender))
  )

  (defcap CREDIT (id:string receiver:string) true)

  (defcap UPDATE_SUPPLY ()
    "private cap for update-supply"
    true)

  (defcap MINT:bool (id:string account:string amount:decimal)
    @managed ;; one-shot for a given amount
    (enforce (< 0.0 amount) "Amount must be positive")
    (compose-capability (CREDIT id account))
    (compose-capability (UPDATE_SUPPLY))
  )

  (defcap BURN (id:string account:string amount:decimal)
    @managed ;; one-shot for a given amount
    (enforce (< 0.0 amount) "Amount must be positive")
    (compose-capability (DEBIT id account))
    (compose-capability (UPDATE_SUPPLY))
  )

  (defcap UPDATE-URI:bool
    ( token-id:string
      new-uri:string
    )
    @managed
    true
  )

  ;; Marmalade Functions

  (defun create-token:bool
    ( id:string
      precision:integer
      uri:string
      policies:[module{kip.token-policy-v2}]
      creation-guard:guard
    )
    @doc "Initializes a TOKEN with given policies and parameters, \
    \ and executes the enforce-init function for each listed policy."
    ;; enforces token and uri protocols
    (enforce-uri-reserved uri)
    (let ((token-details { 'uri: uri, 'precision: precision, 'policies: (sort policies) }))
      (enforce-token-reserved id token-details creation-guard)
    )
    (with-capability (INIT-CALL id precision uri)
      ;; maps policy list and calls policy::enforce-init
      (marmalade-v2.policy-manager.enforce-init
        { 'id: id, 'supply: 0.0, 'precision: precision, 'uri: uri,  'policies: policies})
    )
    (with-capability (CREATE-TOKEN id)
      (enforce-guard creation-guard)
      (insert tokens id {
        "id": id,
        "uri": uri,
        "precision": precision,
        "supply": 0.0,
        "policies": policies
      })
      (insert versions id {
        "version": VERSION
      })
      (emit-event (TOKEN id precision policies uri creation-guard))
    )
  )

  (defun mint:bool
    ( id:string
      account:string
      guard:guard
      amount:decimal
    )
    @doc "Mints an AMOUNT of TOKEN to the provided ACCOUNT and its guard,\
    \ and executes the enforce-mint policy functions."
    (with-capability (MINT-CALL id account amount)
      (marmalade-v2.policy-manager.enforce-mint (get-token-info id) account guard amount)
    )
    (with-capability (MINT id account amount)
      (let
        (
          (receiver (credit id account guard amount))
          (sender:object{sender-balance-change}
            {'account: "", 'previous: 0.0, 'current: 0.0})
        )
        (emit-event (RECONCILE id amount sender receiver))
        (update-supply id amount)
      ))
  )

  (defun burn:bool
    ( id:string
      account:string
      amount:decimal
    )
    @doc "Burns an AMOUNT of TOKEN from the specified ACCOUNT,\
    \ and executes the enforce-burn policy functions"
    (with-capability (BURN-CALL id account amount)
      (marmalade-v2.policy-manager.enforce-burn (get-token-info id) account amount)
    )
    (with-capability (BURN id account amount)
      (let
        (
          (sender (debit id account amount))
          (receiver:object{receiver-balance-change}
            {'account: "", 'previous: 0.0, 'current: 0.0})
        )
        (emit-event (RECONCILE id amount sender receiver))
        (update-supply id (- amount))
      ))
  )

  (defun transfer:bool
    ( id:string
      sender:string
      receiver:string
      amount:decimal
    )
    @doc "Transfers an AMOUNT of TOKEN from the sender ACCOUNT \
    \ to an existing receiver ACCOUNT, and executes the enforce-transfer \
    \ policy functions."

    (enforce (!= sender receiver)
      "sender cannot be the receiver of a transfer")
    (enforce-valid-transfer sender receiver (precision id) amount)
    (with-capability (TRANSFER-CALL id sender receiver amount)
      (marmalade-v2.policy-manager.enforce-transfer (get-token-info id) sender (account-guard id sender) receiver amount)
    )
    (with-capability (TRANSFER id sender receiver amount)
      (with-read ledger (key id receiver)
        { "guard" := g }
        (let
          ( (sender (debit id sender amount))
            (receiver (credit id receiver g amount))
          )
          (emit-event (RECONCILE id amount sender receiver))
        )
      ))
  )

  (defun transfer-create:bool
    ( id:string
      sender:string
      receiver:string
      receiver-guard:guard
      amount:decimal
    )
    @doc "Transfers an AMOUNT of TOKEN from the sender ACCOUNT \
    \ to the receiver ACCOUNT, creates an account if non-existent \
    \ , and executes the enforce-transfer policy functions."

    (enforce (!= sender receiver)
      "sender cannot be the receiver of a transfer")
    (enforce-valid-transfer sender receiver (precision id) amount)
    (with-capability (TRANSFER-CALL id sender receiver amount)
      (marmalade-v2.policy-manager.enforce-transfer (get-token-info id) sender (account-guard id sender) receiver amount)
    )
    (with-capability (TRANSFER id sender receiver amount)
      (let
        (
          (sender (debit id sender amount))
          (receiver (credit id receiver receiver-guard amount))
        )
        (emit-event (RECONCILE id amount sender receiver))
      ))
  )

  (defun update-uri:bool
    ( id:string
      new-uri:string
    )
    @doc "Updates the URI of the token, and executes enforce-update-uri \
    \ policy functions."

    (let ((version:integer (get-version id)))
      (enforce (> version 0) "updatable-uri not supported"))
    (with-capability (UPDATE-URI-CALL id new-uri)
      (marmalade-v2.policy-manager.enforce-update-uri (get-token-info id) new-uri)
    )
    (with-capability (UPDATE-URI id new-uri)
      (update tokens id { 'uri: new-uri })
    )
    true
  )

  ;; Implementation Functions

  (defun debit:object{sender-balance-change}
    ( id:string
      account:string
      amount:decimal
    )

    (require-capability (DEBIT id account))

    (enforce-unit id amount)

    (with-read ledger (key id account)
      { "balance" := old-bal }

      (enforce (<= amount old-bal) "Insufficient funds")

      (let ((new-bal (- old-bal amount)))
        (update ledger (key id account)
          { "balance" : new-bal }
          )
        {'account: account, 'previous: old-bal, 'current: new-bal}
      ))
  )

  (defun credit:object{receiver-balance-change}
    ( id:string
      account:string
      guard:guard
      amount:decimal
    )
    @doc "Credit AMOUNT to ACCOUNT balance"

    @model [ (property (> amount 0.0))
             (property (valid-account account))
           ]
    (enforce-reserved account guard)
    (enforce-unit id amount)

    (require-capability (CREDIT id account))

    (with-default-read ledger (key id account)
      { "balance" : -1.0 }
      { "balance" := old-bal }

      (let* ((is-new:bool
               (= old-bal -1.0))
             (old-bal:decimal (if is-new 0.0 old-bal))
             (new-bal:decimal  (+ old-bal amount)))
        (write ledger (key id account)
           { "balance" : new-bal
           , "guard"   : guard
           , "id" : id
           , "account" : account
           })
        (if is-new (emit-event (ACCOUNT_GUARD id account guard)) true)
        {'account: account, 'previous: old-bal, 'current: new-bal}
      ))
    )

  (defun credit-account:object{receiver-balance-change}
    ( id:string
      account:string
      amount:decimal
    )
    @doc "Credit AMOUNT to ACCOUNT"
    (credit id account (account-guard id account) amount)
  )

  (defun update-supply:bool (id:string amount:decimal)
    (require-capability (UPDATE_SUPPLY))
    (with-default-read tokens id
      { 'supply: 0.0 }
      { 'supply := s }
      (let ((new-supply (+ s amount)))
        (update tokens id {'supply: new-supply })
        (emit-event (SUPPLY id new-supply))))
  )

  (defun enforce-unit:bool (id:string amount:decimal)
    (let ((p (precision id)))
    (enforce
      (= (floor amount p)
         amount)
      "precision violation"))
  )

  (defpact transfer-crosschain:bool
    ( id:string
      sender:string
      receiver:string
      receiver-guard:guard
      target-chain:string
      amount:decimal )
    (step
      (enforce false "cross chain not supported"))
  )

  (defun account-guard:guard (id:string account:string)
    (with-read ledger (key id account) { 'guard := g } g)
  )

  (defun get-token-info:object{kip.token-policy-v2.token-info} (id:string)
    (with-read tokens id
     { 'policies := policies:[module{kip.token-policy-v2}]
     , 'supply := supply:decimal
     , 'precision := precision:integer
     , 'uri := uri:string
     }
     {
       'id: id
       , 'supply: supply
       , 'precision: precision
       , 'uri: uri
       , 'policies: policies
     } )
  )

  (defun create-account:bool
    ( id:string
      account:string
      guard:guard
    )
    (enforce-reserved account guard)
    (insert ledger (key id account)
      { "balance" : 0.0
      , "guard"   : guard
      , "id" : id
      , "account" : account
      })
    (emit-event (ACCOUNT_GUARD id account guard))
  )

  (defun create-token-id:string (token-details:object{token-details}
                                 creation-guard:guard)
    (format "t:{}"
      [(hash [(format "{}" [token-details]) (at 'chain-id (chain-data)) creation-guard])])
  )

  (defun check-reserved:string (token-id:string)
    " Checks token-id for reserved name and returns type if \
    \ found or empty string. Reserved names start with a \
    \ single char and colon, e.g. 't:foo', which would return 't' as type."
    (let ((pfx (take 2 token-id)))
      (if (= ":" (take -1 pfx)) (take 1 pfx) "")))

  (defun enforce-token-reserved:bool (token-id:string token-details:object{token-details}
                                      creation-guard:guard)
    @doc "Enforce reserved token-id name protocols."
    (let ((r (check-reserved token-id)))
      (if (= "t" r)
        (enforce
          (= token-id
             (create-token-id token-details creation-guard))
          "Token protocol violation")
        (enforce false
          (format "Unrecognized reserved protocol: {}" [r]) ))))

  (defun enforce-uri-reserved:bool (uri:string)
    " Enforce reserved uri name protocols "
    (if (= "marmalade:" (take 10 uri))
        (enforce false "Reserved protocol: marmalade:")
        true
    )
  )

  (defun truncate:decimal (id:string amount:decimal)
    (floor amount (precision id))
  )

  ;;
  ;; ACCESSORS
  ;;

  (defun key:string ( id:string account:string )
    @doc "DB key for ledger account"
    (format "{}:{}" [id account])
  )

  (defun get-balance:decimal (id:string account:string)
    @doc "Returns the ACCOUNT balance of the TOKEN"
    (at 'balance (read ledger (key id account)))
  )

  (defun details:object{account-details}
    ( id:string account:string )
    @doc "Returns ACCOUNT information of the TOKEN"
    (read ledger (key id account))
  )

  (defun total-supply:decimal (id:string)
    @doc "Returns total SUPPLY of the TOKEN"
    (with-default-read tokens id
      { 'supply : 0.0 }
      { 'supply := s }
      s)
  )

  (defun precision:integer (id:string)
    @doc "Returns PRECISION of the TOKEN"
    (with-read tokens id
      { 'precision := precision }
      precision
    )
  )

  (defun get-uri:string (id:string)
    @doc "Returns URI of the TOKEN"
    (with-read tokens id
      { 'uri := uri }
      uri
    )
  )

  (defun get-version:integer (id:string)
    @doc "Returns Marmalade VERSION of the TOKEN at creation"
    (with-default-read versions id
      {'version: 0 }
      {'version:= version }
      (if (< version 1)
        (with-read tokens id
          {'id:= id}
          version
        )
        version
      )
    )
  )

  ;;
  ;; sale
  ;;

  (defcap SALE:bool
    (id:string seller:string amount:decimal timeout:integer sale-id:string)
    @doc "Wrapper cap/event of SALE of token ID by SELLER of AMOUNT until TIMEOUT block height."
    @event
    (enforce (> amount 0.0) "Amount must be positive")
    (compose-capability (OFFER id seller amount timeout))
    (compose-capability (SALE_PRIVATE sale-id))
  )

  (defcap OFFER:bool
    (id:string seller:string amount:decimal timeout:integer)
    @doc "Managed cap for SELLER offering AMOUNT of token ID until TIMEOUT."
    @managed
    (enforce (sale-active timeout) "SALE: invalid timeout")
    (compose-capability (DEBIT id seller))
    (compose-capability (CREDIT id (sale-account)))
  )

  (defcap WITHDRAW:bool
    (id:string seller:string amount:decimal timeout:integer sale-id:string)
    @doc "Withdraws offer SALE from SELLER of AMOUNT of token ID after timeout."
    @managed
    (compose-capability (SALE_PRIVATE sale-id))
    (if (= 0 timeout)
      (enforce-guard (at 'guard (details id seller)))
      (enforce (not (sale-active timeout)) "WITHDRAW: still active")
    )
    (compose-capability (DEBIT id (sale-account)))
    (compose-capability (CREDIT id seller))
  )

  (defcap BUY:bool
    (id:string seller:string buyer:string amount:decimal sale-id:string)
    @doc "Completes sale OFFER to BUYER."
    @managed
    (compose-capability (SALE_PRIVATE sale-id))
    (compose-capability (DEBIT id (sale-account)))
    (compose-capability (CREDIT id buyer))
  )

  (defcap SALE_PRIVATE:bool (sale-id:string) true)

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
    (step
      ;; Step 1: buy
      (let ( (buyer:string (read-msg "buyer"))
              (buyer-guard:guard (read-msg "buyer-guard")) )
          (with-capability (BUY-CALL id seller buyer amount (pact-id))
            (marmalade-v2.policy-manager.enforce-buy (get-token-info id) seller buyer buyer-guard amount (pact-id))
          )
          (with-capability (BUY id seller buyer amount (pact-id))
            (buy id seller buyer buyer-guard amount)
          )
          (pact-id)
    ))
  )

  (defun offer:bool
    ( id:string
      seller:string
      amount:decimal
    )
    @doc "Initiate sale with by SELLER by escrowing AMOUNT of TOKEN until TIMEOUT."
    @model
      [ (property (!= id ""))
        (property (!= seller ""))
        (property (>= amount 0.0))
      ]
    (require-capability (SALE_PRIVATE (pact-id)))
    (let
      (
        (sender (debit id seller amount))
        (receiver (credit id (sale-account) (create-capability-pact-guard (SALE_PRIVATE (pact-id))) amount))
      )
      (emit-event (TRANSFER id seller (sale-account) amount))
      (emit-event (RECONCILE id amount sender receiver)))
  )

  (defun withdraw:bool
    ( id:string
      seller:string
      amount:decimal
    )
    @doc "Withdraw offer by SELLER of AMOUNT of TOKEN"
    @model
      [ (property (!= id ""))
        (property (!= seller ""))
        (property (>= amount 0.0))
      ]
    (require-capability (SALE_PRIVATE (pact-id)))
    (let
      (
        (sender (debit id (sale-account) amount))
        (receiver (credit-account id seller amount))
      )
      (emit-event (TRANSFER id (sale-account) seller amount))
      (emit-event (RECONCILE id amount sender receiver)))
  )

  (defun buy:bool
    ( id:string
      seller:string
      buyer:string
      buyer-guard:guard
      amount:decimal
    )
    @doc "Complete sale with transfer."
    @model
      [ (property (!= id ""))
        (property (!= seller ""))
        (property (!= buyer ""))
        (property (>= amount 0.0))
      ]
    (require-capability (SALE_PRIVATE (pact-id)))
    (let
      (
        (sender (debit id (sale-account) amount))
        (receiver (credit id buyer buyer-guard amount))
      )
      (emit-event (TRANSFER id (sale-account) buyer amount))
      (emit-event (RECONCILE id amount sender receiver))
      true
  ))

  (defun sale-active:bool (timeout:integer)
    @doc "Sale is active until TIMEOUT time."
    (if (= 0 timeout)
      true
      (< (at 'block-time (chain-data)) (add-time (time "1970-01-01T00:00:00Z") timeout))
    )
  )

  (defun sale-account:string ()
    (create-principal (create-capability-pact-guard (SALE_PRIVATE (pact-id))))
  )
)

(if (read-msg 'upgrade )
  (if (read-msg 'upgrade_version_1 )
    [ (create-table versions) ]
    ["upgrade complete"]
  )
  [ (create-table ledger)
    (create-table tokens) ])

(enforce-guard ADMIN-KS)
