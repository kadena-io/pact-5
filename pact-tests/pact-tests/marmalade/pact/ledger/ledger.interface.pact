(namespace (read-msg 'ns))

(interface ledger-v1

  (defcap INIT-CALL:bool (id:string precision:integer uri:string)
    @doc
      "Capability securing the modref call for enforce-init "
  )

  (defcap TRANSFER-CALL:bool (id:string sender:string receiver:string amount:decimal)
    @doc
    "Capability securing the modref call for enforce-transfer"
  )

  (defcap MINT-CALL:bool (id:string account:string amount:decimal)
    @doc
      "Capability securing the modref call for enforce-mint"
  )

  (defcap BURN-CALL:bool (id:string account:string amount:decimal)
    @doc
      "Capability securing the modref call for enforce-burn"
  )

  (defcap OFFER-CALL:bool (id:string seller:string amount:decimal timeout:integer sale-id:string)
    @doc
      "Capability securing the modref call for enforce-offer"
  )

  (defcap WITHDRAW-CALL:bool (id:string seller:string amount:decimal timeout:integer sale-id:string)
    @doc
      "Capability securing the modref call for enforce-withdraw"
  )

  (defcap BUY-CALL:bool (id:string seller:string buyer:string amount:decimal sale-id:string)
    @doc
      "Capability securing the modref call for enforce-buy"
  )
)
