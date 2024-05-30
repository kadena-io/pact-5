(namespace 'util)

(module fungible-util GOVERNANCE
  (implements kip.account-protocols-v1)

  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard 'util-ns-admin)))

  (defun enforce-valid-amount
    ( precision:integer
      amount:decimal
    )
    (enforce (> amount 0.0) "Positive non-zero amount")
    (enforce-precision precision amount)
  )

  (defun enforce-precision
    ( precision:integer
      amount:decimal
    )
    (enforce
      (= (floor amount precision) amount)
      "precision violation")
  )

  (defun enforce-valid-transfer
    ( sender:string
      receiver:string
      precision:integer
      amount:decimal)
    (enforce (!= sender receiver)
      "sender cannot be the receiver of a transfer")
    (enforce-valid-amount precision amount)
  )

  (defun enforce-reserved:bool (account:string guard:guard)
    @doc "Enforce reserved account name protocols."
    (enforce (validate-principal guard account) "Incorrect account guard")
    true
  )

)
