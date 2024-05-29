(namespace 'kip)
(interface account-protocols-v1

  " Define a standard for support of Kadena Account Protocols, \
  \ which reserve account names starting with 'X:'' where X is a \
  \ single latin-1 character. It also indicates which protocols are \
  \ supported."

  (defconst SINGLE_KEY "k:"
    " Protocol in which the data portion of the name must match the \
    \ key in a single-key, 'keys-all' guard."
  )

  (defun enforce-reserved:bool
    ( account:string
      guard:guard
    )
    @doc " Enforce reserved account name protocols. Implementations \
         \ must call this function in all account creation modes \
         \ (transfer-create, create-account, etc)."
  )

)
