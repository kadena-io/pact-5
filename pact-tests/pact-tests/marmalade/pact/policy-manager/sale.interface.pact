(namespace (read-msg 'ns))

(interface sale-v2

  " Standard for marmalade-v2 sale contracts to be implemented in quoted sales."

  (defun enforce-quote-update:bool (sale-id:string price:decimal)
    @doc "Read-only Function that is enforced to update quote price at enforce-buy"
  )

  (defun enforce-withdrawal:bool (sale-id:string)
    @doc "Read-only Function that is enforced to allow withdrawal from sale"
  )
)
