(namespace (read-string 'ns))

(interface updatable-uri-policy-v1

  (defun enforce-update-uri:bool
    ( token:object{kip.token-policy-v2.token-info}
      new-uri:string
    )
    @doc "Updating the URI of a token."
    @model [
      (property (!= new-uri ""))
    ]
  )
)
