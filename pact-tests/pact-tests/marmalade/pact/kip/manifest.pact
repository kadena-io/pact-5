(namespace 'kip)

(module token-manifest GOVERNANCE

  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard 'marmalade-admin )))

  (defschema mf-uri
    scheme:string
    data:string
  )

  (defschema mf-datum
    uri:object{mf-uri}
    hash:string
    datum:object
  )

  (defschema manifest
    uri:object{mf-uri}
    hash:string
    data:[object{mf-datum}]
  )

  (defun hash-contents:string
    ( uri:object{mf-uri}
      hashes:[string]
    )
    (hash {'uri: uri, 'data: hashes})
  )

  (defun create-manifest:object{manifest}
    ( uri:object{mf-uri}
      data:[object{mf-datum}]
    )
    { 'uri: uri
    , 'hash: (hash-contents uri (map (at 'hash ) data))
    , 'data: data
    }
  )

  (defun create-datum:object{mf-datum}
    ( uri:object{mf-uri}
      datum:object
    )
    { 'uri: uri
    , 'hash: (hash-contents uri [(hash datum)])
    , 'datum: datum
    }
  )

  (defun verify-manifest:bool
    ( manifest:object{manifest}
    )
    (bind manifest
      { "uri":= uri
      , "data":= data
      }
      (= (create-manifest uri data) manifest)
    )
  )

  (defun enforce-verify-manifest:bool
    ( manifest:object{manifest}
    )
    (enforce
      (verify-manifest manifest)
      "Manifest is not valid")
  )

  (defun uri (scheme:string data:string)
    {'scheme: scheme, 'data: data }
  )

)
