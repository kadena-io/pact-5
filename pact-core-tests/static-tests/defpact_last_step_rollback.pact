(module m g
  (defcap g () true)

  (defpact f ()
    (step-with-rollback 1 1)
  )
  )
