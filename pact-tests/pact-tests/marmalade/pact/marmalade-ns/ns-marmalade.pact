(ns.write-registry (read-msg 'ns) (read-keyset 'marmalade-admin) true)
(define-namespace
  (read-msg 'ns)
  (read-keyset 'marmalade-user) (read-keyset 'marmalade-admin)
)
