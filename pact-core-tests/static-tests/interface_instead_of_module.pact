(module mod G
  (defcap G () true)
  )

(module other-mod OG
  (defcap OG () true)

  (defun foo:string (a:string b:module{mod}) a)
  )
