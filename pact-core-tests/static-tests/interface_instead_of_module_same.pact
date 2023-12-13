(module mod G
  (defcap G () true)

  (defun foo:string (a:string b:module{mod}) a)
  )
