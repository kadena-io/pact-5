(module mgd-mod G
  (defcap G () true)
  (defcap C:bool (id:string) @managed notId foo true)
  (defun foo:string (a:string b:string) a)
  )
