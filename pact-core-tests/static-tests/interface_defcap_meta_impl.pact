(interface iface

  (defcap CAP:bool (a:integer)
    @managed a CAP-MGR
  )

  (defun CAP-MGR:integer (a:integer b:integer))
  )


(module m g
  (defcap g () true)
  (implements iface)

  (defcap CAP:bool (a:integer) true)

  (defun CAP-MGR:integer (a:integer b:integer) 1)
  )
