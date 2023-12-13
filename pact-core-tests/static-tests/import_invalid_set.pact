(module m mg
  (defcap mg () true)

  (defun mfn () 2)
  )

(module n ng
  (defcap ng () true)

  (use m [ nonexistent ])
  )
