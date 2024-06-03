## define-namespace

_namespace_&nbsp;`string` _user-guard_&nbsp;`guard` _admin-guard_&nbsp;`guard`
_&rarr;_&nbsp;`string`

Create a namespace called NAMESPACE where ownership and use of the namespace is
controlled by GUARD. If NAMESPACE is already defined, then the guard previously
defined in NAMESPACE will be enforced, and GUARD will be rotated in its place.

```lisp
(define-namespace 'my-namespace (read-keyset 'user-ks) (read-keyset 'admin-ks))
```

Top level only: this function will fail if used in module code.
