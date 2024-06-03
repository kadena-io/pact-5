## enforce-guard

_guard_&nbsp;`guard` _&rarr;_&nbsp;`bool`

_keysetname_&nbsp;`string` _&rarr;_&nbsp;`bool`

Execute GUARD, or defined keyset KEYSETNAME, to enforce desired predicate logic.

```pact
(enforce-guard 'admin-keyset)
(enforce-guard row-guard)
```
