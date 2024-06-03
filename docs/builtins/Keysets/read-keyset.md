## read-keyset

_key_&nbsp;`string` _&rarr;_&nbsp;`keyset`

Read KEY from message data body as keyset
`({ "keys": KEYLIST, "pred": PREDFUN })`. PREDFUN should resolve to a keys
predicate.

```pact
(read-keyset "admin-keyset")
```
