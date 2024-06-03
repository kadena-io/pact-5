## env-sigs

_sigs_&nbsp;`[object:*]` _&rarr;_&nbsp;`string`

Set transaction signature keys and capabilities. SIGS is a list of objects with "key" specifying the signer key, and "caps" specifying a list of associated capabilities.

```pact
(env-sigs [{'key: "my-key", 'caps: [(accounts.USER_GUARD "my-account")]}, {'key: "admin-key", 'caps: []}
```
