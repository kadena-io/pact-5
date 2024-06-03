## env-data

_json_&nbsp;`<a[integer,string,time,decimal,bool,[<l>],object:<{o}>,keyset]>` _&rarr;_&nbsp;`string`

Set transaction JSON data, either as encoded string, or as pact types coerced to JSON.

```bash
pact> (env-data { "keyset": { "keys": ["my-key" "admin-key"], "pred": "keys-any" } })
"Setting transaction data"
```
