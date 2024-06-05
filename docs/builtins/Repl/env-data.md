## env-data

Use `env-data` to set transaction JSON data, either as an encoded string or as Pact types coerced to JSON.

### Basic syntax

`(env-data json)`

### Arguments

Use the following argument to specify the JSON data when using the `env-data` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `json` | `<a[integer,string,time,decimal,bool,[<l>],object:<{o}>,keyset]>` | Specifies the JSON data to be set for the transaction. The data can be provided as an encoded string or as Pact types that will be coerced to JSON. |

### Return value

The `env-data` function returns a string indicating that the transaction data is being set.

### Example

The following example demonstrates the usage of the `env-data` function within a Pact REPL:

```pact
pact> (env-data { "keyset": { "keys": ["my-key" "admin-key"], "pred": "keys-any" } })
"Setting transaction data"
```
