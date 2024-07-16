## env-data

Use `env-data` to set transaction data for your testing environment either as an encoded string or as Pact types coerced to JSON format.

### Basic syntax

To set transaction data, use the following syntax:

```pact
(env-data json)
```

### Arguments

Use the following argument to specify the JSON data when using the `env-data` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `json` | object | Specifies the JSON data to be set for the transaction. The data can be provided as an encoded string or as Pact types that will be coerced to JSON. |

### Return value

The `env-data` function returns a string indicating that the transaction data is being set.

### Examples

The following example demonstrates the usage of the `env-data` function within a Pact REPL:

```pact
pact> (env-data { "keyset": { "keys": ["my-key" "admin-key"], "pred": "keys-any" } })
"Setting transaction data"
```

In the following example, the `env-data` function is used to set up a mock token identifier and account information for testing Marmalade functions:

```pact
(env-data {
  "token-id": "t:YV6-cQBhE_EoIXAuNV08aGXLfcucBEGy0Gb1Pj6w_Oo"
  ,"account": "k:e4c6807d79d8bf4695e10e5678ebf72862f59b71f971d39dd3349f4beeacd6e3"
  ,"account-guard": {
      "keys": ["e4c6807d79d8bf4695e10e5678ebf72862f59b71f971d39dd3349f4beeacd6e3"], 
      "pred": "keys-all"
    }
  }
)
```