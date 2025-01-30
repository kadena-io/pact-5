## env-verifiers

Set transaction verifier names and capabilities. VERIFIERS is a list of
  objects with "name" specifying the verifier name, and "caps" specifying a list
  of associated capabilities.

### Basic syntax

To set the signature keys to set the transaction verifiers to mock out

```pact
(env-verifiers [{"name":verifier_name, "caps":[capabilities]}])
```

where `verifier_name` is the verifier to add to scope, and `caps` is the capabilities that the verifier is scoped to.

### Arguments

Use the following argument when using the `env-verifiers` Pact function.

| Argument | Type         | Description   |
|----------|--------------|---------------|
| `verifiers`  | [object] | Specifies the list verifier objects. Each object represents a verfier `name` and its associated `caps` capabilities. |

### Return value

The `env-verifiers` function returns a string indicating that the transaction verifiers have been set.

### Examples

The following example illustrates using the `env-verifiers` function to grant the `"COOLZK"` and `"HYPERCHAIN-BRIDGE"` verifiers.

```pact
pact> (module accounts g (defcap g () true) (defcap USER_GUARD (user:string) true))
Loaded module accounts, hash 8aj3ezifsdCMX3l-A7p6Axbd59YJRsGR4MgDoR9qPgc
pact> (module bridge g (defcap g () true) (defcap MINT (coin:string amount:integer) true))
Loaded module bridge, hash 9r3-A9e5vYGyKXTHTQVVq58Z2AJ4vWQeTP1uCTmZdPA
pact> (env-verifiers [{'name: "COOLZK", 'caps: [(accounts.USER_GUARD "my-account")]}, {'name: "HYPERCHAIN-BRIDGE", 'caps: [(bridge.MINT "mycoin" 20)]}])
"Setting transaction verifiers/caps"
```
