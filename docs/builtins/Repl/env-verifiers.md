## env-verifiers

Use `env-verifiers` to set transaction verifier names and capabilities. This function enables you to define a list of objects with the "name" key specifying the verifier name, and the "caps" key specifying a list of associated capabilities.

### Basic syntax

To set the signature keys for transaction verifiers, use the following syntax:

```pact
(env-verifiers [verifiers])
```

Each verifier is an object that consists of a verifier name and a list of capabilities in the following format:

```pact
(env-verifiers [{"name":verifier_name, "caps":[capabilities]}])
```

For each object, the `verifier_name` is the verifier signature to add to scope, and `caps` is the list of capabilities that the `verifier_name` is scoped to.

### Arguments

Use the following argument to set verifier information using the `env-verifiers` Pact function.

| Argument | Type         | Description   |
|----------|--------------|---------------|
| `verifiers`| [object] | Specifies the list verifier objects where each object represents a verifier `name` to add to scope and a list of associated capabilities that the verifier is scoped to.|

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
