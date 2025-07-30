## enforce-verifier

Use `enforce-verifier` to enforce that a verifier plugin with the specified `verifier-name` is in scope.

Note that verifier plugins aren't implemented as Pact code.
Instead, implementing a verifier plugin requires changes to the Chainweb protocol.
For information about the design and implementation of verifier plugins, see [KIP-0028: Pact Verifier Plugins](https://github.com/kadena-io/KIPs/blob/master/kip-0028.md).

### Basic syntax

To enforce that a verifier is in scope, use the following syntax:

```pact
(enforce-verifier verifier-name)
```

### Arguments

Use the following argument to specify the `verifier-name` for the `enforce-verifier` Pact function:

| Argument    | Type   | Description                                     |
|-------------|--------|-------------------------------------------------|
| `verifier-name` | string | Specifies the name of the verifier to enforce.  |

### Return value

The `enforce-verifier` function returns a boolean value indicating whether the specified verifier is in scope.

### Examples

The following example demonstrates the use of a `signed_list` verifier plugin that verifies signatures against structured message data to ensure the integrity and authenticity of messages provided to the `mod` module in a smart contract.

```pact
(namespace "free")

(module mod GOV
  (defcap GOV () true)
  (defcap MSG-GUARD (msg:list dst-fin-id:string)
    (enforce-verifier "signed_list")
  )

  (defun inbox ()
    (with-capability 
      (MSG-GUARD [["issue","finp2p","citi:102:d0c3eb56-0fff-4670-adfd-ad291a4314c3",
           "finId","02fd7923740a775c95ce17e9bb7239ff9096689f70db9263a7efb9a9ad08e9fed7","1"]]
         "02fd7923740a775c95ce17e9bb7239ff9096689f70db9263a7efb9a9ad08e9fed7")
      )
      1)
)
```

In this example, the capability MSG-GUARD uses the `enforce-verifier` function with the `signed_list` verifier plugin. 
The arguments that are passed to the capability are then evaluated by the verifier to ensure the integrity and authenticity of the message.

The following example illustrates using the `enforce-verifier` function in the capability definition, then using the `env-verifiers` function in the Pact REPL to add the "COOLZK" and "HYPERCHAIN-BRIDGE" verifier plugins to the environment data.

```pact
(module accounts GOV 
  (defcap GOV () true) 
  (defcap USER_GUARD (user:string) 
    (enforce-verifier "COOLZK"))
  (defun create-user (user:string)
    (with-capability USER_GUARD "rae")
  )
)
(module bridge GOV 
  (defcap GOV () true) 
  (defcap MINT (coin:string amount:integer) 
    (enforce-verifier "HYPERCHAIN_BRIDGE"))
)
(env-verifiers [{'name: "COOLZK", 'caps: [(accounts.USER_GUARD "my-account")]}, {'name: "HYPERCHAIN-BRIDGE", 'caps: [(bridge.MINT "mycoin" 20)]}])
```

In this example, the verifiers are loaded in the environment so that they are in scope for the `enforce-verifier` function:

```pact
enforce-verifier-test.repl:0:0-7:1:Trace: Loaded module accounts, hash NXmdjAMHjjJSh-JUwTlT0IsOwZiXg_ZytI8Q3B2Xz4o
enforce-verifier-test.repl:8:0-12:1:Trace: Loaded module bridge, hash E2h3NLl5ePmKGvd8aNvFHyo-LONVsbxHXg8giuY3kuc
enforce-verifier-test.repl:13:0-13:146:Trace: "Setting transaction verifiers/caps"
Load successful
```

If the required verifier plugins are in scope, the `enforce-verifier` function returns `true`.
If a verifier is not in scope, the function fails. 

```pact
pact> (enforce-verifier "COOLZK")
<interactive>:0:0:Error: Verifier failure COOLZK: not in transaction
```

The `enforce-verifier` function provides a way to ensure that a specific verifier is available for use within a Pact contract.

For more information about including verifier plugins in environment data for testing, see [`env-verifiers`](/pact-5/repl/env-verifiers).