## static-redeploy

Use `static-redeploy` to redeploy any module, without _any_ code changes. Redeploying a legacy module will store the new module in the new Pact 5 compact storage format.
As a result, redeployed modules require significantly less gas to load.

Note that redeploying leaves governance unchanged.

### Prerequisites

You must have deployed the module in a namespace on at least one chain in the development, test, or main network before you can use the `static-redeploy` function to update the module storage. 
For information about deploying a module, see [Deploy smart contracts](/guides/contracts/howto-deploy-contracts).

### Basic syntax

To redeploy an existing module as a Pact 5 module, use the following syntax:

```pact
(static-redeploy module)
```

### Arguments

Use the following argument to specify the module you want to redeploy using the `static-redeploy` Pact function.

| Argument | Type | Description
| -------- | ---- | -----------
| `module` | string | Specifies the module to redeploy.

### Return value

The function returns the unit value `()` if redeployment is successful.

### Examples

The following example demonstrates how to redeploy the `pistolas-vote` module that was originally deployed in the `free` namespace on `testnet04` and chain `3` using a YAML transaction request file.

```yaml
code: |
  (static-redeploy "free.pistolas-vote")
data:
  vote:
    keys: ["401d6346...114c5ae4"]
    pred: "keys-all"
publicMeta:
  chainId: "3"
  sender: "k:401d6346...114c5ae4"
  gasLimit: 100000
  gasPrice: 0.0000001
  ttl: 7200
networkId: "testnet04"
keyPairs:
  - public: "401d6346...114c5ae4"
    secret: "94df4ba5...2709fd1a"
    caps:
      - name: "coin.GAS"
        args: []
  - public: "401d6346...114c5ae4"
    secret: "94df4ba5...2709fd1a"
    caps: []
type: exec
```

After creating the YAML request, you can format and submit the transaction request to the appropriate network and chain `/send` endpoint.
For example:

```bash
pact --apireq vote-redeploy.yaml > redeploy.json
curl -X POST -H "Content-Type: application/json" -d "@redeploy.json" \
 https://api.testnet.chainweb.com/chainweb/0.0/testnet04/chain/3/pact/api/v1/send
```