## mock-spv

_type_&nbsp;`string` _payload_&nbsp;`object:*` _output_&nbsp;`object:*` _&rarr;_&nbsp;`string`

Mock a successful call to 'spv-verify' with TYPE and PAYLOAD to return OUTPUT.

```pact
(mock-spv "TXOUT" { 'proof: "a54f54de54c54d89e7f" } { 'amount: 10.0, 'account: "Dave", 'chainId: "1" })
```
