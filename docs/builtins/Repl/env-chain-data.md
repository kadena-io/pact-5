## env-chain-data

_new-data_&nbsp;`object:~{public-chain-data}` _&rarr;_&nbsp;`string`

Update existing entries of 'chain-data' with NEW-DATA, replacing those items only.

```bash
pact> (env-chain-data { "chain-id": "TestNet00/2", "block-height": 20 })
"Updated public metadata"
```
