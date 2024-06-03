## env-gas

_&rarr;_&nbsp;`integer`

_gas_&nbsp;`integer` _&rarr;_&nbsp;`string`

Query gas state, or set it to GAS. Note that certain plaforms may charge additional gas that is not captured by the interpreter gas model, such as an overall transaction-size cost.

```bash
pact> (env-gasmodel "table") (env-gaslimit 10) (env-gas 0) (map (+ 1) [1 2 3]) (env-gas)
7
```
