## env-gasmodel

_model_&nbsp;`string` _&rarr;_&nbsp;`string`

_&rarr;_&nbsp;`string`

_model_&nbsp;`string` _rate_&nbsp;`integer` _&rarr;_&nbsp;`string`

Update or query current gas model. With just MODEL, "table" is supported; with MODEL and RATE, 'fixed' is supported. With no args, output current model.

```bash
pact> (env-gasmodel)
"Current gas model is 'fixed 0': constant rate gas model with fixed rate 0"
pact> (env-gasmodel 'table)
"Set gas model to table-based cost model"
pact> (env-gasmodel 'fixed 1)
"Set gas model to constant rate gas model with fixed rate 1"
```
