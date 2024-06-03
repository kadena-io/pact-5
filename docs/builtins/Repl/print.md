## print

_value_&nbsp;`<a>` _&rarr;_&nbsp;`string`

Output VALUE to terminal as unquoted, unescaped text.

## rollback-tx

_&rarr;_&nbsp;`string`

Rollback transaction.

```bash
pact> (begin-tx "Third Act") (rollback-tx)
"Rollback Tx 0: Third Act"
```
