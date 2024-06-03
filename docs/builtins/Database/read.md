## read

_table_&nbsp;`table:<{row}>` _key_&nbsp;`string` _&rarr;_&nbsp;`object:<{row}>`

_table_&nbsp;`table:<{row}>` _key_&nbsp;`string` _columns_&nbsp;`[string]`
_&rarr;_&nbsp;`object:<{row}>`

Read row from TABLE for KEY, returning database record object, or just COLUMNS
if specified.

```pact
(read accounts id ['balance 'ccy])
```
