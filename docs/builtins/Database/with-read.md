## with-read

Use `with-read` to read a row from a specified table for a given key and bind columns according to provided bindings over subsequent body statements.

### Basic syntax

To read a row from a `table` and bind columns according to provided `bindings`, use the following syntax:

```pact
(with-read table key bindings)
```

### Arguments

Use the following arguments to specify the table, key, bindings, and body for execution using the `with-read` Pact special form.

| Argument | Type | Description |
| --- | --- | --- |
| `table` | `table: <{row}>` | Specifies the table from which to read the row. |
| `key` | string | Specifies the key for which to read the row. |
| `bindings` | `binding: <{row}>` | Specifies the bindings for columns to be bound. |
| `body` | any | Specifies the subsequent body statements to be executed. |

### Return value

The `with-read` is a special form returns the result of executing the provided body statements.

### Examples

The following example demonstrates how to use the `with-read` function to read a row from the `accounts` table for the specified key and bind the `balance` and `currency` columns for further processing:

```pact
(with-read accounts id { "balance":= bal, "currency":= ccy }
  (format "Balance for {} is {} {}" [id bal ccy]))
```

