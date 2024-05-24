## with-read
The `with-read` special form is used to read a row from a specified table for a given key and bind columns according to provided bindings over subsequent body statements.

### Basic syntax

To read a row from a `TABLE` and bind columns according to provided `BINDINGS`, use the following syntax:

`(with-read TABLE KEY BINDINGS)`

### Arguments

Use the following arguments to specify the table, key, bindings, and body for execution using the `with-read` Pact special form.

| Argument | Type | Description |
| --- | --- | --- |
| `TABLE` | `table:<{row}>` | Specifies the table from which to read the row. |
| `KEY` | `string` | Specifies the key for which to read the row. |
| `BINDINGS` | `binding:<{row}>` | Specifies the bindings for columns to be bound. |
| `BODY` | `<a>` | Specifies the subsequent body statements to be executed. |

### Return value

The `with-read` special form returns the result of executing the provided body statements.

### Examples

The following example demonstrates the usage of the `with-read` special form within a Pact script. It reads a row from the `accounts` table for the specified key and binds the 'balance' and 'ccy' columns for further processing:

```pact
(with-read accounts id { "balance":= bal, "ccy":= ccy }
  (format "Balance for {} is {} {}" [id bal ccy]))
```

This example illustrates how to use the `with-read` special form to read data from a table and bind specific columns for further operations in Pact, facilitating efficient data retrieval and processing.
