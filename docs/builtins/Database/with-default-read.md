## with-default-read

Use `with-default-read` to read a row from a specified table for a given key and bind columns according to provided bindings. 
If the row is not found, the function reads columns from a default object with matching key names.

### Basic syntax

To read a row from a `table` with `default` values and bind columns according to the provided `binding`, use the following syntax:

```pact
(with-default-read table key default bindings)
```

### Arguments

Use the following arguments to specify the table, key, defaults, bindings, and body for execution using the `with-default-read` Pact special form.

| Argument | Type | Description |
| --- | --- | --- |
| `table` | `table: <{row}>` | Specifies the table from which to read the row. |
| `key` | string | Specifies the key for which to read the row. |
| `default` | object | Specifies a default object containing values for missing columns. |
| `bindings` | binding:<{row}> | Specifies the bindings for columns to be bound. |
| `body` | any | Specifies the subsequent body statements to be executed. |

### Return value

The `with-default-read` is a special form that returns the result of executing the provided body statements.

### Examples

The following example demonstrates how to use the `with-default-read` function to read a row from the `accounts` table for the specified key, using default values if the row is not found, and binds the `balance` and `currency` columns for further processing:

```pact
(with-default-read accounts id { "balance": 0, "currency": "USD" } { "balance":= bal, "currency":= currency }
  (format "Balance for {} is {} {}" [id bal currency]))
```

This example illustrates reading the `accounts` table with the row key of `id` and setting default values of `0` and `USD` for the `balance` and `currency` columns if the row isn't found to format a default message using the `format` function.
