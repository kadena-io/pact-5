## keys

Use `keys` to retrieve all the keys in a specified table.

### Basic syntax

`(keys table)`

### Arguments

Use the following argument to specify the table from which to retrieve keys using the `keys` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `table` | `table` | Specifies the table from which to retrieve keys. The table schema is `table:<{row}>`, where `row` represents the structure of each row in the table. |

### Return values

The `keys` function returns an array of strings, where each string represents a key in the specified table.

### Examples

Suppose we have a table named `accounts` with the following schema:

```pact
(defschema account
  balance:decimal
  owner:string)

(deftable accounts:{account})
```

The `accounts` table stores information about bank accounts, including the balance and owner of each account.

The following example retrieves all the keys from the `accounts` table:

```pact
pact>(keys accounts)
[]
```
