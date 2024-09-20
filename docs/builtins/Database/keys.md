## keys

Use `keys` to retrieve all the keys in a specified table.

### Basic syntax

To retrieve all of the keys for a specified `table`, use the following syntax:

```pact
(keys table)
```

### Arguments

Use the following argument to specify the table from which to retrieve keys using the `keys` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `table` | `table: <{row}>` | Specifies the table from which to retrieve keys. In the table schema, a `row` represents the structure of each row in the table. |

### Return values

The `keys` function returns an array of strings, where each string represents a key in the specified table.

### Examples

The following example demonstrates defining a table schema for an `accounts` table that stores information about bank accounts, including the balance and owner of each account:

```pact
(defschema account
  balance:decimal
  owner:string)

(deftable accounts:{account})
```

You can then retrieve all of the the keys from the `accounts` table using the `keys` function:

```pact
pact> (keys accounts)
[]
```
