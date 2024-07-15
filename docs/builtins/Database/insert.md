## insert

Use `insert` to write an entry in a specified `table` for a given `key` of `object` data.
This operation fails if data already exists for the specified key.

### Basic syntax

To insert data into a table for a specified key, use the following syntax:

```pact
(insert table key object)
```

### Arguments

Use the following arguments to specify the table, key, and object data you want to insert using the `insert` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `table` | table<{row}> | Specifies the table where the entry will be written. |
| `key` | string | Specifies the key for which the data will be inserted. |
| `object` | object | Specifies the object data to be inserted for the specified key. |

### Return value

The `insert` function returns a string indicating the success or an exception on failure of the operation.

### Examples

The following example demonstrates how to use the `insert` function to insert information into the `accounts` table for the account specified using the `id` for the account:

```pact
(insert accounts id { "balance": 0.0, "note": "Created account." })
```

The following example illustrates a more complete flow from defining the table schema to inserting values into the table:

```pact
(defschema coin-schema
    balance:decimal
    guard:guard)

(deftable coin-table:{coin-schema})

(defun create-account:string (account:string guard:guard)

    (insert coin-table account
      { "balance" : 0.0
      , "guard"   : guard
      })
)
```