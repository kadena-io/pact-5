## create-table

Use `create-table` to create a table identified by the specified `table` name.

### Basic syntax

To create a table identified by the specified `table` name, use the following syntax:

```pact
(create-table table)
```

### Prerequisites

Before using this function in a Pact module, you must define the table fields using the `defschema` declaration and the table identifier using the `deftable` declaration. 
Creating the table is a separate step that is outside of the Pact module where the schema and identifier are defined.

### Arguments

Use the following argument to specify the `table` name you want to create using the `create-table` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `table` | `table: <{row}>` | Specifies the table to create. |

### Return values

The `create-table` function returns a string representing the identifier of the created `table`.

### Example

The following example demonstrates how to use the `create-table` function to create a table identified by `accounts` that can be used for storing account information:

```pact
(create-table accounts)
```

The following example illustrates using the `create-table` function after defining the table schema and table identifier:

```pact
(defschema wallet-schema
	name : string
)
(deftable wallet-table:{wallet-schema})

...

(create-table wallet-table)
```