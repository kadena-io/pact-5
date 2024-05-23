## describe-table
Use `describe-table` to get metadata for a specified `TABLE`. This function returns an object with fields including `module`, `name` and `type`.

### Basic syntax

To get metadata for a `TABLE`, use the following syntax:

`(describe-table TABLE)`

### Arguments

Use the following argument to specify the `TABLE` for the `describe-table` Pact function.

| Argument | Type          | Description                                  |
|----------|---------------|----------------------------------------------|
| `table`    | `table:<{row}>` | Specifies the table to describe.             |

### Return values

The `describe-table` function returns an object with metadata for the specified `TABLE`.

### Examples

The following example demonstrates the `describe-table` function:

```lisp
pact>(module m G (defcap G () true) (defschema s i:integer) (deftable t:{s}))
Loaded module m, hash UAnq05ArrOYCFbeJDjCLpWecBq5bS5I0WA6Mj0O041o
pact>(describe-table m.t)
{"module":"m", "name":"t", "type":"table{m.s}"}
```

In this example, `(describe-table accounts)` is used to get metadata for the table named 't'. The function returns an object with fields such as `module`, `name` and `type`, providing detailed information about the table.
