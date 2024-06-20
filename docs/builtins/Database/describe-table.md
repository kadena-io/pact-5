## describe-table

Use `describe-table` to get metadata for a specified `table`. 
This function returns an object with fields including `module`, `name`, and `type`.

### Basic syntax

To get metadata for a specified `table`, use the following syntax:

```pact
(describe-table TABLE)
```

### Arguments

Use the following argument to specify the `table` for the `describe-table` Pact function.

| Argument | Type          | Description                                  |
|----------|---------------|----------------------------------------------|
| `table`    | table:<{row}> | Specifies the table to describe.             |

### Return values

The `describe-table` function returns an object with metadata for the specified `table`.

### Examples

The following example demonstrates use to use the `describe-table` function in the Pact REPL by loading a module that has a table definition:

```pact
pact> (module ledger GOVERNANCE (defcap GOVERNANCE () true) (defschema token-schema id:string uri:string precision:integer supply:decimal) (deftable tokens:{token-schema}))
```

The Pact REPL loads the module and displays the has with output similar to the following:

```text
Loaded module m, hash UAnq05ArrOYCFbeJDjCLpWecBq5bS5I0WA6Mj0O041o
```

```pact
pact> (describe-table tokens)
{"module": "ledger"
,"name": "tokens"
,"type": "(defschema token-schema  [id:string, uri:string, precision:integer, supply:decimal])"}
```
