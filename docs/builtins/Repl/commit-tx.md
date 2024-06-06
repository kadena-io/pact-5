## commit-tx

Use `commit-tx` to commit the current transaction.

### Basic syntax

`(commit-tx)`

### Arguments

The `commit-tx` function does not take any arguments.

### Return value

The `commit-tx` function returns a string indicating the transaction identifier that has been committed.

### Example

The following example demonstrates the usage of the `commit-tx` function within a Pact REPL:

```pact
pact> (begin-tx) (commit-tx)
"Commit Tx 0"
```

In this example, a new transaction is started using `(begin-tx)`, and then `(commit-tx)` is called to commit the transaction. The function returns a string indicating that "Tx 0" has been committed.

Note that `commit-tx` should be used after performing the necessary operations within a transaction. Once `commit-tx` is called, the transaction is finalized, and any changes made during the transaction are persisted.

It's important to ensure that `commit-tx` is called after `begin-tx` and any other transaction-related operations to properly commit the transaction. If `commit-tx` is not called, the transaction will remain open and may lead to unexpected behavior or inconsistencies in the system.
