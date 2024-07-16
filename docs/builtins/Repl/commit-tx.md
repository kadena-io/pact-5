## commit-tx

Use `commit-tx` to commit the current transaction.
In most cases, you include this function in `.repl` files that test specific application features and failure scenarios.

Within the context of a transaction started using the `begin-tx` function, you can set environment data, load modules, and execute functions.
You use the `commit-tx` function to signal the end of a transaction block and can be followed by additional `begin-tx` and `commit-tx` blocks.

### Basic syntax

To commit a transaction, use the following syntax:

```pact
(commit-tx)
```

### Arguments

The `commit-tx` function does not take any arguments.

### Return value

The `commit-tx` function returns a string indicating the transaction identifier that has been committed.

### Examples

The following example demonstrates how to use the `commit-tx` function within a Pact REPL:

```pact
pact> (begin-tx "load module")
"Begin Tx 0: load module"
pact> (commit-tx)
"Commit Tx 0: load module"
```

In this example, a new transaction—Tx0—is started using `(begin-tx)`, and then `(commit-tx)` is called to commit the transaction. 
The function returns a string indicating that "Tx 0" has been committed.

Note that you should only call the `commit-tx` function after performing the necessary operations within a transaction. 
The `commit-tx` function finalizes the transaction, and any changes made during the transaction are persisted.

It's important that you always call the `commit-tx` function after `begin-tx` and after any other transaction-related operations to properly commit the transaction. 
If `commit-tx` is not called, the transaction remains open and may lead to unexpected behavior or inconsistencies in the system.
