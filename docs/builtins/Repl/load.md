## env-keys (DEPRECATED)

Load and evaluate a file, resetting repl state beforehand if optional RESET is
true.



### Basic syntax

To load a separate pact or repl file, call

```pact
(load "my-file.pact")
```

If the load requires resetting repl state, use

```pact
(load "my-file.pact" true)
```


## Arguments

Use the following argument when using the `env-keys` Pact function.

| Argument | Type     | Description                                                  |
|----------|----------|--------------------------------------------------------------|
| File     | string   | The file to load                                             |
| Reset    | bool     | (Optional) Reset the repl state before loading               |

### Return value

`load` returns the unit value `()`

### Example

The following example demonstrates how to use the `env-keys` function to set "my-key" and "admin-key" as the current transaction signing keys in a Pact REPL:

```pact
pact> (load "hello-world.repl")
"Hello pact!"
```
