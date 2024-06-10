## list-module

Use `list-modules` to list the Pact modules that are available for loading.

### Basic syntax

To list the modules available for loading, use the following syntax:

```pact
list-modules
```

### Return value

The `list-modules` function returns a list of strings representing the available modules for loading.

### Examples

The following example demonstrates how to use the `list-modules` function in the Pact REPL:

```pact
pact> (list-modules)
["ns"]
```

In this example, only the namespace module (`"ns"`) is available for loading.
