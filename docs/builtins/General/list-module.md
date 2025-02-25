## list-modules

Use `list-modules` to list the modules that are deployed in your current environment and available for loading.

### Basic syntax

To list the modules deployed and available for loading, use the following syntax:

```pact
(list-modules)
```

## Arguments

The `list-modules` function takes no arguments.

### Return value

The `list-modules` function returns a list of strings representing the modules that are deployed in your current environment and available for loading.

### Examples

The following example demonstrates how to use the `list-modules` function in the Pact REPL:

```pact
pact> (list-modules)
["coin" "fungible-v2" "fungible-xchain-v1" "gas-payer-v1"]
```

In this example, the contract modules that are deployed by default on the development, test, and main network are listed as available for loading.

If there are no contracts deployed, the `list-modules` function returns a empty list.
For example:

```pact
pact> (list-modules)
[]
```
