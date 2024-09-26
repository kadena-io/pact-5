## list-modules

Use `list-modules` to get all of the modules deployed on a particular chain.

Note: This function is only allowed in `/local` in chainweb.

### Basic syntax

To list all modules in `/local`, use

```pact
(list-modules)
```

### Arguments

None

### Return values

The `list-modules` function returns a list of all module names (including their namespace) stored on-chain

### Examples

The following example deploys a simple module in the repl, then calls `list-modules`.

```
pact>(module my-module gov (defcap gov () true) (defun say-hello () (do "hello world!")))
Loaded module my-module, hash Z5GCj0v6YdJTPbcc37eUYYeRiPuJGie1XFRXG16il68
pact>(list-modules)
["my-module"]
```
