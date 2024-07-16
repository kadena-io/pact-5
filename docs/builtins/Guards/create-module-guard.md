## create-module-guard

Use `create-module-guard` to create a predicate function with the specified `name` that ensures that specific conditions are true for the current module.

Module guards are typically used to enable a module to perform administrative operations independently outside of the module itself, for example, to own coins in an external ledger, or to perform administrative operations internally on its database, for example, to own and manage certain assets.

### Basic syntax

To define a predicate function `name` that guards administrative activity for the current module, use the following syntax:

```pact
(create-module-guard name)
```

### Arguments

Use the following argument to specify the `name` for the `create-module-guard` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `name` | string | Specifies the name of the predicate function that guards administrative activity for the current module. |

### Return values

The `create-module-guard` function returns a guard with the specified `name`that enables the current module to perform administrative operations.

### Example

The following example demonstrates how to use the `create-module-guard` function to define a guard named `"module-admin-guard"` for the current module:

```pact
(create-module-guard "module-admin-guard")
```

If the evaluation of the `module-admin-guard` returns true, the current module is granted administrative privileges.
