## typecheck

Use `typecheck` to run the Pact static type checker on a specified `module` name.

### Basic syntax

To run the static type checker on a specified `module` name, use the following syntax:

```pact
(typecheck module)
```

### Arguments

Use the following argument when calling the `typecheck` function:

| Argument | Type | Description |
|----------|------|-------------|
| `module` | string | Specifies the name of the module to run the static type checker on. |

### Return value

If type checking for the module is successful, the `typecheck` function returns the unit value `()`. 
If type checking fails, the function returns an error.

### Examples

The following example demonstrates a simple `.repl` file with the module declaration for a `rewards` module that then calls the static type checker to check the `rewards` module:

```pact
(module rewards GOV
  (defcap GOV () true)

  (defun multiplier (points) (* points 10))
)

(typecheck "rewards")
```

If you execute the code in the file by running `pact rewards.repl --trace`, you see the results of type checking for the module.
For example:

```bash
rewards.repl:0:0-4:1:Trace: Loaded module rewards, hash d6qkp1SyjmFCUofnsMpdV2W3IOLH8VA9lg0Dqv4cN_M
rewards.repl:6:0-6:21:Trace: Typechecking successful for module rewards
Load successful
```

If you specify a namespace before the module declaration, you must include the namespace when you call the `typecheck` function.
For example, if the `rewards` module declaration comes after entering the `(namespace "develop")` namespace, you would call the `typecheck` function like this:

```pact
(typecheck "develop.rewards")
```