## env-module-admin

Use `env-module-admin` to acquire the module administrative rights for any module loaded in the REPL, bypassing any checks. 
This function is particularly useful for writing tests that require administrative rights such as the ability to write to non-upgradeable module tables in the REPL.

### Basic syntax

To acquire module administrative rights for the `module` specified, use the following syntax:

```pact
(env-module-admin module)
```

Note that the `module` must be passed in as a module reference.

## Arguments

Use the following argument when using the `env-module-admin` Pact function.

| Argument | Type | Description |
|----------|------|-------------|
| `module` | modref | Specifies the module for which you want to acquire module administrative rights. |

### Return value

On success, the `env-module-admin` function returns a string that indicates the module administrative rights have been acquired.

### Example

The following example demonstrates how to use the `env-module-admin` function to acquire the module administrative rights for a non upgradeable module.

```pact
pact> (begin-tx)
"Begin Tx 0"
pact> (module m g (defcap g () (enforce false "non-upgradeable"))
....>   (defschema foo a:integer)
....>   (deftable tbl:{foo})
....> )
Loaded module m, hash rzIEM6JcGI4sNeAPY8e_ygWmO8pKIx35ezD3_x5_GMg
pact>
pact> (create-table tbl)
"TableCreated"
pact> (commit-tx)
"Commit Tx 0"
pact>
pact> (begin-tx)
"Begin Tx 1"
pact> (env-module-admin m) ; Acquired admin rights to `m`'s tables.
"Acquired module admin for: m"
pact> (write m.tbl "my-key" {"a":100})
"Write succeeded"
pact> (commit-tx)
"Commit Tx 1"
```
