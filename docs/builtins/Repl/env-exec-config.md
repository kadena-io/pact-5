## env-exec-config

Use `env-exec-config` to query or set execution configuration information for executing transactions in your testing environment.
You can use this function to set one or more of the following configuration flags:

- "AllowReadInLocal"
- "DisableHistoryInTransactionalMode"
- "DisableInlineMemCheck"
- "DisableModuleInstall"
- "DisableNewTrans"
- "DisablePact40"
- "DisablePact410"
- "DisablePact42"
- "DisablePact43"
- "DisablePact431"
- "DisablePact44"
- "DisablePact45"
- "DisablePact46"
- "DisablePact47"
- "DisablePact48"
- "DisablePact49"
- "DisablePactEvents"
- "DisableRuntimeReturnTypeChecking"
- "EnforceKeyFormats"
- "OldReadOnlyBehavior"
- "PreserveModuleIfacesBug"
- "PreserveModuleNameBug"
- "PreserveNsModuleInstallBug"
- "PreserveShowDefs"
  
### Basic syntax

To look up the current configuration settings, use the following syntax:

```pact
(env-exec-config)
```

To set one or more configuration flags, use the following syntax:

```pact
(env-exec-config [flags])
```

### Arguments

Use the following argument to specify the configuration data you want to set using the `env-exec-config` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `flags` | [string] | Specifies the configuration flags you want to be set for the execution environment. |

### Return value

The `env-exec-config` function returns the list of configuration flags that have been set for the execution environment.

### Examples

The following example demonstrates how to use the `env-exec-config` function to enforce key formats and disable Pact events:

```pact
(env-exec-config ["EnforceKeyFormats" "DisablePactEvents"])
["DisablePactEvents" "EnforceKeyFormats"]
```

The following example demonstrates how to unset previously-set configuration flags using the `env-exec-config` function:

```pact
(env-exec-config [])
[]
```

The following example demonstrates how to check the current configuration settings:

```pact
(env-exec-config)
[]
```
