## env-exec-config

_flags_&nbsp;`[string]` _&rarr;_&nbsp;`[string]`

_&rarr;_&nbsp;`[string]`

Queries, or with arguments, sets execution config flags. Valid flags: ["AllowReadInLocal","DisableHistoryInTransactionalMode","DisableInlineMemCheck","DisableModuleInstall","DisableNewTrans","DisablePact40","DisablePact420","DisablePact43","DisablePact431","DisablePact44","DisablePact45","DisablePact46","DisablePact47","DisablePact48","DisablePact49","DisablePactEvents","DisableRuntimeReturnTypeChecking","EnforceKeyFormats","OldReadOnlyBehavior","PreserveModuleIfacesBug","PreserveModuleNameBug","PreserveNsModuleInstallBug","PreserveShowDefs"]

```bash
pact> (env-exec-config ['DisableHistoryInTransactionalMode]) (env-exec-config)
["DisableHistoryInTransactionalMode"]
```
