## describe-module

Use `describe-module` to get metadata for a specified `module`. 
This function returns an object with fields including `name`, `hash`, `blessed`, `code`, and `keyset`.

### Basic syntax

To get metadata for a `module`, use the following syntax:

```pact
(describe-module module)
```

### Arguments

Use the following argument to specify the `module` for the `describe-module` Pact function.

| Argument | Type   | Description                                  |
|----------|--------|----------------------------------------------|
| `module` | string | Specifies the name of the module to describe.|

### Return values

The `describe-module` function returns an object with metadata for the specified `module`.

### Examples

The following example demonstrates how to use `describe-module` function to describe a loaded module:

```pact
pact> (module election-gas-station GOVERNANCE (defcap GOVERNANCE () true))
"Loaded module election-gas-station, hash S6Dtzd7TzbRkYV6_5G5JRhjtF9Ztvaw7LuUEHEeRivQ"

pact> (describe-module "election-gas-station")
{
  "blessed": []
  ,"code": "(module election-gas-station GOVERNANCE (defcap GOVERNANCE () true))"
  ,"hash": "S6Dtzd7TzbRkYV6_5G5JRhjtF9Ztvaw7LuUEHEeRivQ"
  ,"interfaces": []
  ,"keyset": "Governance {_gGovernance = Right (Def {_dDefName = DefName {_unDefName = "GOVERNANCE"}, _dModule = ModuleName {_mnName = "election-gas-station", _mnNamespace = Nothing}, _dDefType = Defcap, _dFunType = FunType {_ftArgs = [], _ftReturn = TyVar {_tyVar = TypeVar {_tvName = "a", _tvConstraint = []}}}, _dDefBody = Scope (TList {_tList = [TLiteral {_tLiteral = LBool {_lBool = True}, _tInfo = true}], _tListType = TyAny, _tInfo = (defcap GOVERNANCE () true)}), _dMeta = Meta {_mDocs = Nothing, _mModel = []}, _dDefMeta = Nothing, _dInfo = (defcap GOVERNANCE () true)})}"
 ,"name": "election-gas-station"
}
```

In this example, `(describe-module 'm)` is used to get metadata for the module named 'm'. The function returns an object providing detailed information about the module.
