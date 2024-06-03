## env-dynref

_iface_&nbsp;`module` _impl_&nbsp;`module{}` _&rarr;_&nbsp;`string`

_&rarr;_&nbsp;`string`

Substitute module IMPL in any dynamic usages of IFACE in typechecking and analysis. With no arguments, remove all substitutions.

```pact
(env-dynref fungible-v2 coin)
```
