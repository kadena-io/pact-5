## env-namespace-policy

_allow-root_&nbsp;`bool` _ns-policy-fun_&nbsp;`ns:string ns-admin:guard -> bool` _&rarr;_&nbsp;`string`

Install a managed namespace policy specifying ALLOW-ROOT and NS-POLICY-FUN.

```pact
(env-namespace-policy (my-ns-policy-fun))
```
