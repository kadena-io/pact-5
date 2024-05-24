## describe-namespace

Use `describe-namespace` to describe the namespace `NS`, returning a row object containing the user and admin guards of the namespace, as well as its name.

### Basic syntax

To describe the namespace `NS`, use the following syntax:

`(describe-namespace NS)`

### Arguments

Use the following argument to specify the `NS` for the `describe-namespace` Pact function.

| Argument | Type   | Description                                 |
|----------|--------|---------------------------------------------|
| `ns`       | `string` | Specifies the name of the namespace to describe.|

### Return values

The `describe-namespace` function returns an object with detailed information about the specified `NS`.

### Examples

The following example demonstrates the `describe-namespace` function:

```pact
pact>(env-data { "keyset": ["fake-key"]})
"Setting transaction data"
pact>(define-namespace 'my-namespace (read-keyset 'keyset) (read-keyset 'keyset))
"Namespace defined: my-namespace"
pact>(describe-namespace 'my-namespace)
"admin-guard":KeySet {keys: [fake-key]
,pred: keys-all}, "namespace-name":"my-namespace", "user-guard":KeySet {keys: [ fake-key ]
,pred: keys-all}}
```

In this example, `(describe-namespace 'my-namespace)` is used to describe the namespace named 'my-namespace'. The function returns a row object containing the user and admin guards of the namespace, as well as its name. This provides a detailed description of the specified namespace.
