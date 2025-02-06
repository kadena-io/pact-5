## describe-namespace

Use `describe-namespace` to describe the specified `namespace`, returning a row object containing the keyset guards for the administrative owner of the namespace and the user allowed to access the namespace for the specified namespace.

### Basic syntax

To describe the namespace `namespace`, use the following syntax:

```pact
(describe-namespace namespace)
```

### Prerequisites

You must define a namespace with the `define-namespace` function before you can use the `describe-namespace` function.
For information about defining a namespace, see [define-namespace](/pact-5/general/define-namespace).

### Arguments

Use the following argument to specify the `namespace` for the `describe-namespace` Pact function.

| Argument | Type   | Description                                 |
|----------|--------|---------------------------------------------|
| `namespace` | string | Specifies the name of the namespace to describe.|

### Return values

The `describe-namespace` function returns an object with detailed information about the specified `namespace`.

### Examples

The following example demonstrates the `describe-namespace` function:

```pact
pact> (env-data { "keyset": ["fake-key"]})
"Setting transaction data"
pact> (define-namespace 'my-namespace (read-keyset 'keyset) (read-keyset 'keyset))
"Namespace defined: my-namespace"
pact> (describe-namespace 'my-namespace)
"admin-guard":KeySet {keys: [fake-key]
,pred: keys-all}, "namespace-name":"my-namespace", "user-guard":KeySet {keys: [ fake-key ]
,pred: keys-all}}
```

In this example, `(describe-namespace 'my-namespace)` is used to describe the namespace named "my-namespace". 
The function returns a row object containing the keyset guards for the administrative owner of the namespace and the user allowed to access the namespace for the specified namespace.
