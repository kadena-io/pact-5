# define-keyset

Use `define-keyset` to define or update the keyset in a namespace.

## Basic usage

define-keyset *name* *keyset*

## Prerequisites

You must define a keyset within the context of a namespace.

## Arguments

Use the following arguments to define or update a keyset with the `define-keyset` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| name | string | Specifies the name of the keyset. |
| keyset | string | Specifies the keyset list of keys and predict in the form of ({ "keys": KEYLIST, "pred": PREDFUN }) or using the 'read-keyset function'.  If keyset name already exists, the current keyset context is enforced before updating to define a new value. |

## Return values

The `define-keyset` function returns the keyset you have defined or updated.

| Value | Type | Description |
| --- | --- | --- |
| keyset | string | Returns the keyset defined or updated. |

## Examples

The following example defines a keyset named `admin-keyset` in the `election` namespace tht can be tested using the Pact REPL:

```lisp
(env-data
    { 'admin-keyset :
      { 'keys : [ 'admin-public-key ]
      , 'pred : 'keys-all
      }
    }
)

(begin-tx "Define a namespace to define the keyset in")
  (define-namespace 'election (read-keyset 'admin-keyset) (read-keyset 'admin-keyset))
(commit-tx)

(begin-tx  "Define a new keyset")
  (namespace 'election)
  (define-keyset "election.admin-keyset" (read-keyset 'admin-keyset))
(commit-tx)
```

In this example, the `admin-keyset` consists of one key simulating the public key for an administrative account and the `keys-all` predicate. The keyset is included in the environment data for the Pact interactive shell so that it can be read to define the namespace, then to define the keyset inside the namespace.

For an example, of calling define-keyset using the Kadena client andTypeScript, see the `define-keyset.ts` script.
