## describe-keyset

Use `describe-keyset` to retrieve metadata for a specified keyset.

**Note:** You can only use this function at the top level of your code. The function fails if used within module code.

### Basic syntax

To get metadata for the specified `keyset` name, use the following syntax:

```pact
(describe-keyset keyset)
```

### Arguments

Use the following argument to specify the keyset for which to retrieve metadata using the `describe-keyset` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `keyset` | string | Specifies the name of the keyset that you want to retrieve metadata for. |

### Return values

The `describe-keyset` function returns a guard.

The returned object includes the following properties:

- `pred`: The predicate function associated with the keyset.
- `keys`: An array of public keys associated with the keyset.

### Examples

The following example retrieves metadata for a keyset named `'admin-keyset'` in the Pact REPL:

```lisp
pact> (describe-keyset 'admin-keyset)
{
  "pred": "keys-all",
  "keys": [
    "ba54b224d1924dd98403f5c751abdd10de6cd81b0121800bf7bdbdcfaec7388d",
    "8cc94f8a4b43f4d9e3f8c5dca3966ea000f13ecbd79abc01bc7c00faacd06a5e"
  ]
}
```
