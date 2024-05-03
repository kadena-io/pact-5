Use `compose-capability` to specify and request the grant of a *`CAPABILITY`*, which is an application of a 'defcap' production. This function is only valid within a (distinct) 'defcap' body. It is used as a way to compose *`CAPABILITY`* with the outer capability, such that the scope of the containing 'with-capability' call will "import" this capability. 

Thus, a call to `(with-capability (OUTER-CAP) OUTER-BODY)`, where the `OUTER-CAP` defcap calls `(compose-capability (INNER-CAP))`, will result in `INNER-CAP` being granted in the scope of `OUTER-BODY`.

### Basic syntax

To specify and request the grant of a *`CAPABILITY`* within a 'defcap' body, use the following syntax:

compose-capability *`CAPABILITY`*

### Arguments

Use the following argument to specify the *`CAPABILITY`* for the `compose-capability` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| capability | capability | Specifies the capability to compose and request grant for. |

### Return values

The `compose-capability` function returns a boolean value to indicate success or failure in requesting the grant of the specified *`CAPABILITY`*.

### Examples

The following example demonstrates the `compose-capability` function:

```lisp
(compose-capability (TRANSFER src dest))
```

In this example, `(compose-capability (TRANSFER src dest))` is used within a 'defcap' body to specify and request the grant of the `TRANSFER` capability with source `src` and destination `dest`. This would be used in the context of composing capabilities within a 'defcap' production and requesting grants in the scope of the containing 'with-capability' call.
