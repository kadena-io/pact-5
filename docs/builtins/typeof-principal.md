## typeof-principal
The `typeof-principal` function returns the protocol type of a given PRINCIPAL value. If the input value is not a principal type, then an empty string is returned.

### Basic syntax

To determine the protocol type of a principal value, use the following syntax:

typeof-principal *principal*

### Argument

Use the following argument to specify the PRINCIPAL value for which to determine the protocol type using the `typeof-principal` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| principal | string | Specifies the principal value for which to determine the protocol type. |

### Return value

The `typeof-principal` function returns the protocol type of the given PRINCIPAL value as a string. If the input value is not a principal type, an empty string is returned.

### Examples

The following example demonstrates the usage of the `typeof-principal` function within a Pact script. It determines the protocol type of a given principal value:

```lisp
(typeof-principal 'k:462e97a099987f55f6a2b52e7bfd52a36b4b5b470fed0816a3d9b26f9450ba69)
```

This example illustrates how to use the `typeof-principal` function to determine the protocol type of a principal value in Pact.
