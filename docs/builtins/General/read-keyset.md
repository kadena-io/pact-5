## read-keyset

Use `read-keyset` to read the specified `key` from the message data body as a keyset.
A keyset consists of a list of keys (`keylist`) and, optionally, a predicate function (`predfun`).

### Basic syntax

To read a `key` from the message data body as a keyset, use the following syntax:

```pact
(read-keyset key)
```

### Arguments

Use the following argument to specify the `key` to be read as a keyset using the `read-keyset` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `key` | string | Specifies the name of the key to be read from the message data body as a keyset. |

### Return value

The `read-keyset` function returns the keyset object corresponding to the specified `key` string.

### Example

The following example demonstrates how to use the `read-keyset` function in a Pact script. 
This example reads the object specified for the `admin-keyset` key from the body of a message as the name of a keyset object:

```pact
(read-keyset 'admin-keyset)
```
