## read-keyset
The `read-keyset` function is used to read a *`key`* from the message data body as a keyset, where a keyset consists of a list of keys (*`KEYLIST`*) and a predicate function (*`PREDFUN`*).

### Basic syntax

To read a *`key`* from the message data body as a keyset, use the following syntax:

read-keyset *key*

### Arguments

Use the following argument to specify the *`key`* to be read as a keyset using the `read-keyset` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| key | string | Specifies the key to be read from the message data body as a keyset. |

### Return value

The `read-keyset` function returns the keyset object corresponding to the specified *`key`*.

### Example

The following example demonstrates the usage of the `read-keyset` function within a Pact script. It reads the *`admin-keyset`* from the message data body:

```lisp
(read-keyset "admin-keyset")
```

This example illustrates how to use the `read-keyset` function to retrieve a keyset from the message data body for further processing within a Pact script.
