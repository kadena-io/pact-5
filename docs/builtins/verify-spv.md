The `verify-spv` function performs a platform-specific SPV (Simplified Payment Verification) proof of a specified type on a given payload. The format of the payload object depends on the type of proof (TYPE), as does the format of the return object. Specific payload types and return values are documented for platforms such as Chainweb.

### Basic syntax

To perform an SPV proof of a specified type on a payload, use the following syntax:

verify-spv *type* *payload*

### Arguments

Use the following arguments to specify the type of SPV proof and the payload for verification using the `verify-spv` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| type | string | Specifies the type of SPV proof to be performed. |
| payload | object:<in> | Specifies the payload object to be used for verification. |

### Return value

The `verify-spv` function returns an object whose format depends on the specific platform and the type of SPV proof being performed.

### Examples

The following example demonstrates the usage of the `verify-spv` function within a Pact script. It verifies an SPV proof of type "TXOUT" using the payload obtained from reading a message:

```lisp
(verify-spv "TXOUT" (read-msg "proof"))
```

This example illustrates how to use the `verify-spv` function to perform SPV proof verification in Pact, leveraging platform-specific functionality to ensure the validity of transactions or other data.
