## verify-spv

Use `verify-spv` to perform a platform-specific simplified payment verification (SPV) proof of a specified type for the specified payload. 
The format of the payload object and the returned object depends on the type of proof you specify. 
For information about payload types and return values, see platform-specific documentation. 
For Chainweb nodes, see the [Chainweb API](/reference/chainweb-ref) specification.

### Basic syntax

To perform an SPV proof of a specified `type` on a specified `payload`, use the following syntax:

```pact
(verify-spv type payload)
```

### Arguments

Use the following arguments to specify the type of SPV proof and the payload for verification using the `verify-spv` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `type` | string | Specifies the type of SPV proof to be performed. |
| `payload` | object | Specifies the payload object to be used for verification. |

### Return value

The `verify-spv` function returns an object whose format depends on the specific platform and the type of SPV proof being performed.

### Examples

The following example demonstrates how to use the `verify-spv` function to verify an SPV proof of type `"txout"` using the payload obtained from reading a message:

```pact
(verify-spv "txout" (read-msg "proof"))
```
