## hash-keccak256

Use `hash-keccak256` to compute the hash of a list of unpadded base64url-encoded inputs `VALUES`. 
The hash is computed incrementally over all of the decoded inputs.

### Basic syntax

To compute the hash of a list of inputs, use the following syntax:

`(hash-keccak256 VALUES)`

### Arguments

Use the following argument to specify the list of inputs for the `hash-keccak256` Pact function:

| Argument  | Type       | Description                                       |
|-----------|------------|---------------------------------------------------|
| `VALUES`  | `[string]`   | Specifies the list of unpadded base64url-encoded inputs. |

### Return value

The `hash-keccak256` function returns a string representing the computed hash value.

### Examples

The following examples demonstrate the `hash-keccak256` function:

```pact
pact>(hash-keccak256 [])
"xdJGAYb3IzySfn2y3McDwOUAtlPKgic7e_rYBF2FpHA"
```

In this example, an empty list `[]` is provided as input. The `hash-keccak256` function computes the hash of the empty list and returns the hash value.


```pact
(hash-keccak256 ["T73FllCNJKKgAQ4UCYC4CfucbVXsdRJYkd2YXTdmW9gPm-tqUCB1iKvzzu6Md82KWtSKngqgdO04hzg2JJbS-yyHVDuzNJ6mSZfOPntCTqktEi9X27CFWoAwWEN_4Ir7DItecXm5BEu_TYGnFjsxOeMIiLU2sPlX7_macWL0ylqnVqSpgt-tvzHvJVCDxLXGwbmaEH19Ov_9uJFHwsxMmiZD9Hjl4tOTrqN7THy0tel9rc8WtrUKrg87VJ7OR3Rtts5vZ91EBs1OdVldUQPRP536eTcpJNMo-N0fy-taji6L9Mdt4I4_xGqgIfmJxJMpx6ysWmiFVte8vLKl1L5p0yhOnEDsSDjuhZISDOIKC2NeytqoT9VpBQn1T3fjWkF8WEZIvJg5uXTge_qwA46QKV0LE5AlMKgw0cK91T8fnJ-u1Dyk7tCo3XYbx-292iiih8YM1Cr1-cdY5cclAjHAmlglY2ia_GXit5p6K2ggBmd1LpEBdG8DGE4jmeTtiDXLjprpDilq8iCuI0JZ_gvQvMYPekpf8_cMXtTenIxRmhDpYvZzyCxek1F4aoo7_VcAMYV71Mh_T8ox7U1Q4U8hB9oCy1BYcAt06iQai0HXhGFljxsrkL_YSkwsnWVDhhqzxWRRdX3PubpgMzSI290C1gG0Gq4xfKdHTrbm3Q"])
```

In this example, a list containing multiple base64url-encoded strings is provided as input. The `hash-keccak256` function computes the hash of all the inputs and returns the hash value.

The `hash-keccak256` function is useful for computing hash values of data for various cryptographic operations in Pact contracts.
