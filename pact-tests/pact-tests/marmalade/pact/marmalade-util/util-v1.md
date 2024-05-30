
# Util V1

This contract, `util-v1`, is designed to locate utility functions that marmalade functions can leverage, including easy creation or minting of the marmalade tokens, or concrete-policy formation.

## Specifications and Tables:

**Capabilities**:
 - `GOVERNANCE`: enforces access control of contract upgrade.
 - `UTIL_SIGN`: provides a capability to scope user's signature. NOTE that the capability will scope the user's signature to all the policies that the token is created with. Users are encouraged to only sign the capability when using a basic NFT with only `non-fungible-policy-v1`, and scope the signatures to only the code that enforces the signature.

## Functions:

### mint-NFT

This function merges the `marmalade-v2.ledger.create-token` and `marmalade-v2.ledger.mint` functions to provide users a easier minting experience. The function limits its usage to tokens that implement the concrete-policy,`NON-FUNGIBLE-POLICY`, which programs the token to limit the supply to 1 and precision to 0, making the token one and only.
The function takes in 3 parameters, `uri` , `policies`, `guard`, and crafts the `account` name using `create-principal` pact native function by inputting the guard, and must be signed with `guard`.


### mint-basic-NFT

This function wraps the `mint-NFT` token, and provides an easy tool to create token without any configuration. The token will be available to participate in Marmalade sales, but will not have any additional features such as royalties or collections.

### create-token-with-mint-guard

This function is a wrapper function around `marmalade-v2.ledger.create-token`. DApps can leverage the function
to enforce that `mint_guard` is always registered with `guard-token-policy-v1`.

#### create-policies

The function takes in a `{concrete-policy-bool}` schema and generates a list, `[module{kip.token-policy-v2}]`, filtering only the modules selected to `true`.

### create-concrete-policy

This function is opposite of `create-policies`, and takes in the list of token-policies and returns a `{concrete-policy-bool}`, providing an easy way to learn which concrete features are active to token.

### to-timestamp

This function takes in `time` argument and returns a UTC timestamp as `integer`. This function can be used in
Marmalade sales, which requires an UTC timestamp in `integer` type.

### curr-time

This function returns the block time in UTC timestamp as `integer`. This function can be used in
policies or sale contracts to check against various logics. 
