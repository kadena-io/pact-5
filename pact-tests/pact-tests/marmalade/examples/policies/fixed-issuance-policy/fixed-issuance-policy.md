
# Fixed Issuance Policy

Fixed Issuance Policy is an example module of Marmalade which provides a simple method to program a fractional token. Because there are various ways to program fractional tokens, the contract only provides the simplest feature that is required for fractional tokens, and encourages users to extend upon the
contract. 

## Specification, tables, capabilities:

**Schemas**: `supply-schema` is a schema that store information related to the issuance of the token.
   - `max-supply`: refers to the maximum number of supply that tokens can be minted to. If max-supply equals `0.0`, then the token is considered to have unlimited supply .
   - `min-amount`: min-amount is the minimum amount that the token can be minted with.
   - `precision`: precision is the same precision used in `marmalade-v2.ledger`. It is enforced that the two precisions equal each other.

**Tables**: `supplies` table stores token supply information.

**Capabilities**:
 - `GOVERNANCE`: enforces access control of contract upgrade.

## Policy Functions

**`enforce-init`:** Run during `marmalade-v2.ledger.create-token`, and registers the token's supply information.

**`enforce-mint`:** Enforced during `marmalade-v2.ledger.mint`, and enforces the mint logic following the provided supply information

**`enforce-burn`:** Enabled without limitation. In order to program burn, the token must incorporate a different policy that programs burn.

**`enforce-offer`:** Enabled without limitation. In order to program offer, the token must incorporate a different policy that programs offer.

**`enforce-buy`:** Enabled without limitation. In order to program buy, the token must incorporate a different policy that programs buy.

**`enforce-withdraw`:** Enabled without limitation. In order to program withdraw, the token must incorporate a different policy that programs withdraw.

**`enforce-transfer`:** Enabled without limitation. In order to program transfer, the token must incorporate a different policy that programs transfer.

## Custom functions

**`get-supply:`** This function is used to retrieve supply information from the `token_spec`
