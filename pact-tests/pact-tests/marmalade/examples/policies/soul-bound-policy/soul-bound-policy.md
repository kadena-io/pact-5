# Soul Bound Policy

Soul Bound Policy is an example module of Marmalade which provides a simple method to bind a token to an account (soul). In this context, a soul-bound token refers to a token that is uniquely associated with a specific account.

This token can be initiated, minted, and burned but not sold or transferred.

The token can be minted only once, even if the token is burned in the future.

## Requirements:

Concrete policy `guard-policy` must be used in conjunction with `soul-bound-policy` to make sure only an authorized account can mint and burn the token.

## Specification, tables, capabilities:

**Schemas**: `mint-record` is a schema that stores information related to the issuance of the token.
  - `account`: refers to an owner of the token.

**Tables**: `records` table stores token supply information.
  - `id` is composed of token id and account

**Capabilities**:
 - `GOVERNANCE`: enforces access control of contract upgrades.

## Policy Functions

**`enforce-init`:** Enforced during `marmalade-v2.ledger.create-token`, and will ensure the `precision` is set to 0 and concrete `guard-policy` is present.

**`enforce-mint`:** Enforced during `marmalade-v2.ledger.mint`, and will ensure that an account can only own one instance of a token at any given time and initiate `guard-policy` checks.

**`enforce-burn`:** Enforced during `marmalade-v2.ledger.burn`, and will ensure the whole amount is burned.

**`enforce-offer`:** Enforced during `marmalade-v2.ledger.offer`, and throws `Sale is not allowed!`.

**`enforce-buy`:** Enforced during `marmalade-v2.ledger.buy`, and throws `Sale is not allowed!`.

**`enforce-withdraw`:** Enforced during `marmalade-v2.ledger.withdraw`, and throws `Sale is not allowed!`.

**`enforce-transfer`:** Enforced during `marmalade-v2.ledger.transfer`, and throws `Transfer is not allowed!`.