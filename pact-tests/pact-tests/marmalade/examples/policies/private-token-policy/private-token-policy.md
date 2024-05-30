# Private token policy

Private token policy allows creators to make an airdrop without revealing the metadata of the token beforehand. The token URI can be revealed at any time, making the metadata known to all.

## Requirements:

Concrete policy `guard-policy` must be used in conjunction with `private-token-policy` to make sure only an authorized account can update the token URI.

While creating a token, the URI should be the hash of the actual URI. This can be calculated using a local call to the node so there is no trace recorded on the chain.

Note: token URI can still be updated by the `uri-guard` but only after revealing the initial URI.

## Specification, tables, capabilities, events:

**Schemas**: `revealed-tokens-schema` is a schema that stores which tokens have been revealed
  - `revealed`: shows if the URI has been revealed.

**Tables**: `revealed-tokens` table stores which tokens have been revealed.
  - `id`: the id of the token

**Capabilities**:
 - `GOVERNANCE`: enforces access control of contract upgrades.

**Events**:
 - `TOKEN_REVEALED (token-id uri)`: Emitted when the token URI has been revealed.

## Policy Functions

**`enforce-init`:** Enforced during `marmalade-v2.ledger.create-token`, and will ensure the concrete `guard-policy` is present along with the URI guard.

**`enforce-mint`:** Enabled without limitation.

**`enforce-burn`:** Enabled without limitation.

**`enforce-offer`:** Enabled without limitation.

**`enforce-buy`:** Enabled without limitation.

**`enforce-withdraw`:** Enabled without limitation.

**`enforce-transfer`:** Enabled without limitation.

**`enforce-update-uri`:** Enforced during `marmalade-v2.ledger.update-uri`, and will allow updating only if the token has been revealed before.

**`reveal-uri`:** Will make sure that the saved hash of the URI matches the hashed new URI and will invoke `marmalade-v2.ledger.update-uri`.

**`is-revealed`:** Check if the URI has been revealed.