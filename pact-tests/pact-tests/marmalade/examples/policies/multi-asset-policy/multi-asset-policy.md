# Multi-asset Policy

The `multi-asset-policy` module is a part of the Marmalade example policies designed to manage multi-asset tokens in a context-dependent manner. It adheres to the `kip.token-policy-v2` standard and provides functionality for asset management, including proposing, accepting, and rejecting assets related to a token.

There are two possible implementations, for fungible and non-fungible tokens.

- Fungible: Asset URIs need to be provided at the token creation
- Non-fungible: Asset URIs can be proposed by the token operator and accepted/rejected by the token owner

## Specification, tables, capabilities:

**Schemas**:
- `token-asset-schema`
  - `uri:string`: Represents the asset URI.

- `token-proposed-asset-schema`
  - `assets:[string]`: Represents the list of proposed asset URIs.

- `token-operators-schema`
  - `guard:guard`: Represents the guard of the token creator/operator.

**Tables**:
- `token-assets:{token-asset-schema}`: Stores the token asset URIs.
- `proposed-assets:{token-proposed-asset-schema}`: Stores the list of proposed asset URIs.
- `token-operators:{token-operators-schema}`: Stores the token operator guard for proposing new assets.

**Capabilities**:
 - `GOVERNANCE`: enforces access control of contract upgrade.
 - `PROPOSE_ASSET (token-id:string uri:string)`: enforces access control of token operator.
 - `ACCEPT_ASSET (token-id:string asset-id:integer owner:string)`: enforces access control of token owner.
 - `REJECT_ASSET (token-id:string asset-id:integer owner:string)`: enforces access control of token owner.
 - `REJECT_ALL_ASSETS (token-id:string owner:string)`: enforces access control of token owner.
 - `SET_ASSET_PRIORITY (token-id:string asset-id:integer priority:integer owner:string)`: enforces access control of token owner.


**Events**:
 - `ASSET_PROPOSED (token-id:string asset-id:integer uri:string)`: emitted when new asset is proposed.
 - `ASSET_ACCEPTED (token-id:string asset-id:integer uri:string owner:string)`: emitted when the asset has been accepted.
 - `ASSET_REJECTED (token-id:string asset-id:integer uri:string owner:string)`: emitted when the asset has been rejected.
 - `ASSET_SET (token-id:string asset-id:integer uri:string)`: emitted at init for fungible tokens.
 - `ASSET_PRIORITY_SET (token-id:string asset-id:integer priority:integer owner:string)`: emitted when new asset priority is set.

## Policy Functions

**`enforce-init`:** Enforced during `marmalade-v2.ledger.create-token`, and requires `assets` data message for fungible and `operator_guard` for non-fungible tokens.

**`enforce-mint`:** Enabled without limitation.

**`enforce-burn`:** Enabled without limitation.

**`enforce-offer`:** Enabled without limitation.

**`enforce-buy`:** Enabled without limitation.

**`enforce-withdraw`:** Enabled without limitation.

**`enforce-transfer`:** Enabled without limitation.

## Custom functions

**`get-asset:`** Retrieves an asset URI for the given token by index.

**`get-assets:`** Retrieves all asset URIs for the given token.

**`propose-asset:`** Used by token operator to propose new asset URI.

**`replace-proposed-asset:`** Used by token operator to replace existing proposed asset URI.

**`get-proposed-assets:`** Get a list of proposed asset URIs.

**`get-proposed-asset:`** Get proposed asset URI by index.

**`reject-proposed-asset:`** Reject proposed asset URI for token by index.

**`reject-all-proposed-assets:`** Reject all proposed assets for token.

**`accept-asset:`** Used by token owner to accept proposed asset by index.

**`set-asset-priority:`** Used by token owner to set asset priority.

## Utility functions

**`has-non-fungible-policy:`** Returns `true` if `marmalade-v2.non-fungible-policy-v1` is present in the `policies` array.

**`enforce-non-fungible-policy:`** Enforces that `marmalade-v2.non-fungible-policy-v1` is present in token details.

**`enforce-assets-at-init:`** Enforces that `assets` data message is present at `enforce-init` for fungible tokens.

**`enforce-operator-at-init:`** Enforces that `operator_guard` data message is present at `enforce-init` for non-fungible tokens.

**`update-array:`** Updates string array item by index.

**`remove-item:`** Remove item from array by index.