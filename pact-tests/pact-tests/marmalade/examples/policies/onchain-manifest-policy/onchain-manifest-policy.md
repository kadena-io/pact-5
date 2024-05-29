# Onchain Manifest Policy

The `onchain-manifest-policy-v1` module is a part of Marmalade that provides onchain manifest storage for `marmalade-v2`. It implements the `kip.token-policy-v2` interface and enables the management of token manifests on the blockchain.

## Specification, tables, capabilities:

**Schemas**:
- `manifest-spec`: Defines the structure of the manifest specification.
  - `manifest:object{manifest}`: Represents the manifest object.
  - `guard:guard`: Manages the upgrade of the manifest on chain.

**Tables**:
- `manifests:{manifest-spec}`: Stores the token manifest information.

**Capabilities**:
 - `GOVERNANCE`: enforces access control of contract upgrade.

## Policy Functions

**`enforce-init`:** Enforced during `marmalade-v2.ledger.create-token`, and enforces the following msg-data keys:
  - `manifest_spec:object{manifest-spec}`: Registers the manifest object of the token and the guard that manages the upgrade of the manifest on chain.

**`enforce-mint`:** Enforced during `marmalade-v2.ledger.mint`, and enforces the mint logic following the provided supply information

**`enforce-burn`:** Enabled without limitation.

**`enforce-offer`:** Enabled without limitation.

**`enforce-buy`:** Enabled without limitation.

**`enforce-withdraw`:** Enabled without limitation.

**`enforce-transfer`:** Enabled without limitation.

## Custom functions

**`get-manifest:`** Retrieves the manifest object for a given token ID.

**`upgrade-manifest:`** Upgrades the manifest for a given token ID.