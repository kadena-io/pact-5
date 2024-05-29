# Timed Mint Policy

The Timed Mint Policy is a module of Marmalade that enables the minting of tokens within a specific time window. This policy restricts the minting of tokens to a predefined period and provides control over the minting process.

## Specification, tables, capabilities:

**Schemas**: `timed-mint-schema` is a schema that store information related to the time window and max supply.
   - `max-supply`: refers to the maximum number of supply that tokens can be minted to. If max-supply equals `0.0`, then the token is considered to have unlimited supply .
   - `mint-start-time`: mint-start-time is the beginning of the minting time window.
   - `mint-end-time`: mint-end-time is the end of the minting time window.

**Tables**: `timed-mint` table stores time window and max supply information.

**Capabilities**:
- `GOVERNANCE`: Enforces access control of contract upgrades.

## Policy Functions

**`enforce-init`:** Requires `TIMED-MINT-SPEC` object to be included while calling `create-token`.

**`enforce-mint`:** Enforced during the specified time window, throws `Minting is not allowed outside the time window!`.

**`enforce-burn`:** Enabled without limitation.

**`enforce-offer`:** Enabled without limitation.

**`enforce-buy`:** Enabled without limitation.

**`enforce-withdraw`:** Enabled without limitation.

**`enforce-transfer`:** Enabled without limitation.

## Custom Functions

**`get-timed-mint`:** This function is used to retrieve time window and max supply