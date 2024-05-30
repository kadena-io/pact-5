# Marmalade - V2 Deployment

## Data file
The YAML data files includes parameters for deployment:

Example for testnet:
```yaml
network: testnet04
chain: 1
marmalade_namespace: marmalade-v2
kip_namespace: kip
is_upgrade: false
sender: dfb16b13e4032a6878fd98506b22cb0d6e5932c541e656b7ee5d69d72e6eb76e
sender_key: dfb16b13e4032a6878fd98506b22cb0d6e5932c541e656b7ee5d69d72e6eb76e
```

## YAML transactions files

The current deployment YAML files assume that only 1 signer is needed:
  - For gas payment
  - For namespaces unlocking

Others signers can be added if needed

## Transactions (JSON) files

Transaction files can be generated and submitted by:

```sh
kda gen -t XXX.yaml -d data_file.yaml

# Depending on your signature method
kda sign tx.yaml --chainweaver
# or
kda sign tx.yaml -k key.yaml
# or
# ...

# Then submit
kda send tx.json
```


## Deployment Order / Dependencies

The deployment order must be respected to fulfill all modules dependencies.

* In the `kip` Namespace

  0. account-protocols-v1

* In the `util` Namespace
  1. fungible-util

* In the `kip` Namespace
  2. poly-fungible-v3
  3. updatable-uri-v1
  4. poly-fungible-v3

* In the `marmalade-v2` Namespace

  5. ledger-v2
  6. sale-v1
  7. policy-manager
  8. ledger
  9. Concrete policies
    * collection-policy-v1
    * guard-policy-v1
    * non-fungible-policy-v1
    * royalty-policy-v1
  10. policy-init **(Don't forget this one)**
  11. util-v1

Ideally, verify that each transaction has been fully mined and has succeed between each step.

* In the `marmalade-sale` Namespace
  12. conventional-auction
  13. dutch-auction
  14. sale-init **(Don't forget this one)**

## Note about the KIP namespace

Most marmalade contracts include fully qualified links to the KIP deployed interfaces.

If the `token-policy-v2` and `poly-fungible-v3` are deployed in another namespace than `kip`, the links have to be manually fixed before deployment, in each Pact module.

## Note about upgrading modules

Don't forget to set `is_upgrade` to `true` in the YAML data file
