(namespace (read-string 'ns))
(use policy-manager [NON_FUNGIBLE_POLICY ROYALTY_POLICY COLLECTION_POLICY GUARD_POLICY])

(policy-manager.init ledger)
(policy-manager.write-concrete-policy NON_FUNGIBLE_POLICY non-fungible-policy-v1)
(policy-manager.write-concrete-policy ROYALTY_POLICY royalty-policy-v1)
(policy-manager.write-concrete-policy COLLECTION_POLICY collection-policy-v1)
(policy-manager.write-concrete-policy GUARD_POLICY guard-policy-v1)
