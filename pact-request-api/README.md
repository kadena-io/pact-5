pact-request-api
================

This is the public interface to `pact-5`. Downstream users of the
pact interpreter should use functions and types from this package,
rather than using the `pact-tng` library components directly.

Our two main clients are:

 - chainweb-node: Chainweb interacts with the pact interpreter via
   `Command` and `CommandResult` types.
 - pact-cli: Our own CLI tool (which hosts the repl, local server,
   and command generation code) depends on the functions exposed
   by `pact-request-api`.
