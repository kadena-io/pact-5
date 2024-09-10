## acquire-module-admin

Use `acquire-module-admin` to grant module admin privileges for a particular module `m`. You must already own admin for this particular module; that is, you must either be the owner of the keyset that grants the governance, or be able to pass the governance capability acquisition.

### Basic syntax

Assume you have some module
```pact
(module my-module gov
  (defcap gov ()
    (do-stuff-for-governance)
  )
)
```

To grant module admin, use

```pact
(acquire-module-admin some-module)
```

It will attempt to acquire the governance cap and if successful, it will grant module admin for the rest of the transaction.

### Arguments

Use one of the following argument to define the value you want to retrieve using the `at` Pact function.

| Argument | Type | Description
| -------- | ---- | -----------
| `ref` | modref | Specifies the module to acquire administrative capabilities for

### Return values

Module admin acquisition will either fail, or return "Module admin for module <my-module> acquired"

### Examples

See: Basic Syntax.
