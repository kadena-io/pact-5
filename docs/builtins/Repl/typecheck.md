## typecheck

Use `(typecheck "my-module")` to run the pact static typechecker on some module `my-module`

### Basic syntax

For some module `foo`:

```pact
(module foo GOV
  (defcap GOV () true)

  (defun plus-one (a) (+ a 1))
)
```

To statically typecheck `foo`, call

```pact
(typecheck "foo")
```

If your module `foo` is under `(namespace "bar)`, then call `(typecheck "bar.foo")`

## Arguments

Use the following arguments when calling `typecheck`:

| Argument | Type | Description |
|----------|------|-------------|
| `module` | string | The module to run the static typechecker on |

## Return value

On typechecking succeess, `typecheck` returns the unit value `()`. Otherwise, it will throw an error.

## Examples

The following example demonstrates how to call the static typechecker:


For some module `foo`:

```pact
(module foo GOV
  (defcap GOV () true)

  (defun plus-one (a) (+ a 1))
)

(typecheck "foo")
```
