## namespace
Use `namespace` to set the current namespace to a specified value. All expressions that occur in the current transaction will be contained in the specified namespace. Once committed, they may be accessed via their fully qualified name, which will include the namespace. Subsequent namespace calls in the same transaction will set a new namespace for all declarations until either the next namespace declaration or the end of the transaction.

### Basic syntax

To set the current namespace to a specified value, use the following syntax:

namespace *namespace*

### Argument

Use the following argument to specify the namespace to be set using the `namespace` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| namespace | string | Specifies the namespace to be set. |

### Return value

The `namespace` function returns the string representing the namespace that has been set.

### Examples

The following example demonstrates the use of `namespace` in a Pact script:

```lisp
(namespace 'my-namespace)
```

In this example, the current namespace is set to `'my-namespace'`. All subsequent expressions within the same transaction will be contained in this namespace until a new namespace is set or the transaction ends.
