## namespace

Use `namespace` to set the current working environment to the specified namespace value.
After you declare the namespace you want to work with, all of the modules and functions you define are contained within that namespace.

You can access the modules and functions in a namespace by using their fully qualified name.
The fully-qualified name includes the namespace string as a prefix before the module name.
For example, if you declare a principal namespace such as `n_14912521e87a6d387157d526b281bde8422371d1` for the module `my-calculator`, you can call functions in the module using a fully-qualified name similar to the following:

`n_14912521e87a6d387157d526b281bde8422371d1.my-calculator.add`

If you call the `namespace` function after the initial declaration, Pact creates a new namespace for all subsequent declarations until either the next `namespace` call or the end of the transaction.

### Prerequisites

You must define a namespace before you can set your working context to use the `namespace` function. For information about defining a namespace, see [define-namespace](/pact-5/general/define-namespace).

### Basic syntax

To set the current `namespace` to a specified value, use the following syntax:

```pact
(namespace namespace)
```

### Arguments

Use the following argument to specify the namespace to be set using the `namespace` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `namespace` | string | Specifies the name of the namespace you want to use as your working context. |

### Return value

The `namespace` function returns the string representing the namespace that has been set.

### Examples

The following example demonstrates the use of `namespace` in a Pact script:

```pact
(namespace 'my-namespace)
```

In this example, the current namespace is set to `'my-namespace'`. All subsequent expressions within the same transaction will be contained in this namespace until a new namespace is set or the transaction ends.
