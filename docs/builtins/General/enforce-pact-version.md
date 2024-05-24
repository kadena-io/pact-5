## enforce-pact-version
Use `enforce-pact-version` to enforce the runtime Pact version to be within a specified range, where the version is greater than or equal to `MIN-VERSION` and less than or equal to `MAX-VERSION`. Version values are matched numerically from the left, meaning versions like '2', '2.2', and '2.2.3' would all allow '2.2.3'.

### Basic syntax

To enforce the runtime Pact version within a specified range, use the following syntax:

`(enforce-pact-version MIN-VERSION)`

`(enforce-pact-version MIN-VERSION MAX-VERSION)`

### Arguments

Use the following arguments to specify the minimum and maximum Pact versions for the `enforce-pact-version` Pact function:

| Argument    | Type   | Description                                                    |
|-------------|--------|----------------------------------------------------------------|
| `MIN-VERSION` | `string` | Specifies the minimum Pact version to enforce.             |
| `MAX-Version` | `string` | Specifies the maximum Pact version to enforce (optional).  |

### Return values

The `enforce-pact-version` function returns a boolean value indicating whether the runtime Pact version is within the specified range.

### Examples

The following example demonstrates the `enforce-pact-version` function:

```pact
pact>(enforce-pact-version "2.3")
true
```

In this example, `(enforce-pact-version "2.3")` is used to enforce that the runtime Pact version is at least "2.3". If the current Pact version is "2.3" or higher, the function returns true. If the Pact version is lower than "2.3", the function fails.

You can also specify a maximum Pact version:

```pact
pact>(enforce-pact-version "2.3" "2.4")
<interactive>:0:0:Error: Invalid pact version 4.11, maximum allowed: 2.4
```

In this example, `(enforce-pact-version "2.3" "2.4")` is used to enforce that the runtime Pact version is between "2.3" and "2.4" inclusive. If the current Pact version is "2.3", "2.4", or anything in between, the function returns true. If the Pact version is outside this range, the function fails.
