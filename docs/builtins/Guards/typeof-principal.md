## typeof-principal

Use `typeof-principal` to return the protocol type of the specified `principal` value. 
If the specified value is not a principal type, then an empty string is returned.

### Basic syntax

To determine the protocol type of a specified `principal` value, use the following syntax:

```pact
(typeof-principal principal)
```

### Argument

Use the following argument to specify the `principal` value that you want to determine the protocol type for using the `typeof-principal` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `principal` | string | Specifies the principal value to determine the protocol type for. |

### Return value

The `typeof-principal` function returns the protocol type of the specified `principal` value as a string. 
If the input value is not a principal type, an empty string is returned.

### Examples

The following example demonstrates how to use the `typeof-principal` function to determine the protocol type of a principal value:

```pact
(typeof-principal "k:462e97a099987f55f6a2b52e7bfd52a36b4b5b470fed0816a3d9b26f9450ba69")
"k:"
```

The following example uses the `typeof-principal` function to create a namespace using the guard associated with the principal account name:

```pact
(defun create-principal-namespace:string
      ( g:guard
        )
    " Format principal namespace as Pact hash (BLAKE2b256) of principal \
    \ in hex truncated to 160 bits (40 characters), prepended with 'n_'.\
    \ Only w: and k: account protocols are supported. "

    (let
      ((ty (typeof-principal (create-principal g))))

      ;; only w: and k: currently supported
      (if (or (= ty "k:") (= ty "w:"))
        (+ "n_" (take 40 (int-to-str 16 (str-to-int 64 (hash g)))))
        (enforce false
          (format "Unsupported guard protocol: {}" [ty]))
        ))
)
```