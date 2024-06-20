## decrypt-cc20p1305

Use `decrypt-cc20p1305` to decrypt a specified `cipherText` stringusing the ChaCha20-Poly1305 Authenticated Encryption with Associated Data (AEAD) construction described in IETF RFC 7539.

### Basic syntax

To decrypt a specified `cipherText` string using ChaCha20-Poly1305, use the following syntax:

```pact
(decrypt-cc20p1305 cipherText nonce aad mac public-key secret-key)
```

### Arguments

Use the following arguments to specify the inputs for the `decrypt-cc20p1305` Pact function:

| Argument | Type | Description |
|----------| ---- |------------ |
| `ciphertext` | string | Specifies the string to decrypt to an unpadded base64url string. |
| `nonce` | string | Specifies the 12-byte nonce used in the encryption (base64).|
| `aad` | string | Specifies the additional authentication data (AAD) (base64).|
| `mac` | string | Specifies the "detached" base64 tag value for message authentication code (mac).|
| `public-key` | string | Specifies the base-16 Curve25519 public key. |
| `secret-key` | string | Specifies the base-16 Curve25519 secret key. |

### Return values

The `decrypt-cc20p1305` function returns the decrypted plaint ext as an unpadded base64url string.

### Example

The following example demonstrates how to use the `decrypt-cc20p1305` function to decrypt the ciphertext "aGVsbG8gd29ybGQh" using ChaCha20-Poly1305 with the specified nonce, additional authentication data (AAD), message authentication code, public key, and secret key:

```pact
(decrypt-cc20p1305 "aGVsbG8gd29ybGQh" "jXg5EEna3ncA" "QW5kcm9pZCBBdXRob3JpemF0aW9uIERhdGE" "yzNz8N5cc0ZT4vp_zV-6PmfYvgEtZhhlS8_JZ6odS0A" "A9Fp8OL3Mgu_8haIjy8JhsmLzPmzJ0JcbE9kSVNB5Y" "MwYTMzc2NjVhZGFmY2QxMzM4Y2I1ZmU2ZDM1YjQ3MzU") 
```

In this example, the `decrypt-cc20p1305` function returns the decrypted string as an unpadded base64URL string.
