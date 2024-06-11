## decrypt-cc20p1305
Use `decrypt-cc20p1305` to perform decryption of a `CIPHERTEXT` using the CHACHA20-POLY1305 Authenticated Encryption with Associated Data (AEAD) construction described in IETF RFC 7539.

### Basic syntax

To perform decryption of a `CIPHERTEXT` using CHACHA20-POLY1305, use the following syntax:

`(decrypt-cc20p1305 CIPHERTEXT NONCE AAD MAC PUBLIC-KEY SECRET-KEY)`

### Arguments

Use the following arguments to specify the inputs for the `decrypt-cc20p1305` Pact function:

| Argument   | Type   | Description                                                  |
|------------|--------|--------------------------------------------------------------|
| `ciphertext` | `string` | Specifies the ciphertext to decrypt (unpadded base64url).    |
| `nonce` | `string` | Specifies the 12-byte nonce used in the encryption (base64).|
| `aad` | `string` | Specifies the additional authentication data (AAD) (base64).|
| `mac` | `string` | Specifies the "detached" base64 tag value for authentication.|
| `public-key` | `string` | Specifies the base-16 Curve25519 public key.                |
| `secret-key` | `string` | Specifies the base-16 Curve25519 secret key.                |

### Return values

The `decrypt-cc20p1305` function returns the decrypted plaintext as an unpadded base64URL string.

### Example

The following example demonstrates the `decrypt-cc20p1305` function:

```pact
(decrypt-cc20p1305 "aGVsbG8gd29ybGQh" "jXg5EEna3ncA" "QW5kcm9pZCBBdXRob3JpemF0aW9uIERhdGE" "yzNz8N5cc0ZT4vp_zV-6PmfYvgEtZhhlS8_JZ6odS0A" "A9Fp8OL3Mgu_8haIjy8JhsmLzPmzJ0JcbE9kSVNB5Y" "MwYTMzc2NjVhZGFmY2QxMzM4Y2I1ZmU2ZDM1YjQ3MzU") 
```

In this example, `decrypt-cc20p1305` is used to decrypt the ciphertext "aGVsbG8gd29ybGQh" using CHACHA20-POLY1305 with the specified nonce, additional authentication data (AAD), authentication tag (MAC), public key, and secret key. The function returns the decrypted plaintext as an unpadded base64URL string.
