## hyperlane-message-id

Use `hyperlane-message-id` to get the Message Id of a Hyperlane Message object.

### Basic syntax

To get the Message Id of a Hyperlane Message object using `hyperlane-message-id`, use the following syntax:

```pact
(hyperlane-message-id x)
```

### Arguments

| Name | Type | Description |
|------|------|-------------|
| `x`  | object:* | A Hyperlane Message object |

The Hyperlane Message object should have the following structure:
- `destinationDomain`: integer
- `nonce`: integer
- `originDomain`: integer
- `recipient`: integer
- `sender`: string
- `messageBody`: string
- `version`: integer

### Return value

The `hyperlane-message-id` function returns a string representing the Message Id of the given Hyperlane Message object.

### Examples

Here's an example of using `hyperlane-message-id` to get the Message Id of a Hyperlane Message object:

```pact
pact> (hyperlane-message-id {"destinationDomain": 1,"nonce": 325,"originDomain": 626,"recipient": "AAAAAAAAAADpgrOqkM0BOY-FQnNzkDXuYlsVcf50GRU","sender": "AAAAAAAAAAAAAAAAf6k4W-ECrD6sKXSD3WIz1is-FJY","messageBody": "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAHsABHsicHJlZCI6ICJrZXlzLWFsbCIsICJrZXlzIjpbImRhMWEzMzliZDgyZDJjMmU5MTgwNjI2YTAwZGMwNDMyNzVkZWIzYWJhYmIyN2I1NzM4YWJmNmI5ZGNlZThkYjYiXX0","version": 1})
"9lxextceVw0b18kUdfwSze-3Iw7OE-Z5Kq9I8HTDKGE"
```

In this example, the function takes a Hyperlane Message object with the following properties:
- `destinationDomain`: 1
- `nonce`: 325
- `originDomain`: 626
- `recipient`: "AAAAAAAAAADpgrOqkM0BOY-FQnNzkDXuYlsVcf50GRU"
- `sender`: "AAAAAAAAAAAAAAAAf6k4W-ECrD6sKXSD3WIz1is-FJY"
- `messageBody`: "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAHsABHsicHJlZCI6ICJrZXlzLWFsbCIsICJrZXlzIjpbImRhMWEzMzliZDgyZDJjMmU5MTgwNjI2YTAwZGMwNDMyNzVkZWIzYWJhYmIyN2I1NzM4YWJmNmI5ZGNlZThkYjYiXX0"
- `version`: 1

The function returns the Message Id "9lxextceVw0b18kUdfwSze-3Iw7OE-Z5Kq9I8HTDKGE" for this Hyperlane Message object.
