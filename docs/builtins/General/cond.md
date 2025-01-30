## cond

`cond` is our macro for multiple branching conditional expressions. It allows you to do a multiway if expression in a more concise manner.

### Basic syntax

Use the following syntax:

```pact
(defun award:string (score:integer)
  (cond ((< score 10) "Bronze")
        ((< score 20) "Silver")
        ((< score 30) "Gold")
        "Platinum"))
```

### Examples

```pact
(module m g
  (defcap g () true)
  (defun award:string (score:integer)
    (cond ((< score 10) "Bronze")
          ((< score 20) "Silver")
          ((< score 30) "Gold")
          "Platinum")))

(award 100)
```
