## cond

Use `cond` to evaluate multiple branching conditional expressions. 
This function allows you to evaluate a series of `if` expression in a more concise manner.

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
