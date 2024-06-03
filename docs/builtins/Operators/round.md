## round

_x_&nbsp;`decimal` _prec_&nbsp;`integer` _&rarr;_&nbsp;`decimal`

_x_&nbsp;`decimal` _&rarr;_&nbsp;`integer`

Performs Banker's rounding value of decimal X as integer, or to PREC precision
as decimal.

```bash
pact> (round 3.5)
4
pact> (round 100.15234 2)
100.15
```
