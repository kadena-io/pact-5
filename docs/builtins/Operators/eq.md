## =

_x_&nbsp;`<a[integer,string,time,decimal,bool,[<l>],object:<{o}>,keyset,guard,module{}]>`
_y_&nbsp;`<a[integer,string,time,decimal,bool,[<l>],object:<{o}>,keyset,guard,module{}]>`
_&rarr;_&nbsp;`bool`

Compare alike terms for equality, returning TRUE if X is equal to Y. Equality
comparisons will fail immediately on type mismatch, or if types are not value
types.

```bash
pact> (= [1 2 3] [1 2 3])
true
pact> (= 'foo "foo")
true
pact> (= { 'a: 2 } { 'a: 2})
true
```
