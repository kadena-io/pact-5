{-# LANGUAGE QuasiQuotes #-}

module Pact.Core.Repl.BuiltinDocs
  ( topLevelHasDocs
  , builtinDocs
  ) where

import NeatInterpolation (text)
import Data.Text (Text)
import Pact.Core.Syntax.ParseTree
import Pact.Core.Names

import qualified Data.Map.Strict as M


topLevelHasDocs :: TopLevel i -> Maybe Text
topLevelHasDocs (TLTerm term) = case term of
  Var (BN (BareName bn)) _ -> M.lookup bn builtinDocs
  Operator op _ -> M.lookup (renderOp op) builtinDocs
  _ -> mempty


topLevelHasDocs _ = Nothing

builtinDocs :: M.Map Text Text
builtinDocs = M.fromList [
    ("!=", [text|
            native `!=`

              True if X does not equal Y.

              Type:
              x:<a[integer
              ,string
              ,time
              ,decimal
              ,bool
              ,[<l>]
              ,object:<{o}>
              ,keyset
              ,guard
              ,module{}]> y:<a[integer
              ,string
              ,time
              ,decimal
              ,bool
              ,[<l>]
              ,object:<{o}>
              ,keyset
              ,guard
              ,module{}]> -> bool

              Examples:
              > (!= "hello" "goodbye")
    |])
   ,("&", [text|
            native `&`

              Compute bitwise X and Y.

              Type:
              x:integer y:integer -> integer

              Examples:
              > (& 2 3)
              > (& 5 -7)
    |])
   ,("*", [text|
            native `*`

              Multiply X by Y.

              Type:
              x:<a[integer,decimal]> y:<a[integer,decimal]> -> <a[integer,decimal]>
              x:<a[integer,decimal]> y:<b[integer,decimal]> -> decimal

              Examples:
              > (* 0.5 10.0)
              > (* 3 5)
    |])
   ,("+", [text|
            native `+`

              Add numbers, concatenate strings/lists, or merge objects.

              Type:
              x:<a[integer,decimal]> y:<a[integer,decimal]> -> <a[integer,decimal]>
              x:<a[integer,decimal]> y:<b[integer,decimal]> -> decimal
              x:<a[string,[<l>],object:<{o}>]> y:<a[string,[<l>],object:<{o}>]> -> <a[string
              ,[<l>]
              ,object:<{o}>]>

              Examples:
              > (+ 1 2)
              > (+ 5.0 0.5)
              > (+ "every" "body")
              > (+ [1 2] [3 4])
              > (+ { "foo": 100 } { "foo": 1, "bar": 2 })
    |])
   ,("-", [text|
            native `-`

              Negate X, or subtract Y from X.

              Type:
              x:<a[integer,decimal]> y:<a[integer,decimal]> -> <a[integer,decimal]>
              x:<a[integer,decimal]> y:<b[integer,decimal]> -> decimal
              x:<a[integer,decimal]> -> <a[integer,decimal]>

              Examples:
              > (- 1.0)
              > (- 3 2)
    |])
   ,("/", [text|
            native `/`

              Divide X by Y.

              Type:
              x:<a[integer,decimal]> y:<a[integer,decimal]> -> <a[integer,decimal]>
              x:<a[integer,decimal]> y:<b[integer,decimal]> -> decimal

              Examples:
              > (/ 10.0 2.0)
              > (/ 8 3)
    |])
   ,("<", [text|
            native `<`

              True if X < Y.

              Type:
              x:<a[integer,decimal,string,time]> y:<a[integer,decimal,string,time]> -> bool

              Examples:
              > (< 1 3)
              > (< 5.24 2.52)
              > (< "abc" "def")
    |])
   ,("<=", [text|
            native `<=`

              True if X <= Y.

              Type:
              x:<a[integer,decimal,string,time]> y:<a[integer,decimal,string,time]> -> bool

              Examples:
              > (<= 1 3)
              > (<= 5.24 2.52)
              > (<= "abc" "def")
    |])
   ,("=", [text|
            native `=`

              Compare alike terms for equality, returning TRUE if X is equal to Y. Equality
              comparisons will fail immediately on type mismatch, or if types are not value
              types.

              Type:
              x:<a[integer
              ,string
              ,time
              ,decimal
              ,bool
              ,[<l>]
              ,object:<{o}>
              ,keyset
              ,guard
              ,module{}]> y:<a[integer
              ,string
              ,time
              ,decimal
              ,bool
              ,[<l>]
              ,object:<{o}>
              ,keyset
              ,guard
              ,module{}]> -> bool

              Examples:
              > (= [1 2 3] [1 2 3])
              > (= 'foo "foo")
              > (= { 'a: 2 } { 'a: 2})
    |])
   ,(">", [text|
            native `>`

              True if X > Y.

              Type:
              x:<a[integer,decimal,string,time]> y:<a[integer,decimal,string,time]> -> bool

              Examples:
              > (> 1 3)
              > (> 5.24 2.52)
              > (> "abc" "def")
    |])
   ,(">=", [text|
            native `>=`

              True if X >= Y.

              Type:
              x:<a[integer,decimal,string,time]> y:<a[integer,decimal,string,time]> -> bool

              Examples:
              > (>= 1 3)
              > (>= 5.24 2.52)
              > (>= "abc" "def")
    |])
   ,("CHARSET_ASCII", [text|
            0
    |])
   ,("CHARSET_LATIN1", [text|
            1
    |])
   ,("^", [text|
            native `^`

              Raise X to Y power.

              Type:
              x:<a[integer,decimal]> y:<a[integer,decimal]> -> <a[integer,decimal]>
              x:<a[integer,decimal]> y:<b[integer,decimal]> -> decimal

              Examples:
              > (^ 2 3)
    |])
   ,("abs", [text|
            native `abs`

              Absolute value of X.

              Type:
              x:decimal -> decimal
              x:integer -> integer

              Examples:
              > (abs (- 10 23))
    |])
   ,("add-time", [text|
            native `add-time`

              Add SECONDS to TIME; SECONDS can be integer or decimal.

              Type:
              time:time seconds:decimal -> time
              time:time seconds:integer -> time

              Examples:
              > (add-time (time "2016-07-22T12:00:00Z") 15)
    |])
   ,("and", [text|
            native `and`

              Boolean logic with short-circuit.

              Type:
              x:bool y:bool -> bool

              Examples:
              > (and true false)
    |])
   ,("and?", [text|
            native `and?`

              Apply logical 'and' to the results of applying VALUE to A and B, with
              short-circuit.

              Type:
              a:x:<r> -> bool b:x:<r> -> bool value:<r> -> bool

              Examples:
              > (and? (> 20) (> 10) 15)
    |])
   ,("at", [text|
            native `at`

              Index LIST at IDX, or get value with key IDX from OBJECT.

              Type:
              idx:integer list:[<l>] -> <a>
              idx:string object:object:<{o}> -> <a>

              Examples:
              > (at 1 [1 2 3])
              > (at "bar" { "foo": 1, "bar": 2 })
    |])
   ,("base64-decode", [text|
            native `base64-decode`

              Decode STRING from unpadded base64

              Type:
              string:string -> string

              Examples:
              > (base64-decode "aGVsbG8gd29ybGQh")
    |])
   ,("base64-encode", [text|
            native `base64-encode`

              Encode STRING as unpadded base64

              Type:
              string:string -> string

              Examples:
              > (base64-encode "hello world!")
    |])
   ,("bind", [text|
            native `bind`

              Special form evaluates SRC to an object which is bound to with BINDINGS over
              subsequent body statements.

              Type:
              src:object:<{row}> binding:binding:<{row}> -> <a>

              Examples:
              > (bind { "a": 1, "b": 2 } { "a" := a-value } a-value)
    |])
   ,("ceiling", [text|
            native `ceiling`

              Rounds up value of decimal X as integer, or to PREC precision as decimal.

              Type:
              x:decimal prec:integer -> decimal
              x:decimal -> integer

              Examples:
              > (ceiling 3.5)
              > (ceiling 100.15234 2)
    |])
   ,("chain-data", [text|
            native `chain-data`

              Get transaction public metadata. Returns an object with 'chain-id',
              'block-height', 'block-time', 'prev-block-hash', 'sender', 'gas-limit',
              'gas-price', and 'gas-fee' fields.

              Type:
               -> object:{public-chain-data}

              Examples:
              > (chain-data)
    |])
   ,("compose", [text|
            native `compose`

              Compose X and Y, such that X operates on VALUE, and Y on the results of X.

              Type:
              x:x:<a> -> <b> y:x:<b> -> <c> value:<a> -> <c>

              Examples:
              > (filter (compose (length) (< 2)) ["my" "dog" "has" "fleas"])
    |])
   ,("compose-capability", [text|
            native `compose-capability`

              Specifies and requests grant of CAPABILITY which is an application of a
              'defcap' production, only valid within a (distinct) 'defcap' body, as a way to
              compose CAPABILITY with the outer capability such that the scope of the
              containing 'with-capability' call will "import" this capability. Thus, a call
              to '(with-capability (OUTER-CAP) OUTER-BODY)', where the OUTER-CAP defcap
              calls '(compose-capability (INNER-CAP))', will result in INNER-CAP being
              granted in the scope of OUTER-BODY.

              Type:
              capability: -> bool -> bool

              Examples:
              (compose-capability (TRANSFER src dest))
    |])
   ,("concat", [text|
            native `concat`

              Takes STR-LIST and concats each of the strings in the list, returning the
              resulting string

              Type:
              str-list:[string] -> string

              Examples:
              > (concat ["k" "d" "a"])
              > (concat (map (+ " ") (str-to-list "abcde")))
    |])
   ,("constantly", [text|
            native `constantly`

              Lazily ignore arguments IGNORE* and return VALUE.

              Type:
              value:<a> ignore1:<b> -> <a>
              value:<a> ignore1:<b> ignore2:<c> -> <a>
              value:<a> ignore1:<b> ignore2:<c> ignore3:<d> -> <a>

              Examples:
              > (filter (constantly true) [1 2 3])
    |])
   ,("contains", [text|
            native `contains`

              Test that LIST or STRING contains VALUE, or that OBJECT has KEY entry.

              Type:
              value:<a> list:[<a>] -> bool
              key:<a> object:object:<{o}> -> bool
              value:string string:string -> bool

              Examples:
              > (contains 2 [1 2 3])
              > (contains 'name { 'name: "Ted", 'age: 72 })
              > (contains "foo" "foobar")
    |])
   ,("continue", [text|
            native `continue`

              Continue a previously started nested defpact.

              Type:
              value:* -> *

              Examples:
              (continue f)
    |])
   ,("create-capability-guard", [text|
            native `create-capability-guard`

              Creates a guard that will enforce that CAPABILITY is acquired.

              Type:
              capability: -> bool -> guard

              Examples:
              (create-capability-guard (BANK_DEBIT 10.0))
    |])
   ,("create-capability-pact-guard", [text|
            native `create-capability-pact-guard`

              Creates a guard that will enforce that CAPABILITY is acquired and that the
              currently-executing defpact is operational.

              Type:
              capability: -> bool -> guard

              Examples:
              (create-capability-pact-guard (ESCROW owner))
    |])
   ,("create-module-guard", [text|
            native `create-module-guard`

              Defines a guard by NAME that enforces the current module admin predicate.

              Type:
              name:string -> guard
    |])
   ,("create-pact-guard", [text|
            native `create-pact-guard`

              Defines a guard predicate by NAME that captures the results of 'pact-id'. At
              enforcement time, the success condition is that at that time 'pact-id' must
              return the same value. In effect this ensures that the guard will only succeed
              within the multi-transaction identified by the pact id.

              Type:
              name:string -> guard
    |])
   ,("create-principal", [text|
            native `create-principal`

              Create a principal which unambiguously identifies GUARD.

              Type:
              guard:guard -> string

              Examples:
              (create-principal (read-keyset 'keyset))
    |])
   ,("create-table", [text|
            native `create-table`

              Create table TABLE.

              Type:
              table:table:<{row}> -> string

              Examples:
              (create-table accounts)
    |])
   ,("create-user-guard", [text|
            native `create-user-guard`

              Defines a custom guard CLOSURE whose arguments are strictly evaluated at
              definition time, to be supplied to indicated function at enforcement time.

              Type:
              closure: -> bool -> guard
    |])
   ,("days", [text|
            native `days`

              N days, for use with 'add-time'

              Type:
              n:decimal -> decimal
              n:integer -> decimal

              Examples:
              > (add-time (time "2016-07-22T12:00:00Z") (days 1))
    |])
   ,("dec", [text|
            native `dec`

              Cast an integer to a decimal value of integer X as decimal.

              Type:
              x:integer -> decimal

              Examples:
              > (dec 3)
    |])
   ,("decrypt-cc20p1305", [text|
            native `decrypt-cc20p1305`

              Perform decryption of CIPHERTEXT using the CHACHA20-POLY1305 Authenticated
              Encryption with Associated Data (AEAD) construction described in IETF RFC
              7539. CIPHERTEXT is an unpadded base64url string. NONCE is a 12-byte base64
              string. AAD is base64 additional authentication data of any length. MAC is the
              "detached" base64 tag value for validating POLY1305 authentication. PUBLIC-KEY
              and SECRET-KEY are base-16 Curve25519 values to form the DH symmetric
              key.Result is unpadded base64URL.

              Type:
              ciphertext:string nonce:string aad:string mac:string public-key:string secret-key:string -> string

              Examples:
              (decrypt-cc20p1305 ciphertext nonce aad mac pubkey privkey)
    |])
   ,("define-keyset", [text|
            native `define-keyset`

              Define keyset as NAME with KEYSET, or if unspecified, read NAME from message
              payload as keyset, similarly to 'read-keyset'. If keyset NAME already exists,
              keyset will be enforced before updating to new value.

              Type:
              name:string keyset:string -> string
              name:string -> string

              Examples:
              (define-keyset 'admin-keyset (read-keyset "keyset"))
    |])
   ,("define-namespace", [text|
            native `define-namespace`

              Create a namespace called NAMESPACE where ownership and use of the namespace
              is controlled by GUARD. If NAMESPACE is already defined, then the guard
              previously defined in NAMESPACE will be enforced, and GUARD will be rotated in
              its place.

              Type:
              namespace:string user-guard:guard admin-guard:guard -> string

              Examples:
              (define-namespace 'my-namespace (read-keyset 'user-ks) (read-keyset 'admin-ks))
    |])
   ,("describe-keyset", [text|
            native `describe-keyset`

              Get metadata for KEYSET.

              Type:
              keyset:string -> object:*
    |])
   ,("describe-module", [text|
            native `describe-module`

              Get metadata for MODULE. Returns an object with 'name', 'hash', 'blessed',
              'code', and 'keyset' fields.

              Type:
              module:string -> object:*

              Examples:
              (describe-module 'my-module)
    |])
   ,("describe-namespace", [text|
            native `describe-namespace`

              Describe the namespace NS, returning a row object containing the user and
              admin guards of the namespace, as well as its name.

              Type:
              ns:string -> object:{described-namespace}

              Examples:
              (describe-namespace 'my-namespace)
    |])
   ,("describe-table", [text|
            native `describe-table`

              Get metadata for TABLE. Returns an object with 'name', 'hash', 'blessed',
              'code', and 'keyset' fields.

              Type:
              table:table:<{row}> -> object:*

              Examples:
              (describe-table accounts)
    |])
   ,("diff-time", [text|
            native `diff-time`

              Compute difference between TIME1 and TIME2 in seconds.

              Type:
              time1:time time2:time -> decimal

              Examples:
              > (diff-time (parse-time "%T" "16:00:00") (parse-time "%T" "09:30:00"))
    |])
   ,("distinct", [text|
            native `distinct`

              Returns from a homogeneous list of VALUES a list with duplicates removed. The
              original order of the values is preserved.

              Type:
              values:[<a>] -> [<a>]

              Examples:
              > (distinct [3 3 1 1 2 2])
    |])
   ,("drop", [text|
            native `drop`

              Drop COUNT values from LIST (or string), or entries having keys in KEYS from
              OBJECT. If COUNT is negative, drop from end. If COUNT exceeds the interval
              (-2^63,2^63), it is truncated to that range.

              Type:
              count:integer list:<a[[<l>],string]> -> <a[[<l>],string]>
              keys:[string] object:object:<{o}> -> object:<{o}>

              Examples:
              > (drop 2 "vwxyz")
              > (drop (- 2) [1 2 3 4 5])
              > (drop ['name] { 'name: "Vlad", 'active: false})
    |])
   ,("emit-event", [text|
            native `emit-event`

              Emit CAPABILITY as event without evaluating body of capability. Fails if
              CAPABILITY is not @managed or @event.

              Type:
              capability: -> bool -> bool

              Examples:
              (emit-event (TRANSFER "Bob" "Alice" 12.0))
    |])
   ,("enforce", [text|
            native `enforce`

              Fail transaction with MSG if pure expression TEST is false. Otherwise, returns
              true.

              Type:
              test:bool msg:string -> bool

              Examples:
              > (enforce (!= (+ 2 2) 4) "Chaos reigns")
    |])
   ,("enforce-guard", [text|
            native `enforce-guard`

              Execute GUARD, or defined keyset KEYSETNAME, to enforce desired predicate
              logic.

              Type:
              guard:guard -> bool
              keysetname:string -> bool

              Examples:
              (enforce-guard 'admin-keyset)
              (enforce-guard row-guard)
    |])
   ,("enforce-keyset", [text|
            native `enforce-keyset`

              Execute GUARD, or defined keyset KEYSETNAME, to enforce desired predicate
              logic.

              Type:
              guard:guard -> bool
              keysetname:string -> bool

              Examples:
              (enforce-keyset 'admin-keyset)
              (enforce-keyset row-guard)
    |])
   ,("enforce-one", [text|
            native `enforce-one`

              Run TESTS in order (in pure context, plus keyset enforces). If all fail, fail
              transaction. Short-circuits on first success.

              Type:
              msg:string tests:[bool] -> bool

              Examples:
              > (enforce-one "Should succeed on second test" [(enforce false "Skip me") (enforce (= (+ 2 2) 4) "Chaos reigns")])
    |])
   ,("enforce-pact-version", [text|
            native `enforce-pact-version`

              Enforce runtime pact version as greater than or equal MIN-VERSION, and less
              than or equal MAX-VERSION. Version values are matched numerically from the
              left, such that '2', '2.2', and '2.2.3' would all allow '2.2.3'.

              Type:
              min-version:string -> bool
              min-version:string max-version:string -> bool

              Examples:
              > (enforce-pact-version "2.3")
    |])
   ,("enumerate", [text|
            native `enumerate`

              Returns a sequence of numbers from FROM to TO (both inclusive) as a list. INC
              is the increment between numbers in the sequence. If INC is not given, it is
              assumed to be 1. Additionally, if INC is not given and FROM is greater than TO
              assume a value for INC of -1. If FROM equals TO, return the singleton list
              containing FROM, irrespective of INC's value. If INC is equal to zero, this
              function will return the singleton list containing FROM. If INC is such that
              FROM + INC > TO (when FROM < TO) or FROM + INC < TO (when FROM > TO) return
              the singleton list containing FROM. Lastly, if INC is such that FROM + INC <
              TO (when FROM < TO) or FROM + INC > TO (when FROM > TO), then this function
              fails.

              Type:
              from:integer to:integer inc:integer -> [integer]
              from:integer to:integer -> [integer]

              Examples:
              > (enumerate 0 10 2)
              > (enumerate 0 10)
              > (enumerate 10 0)
    |])
   ,("exp", [text|
            native `exp`

              Exp of X.

              Type:
              x:<a[integer,decimal]> -> <a[integer,decimal]>

              Examples:
              > (round (exp 3) 6)
    |])
   ,("filter", [text|
            native `filter`

              Filter LIST by applying APP to each element. For each true result, the
              original value is kept.

              Type:
              app:x:<a> -> bool list:[<a>] -> [<a>]

              Examples:
              > (filter (compose (length) (< 2)) ["my" "dog" "has" "fleas"])
    |])
   ,("floor", [text|
            native `floor`

              Rounds down value of decimal X as integer, or to PREC precision as decimal.

              Type:
              x:decimal prec:integer -> decimal
              x:decimal -> integer

              Examples:
              > (floor 3.5)
              > (floor 100.15234 2)
    |])
   ,("fold", [text|
            native `fold`

              Iteratively reduce LIST by applying APP to last result and element, starting
              with INIT.

              Type:
              app:x:<a> y:<b> -> <a> init:<a> list:[<b>] -> <a>

              Examples:
              > (fold (+) 0 [100 10 5])
    |])
   ,("fold-db", [text|
            native `fold-db`

              Select rows from TABLE using QRY as a predicate with both key and value, and
              then accumulate results of the query in CONSUMER. Output is sorted by the
              ordering of keys.

              Type:
              table:table:<{row}> qry:a:string b:object:<{row}> -> bool consumer:a:string b:object:<{row}> -> <b> -> [<b>]

              Examples:
              (let*
               ((qry (lambda (k obj) true)) ;; select all rows
                (f (lambda (x) [(at 'firstName x), (at 'b x)]))
               )
               (fold-db people (qry) (f))
              )
    |])
   ,("format", [text|
            native `format`

              Interpolate VARS into TEMPLATE using {}.

              Type:
              template:string vars:[*] -> string

              Examples:
              > (format "My {} has {}" ["dog" "fleas"])
    |])
   ,("format-time", [text|
            native `format-time`

              Format TIME using FORMAT. See ["Time Formats"
              docs](pact-reference.html#time-formats) for supported formats.

              Type:
              format:string time:time -> string

              Examples:
              > (format-time "%F" (time "2016-07-22T12:00:00Z"))
    |])
   ,("hash", [text|
            native `hash`

              Compute BLAKE2b 256-bit hash of VALUE represented in unpadded base64-url.
              Strings are converted directly while other values are converted using their
              JSON representation. Non-value-level arguments are not allowed.

              Type:
              value:<a> -> string

              Examples:
              > (hash "hello")
              > (hash { 'foo: 1 })
    |])
   ,("hours", [text|
            native `hours`

              N hours, for use with 'add-time'

              Type:
              n:decimal -> decimal
              n:integer -> decimal

              Examples:
              > (add-time (time "2016-07-22T12:00:00Z") (hours 1))
    |])
   ,("identity", [text|
            native `identity`

              Return provided value.

              Type:
              value:<a> -> <a>

              Examples:
              > (map (identity) [1 2 3])
    |])
   ,("if", [text|
            native `if`

              Test COND. If true, evaluate THEN. Otherwise, evaluate ELSE.

              Type:
              cond:bool then:<a> else:<a> -> <a>

              Examples:
              > (if (= (+ 2 2) 4) "Sanity prevails" "Chaos reigns")
    |])
   ,("insert", [text|
            native `insert`

              Write entry in TABLE for KEY of OBJECT column data, failing if data already
              exists for KEY.

              Type:
              table:table:<{row}> key:string object:object:<{row}> -> string

              Examples:
              (insert accounts id { "balance": 0.0, "note": "Created account." })
    |])
   ,("install-capability", [text|
            native `install-capability`

              Specifies, and provisions install of, a _managed_ CAPABILITY, defined in a
              'defcap' in which a '@managed' tag designates a single parameter to be managed
              by a specified function. After install, CAPABILITY must still be brought into
              scope using 'with-capability', at which time the 'manager function' is invoked
              to validate the request. The manager function is of type 'managed:<p>
              requested:<p> -> <p>', where '<p>' indicates the type of the managed
              parameter, such that for '(defcap FOO (bar:string baz:integer) @managed baz
              FOO-mgr ...)', the manager function would be '(defun FOO-mgr:integer
              (managed:integer requested:integer) ...)'. Any capability matching the
              'static' (non-managed) parameters will cause this function to be invoked with
              the current managed value and that of the requested capability. The function
              should perform whatever logic, presumably linear, to validate the request, and
              return the new managed value representing the 'balance' of the request. NOTE
              that signatures scoped to a managed capability cause the capability to be
              automatically provisioned for install similarly to one installed with this
              function.

              Type:
              capability: -> bool -> string

              Examples:
              (install-capability (PAY "alice" "bob" 10.0))
    |])
   ,("int-to-str", [text|
            native `int-to-str`

              Represent integer VAL as a string in BASE. BASE can be 2-16, or 64 for
              unpadded base64URL. Only positive values are allowed for base64URL conversion.

              Type:
              base:integer val:integer -> string

              Examples:
              > (int-to-str 16 65535)
              > (int-to-str 64 43981)
    |])
   ,("is-charset", [text|
            native `is-charset`

              Check that a string INPUT conforms to the a supported character set CHARSET.
              Character sets currently supported are: 'CHARSET_LATIN1' (ISO-8859-1), and
              'CHARSET_ASCII' (ASCII). Support for sets up through ISO 8859-5 supplement
              will be added in the future.

              Type:
              charset:integer input:string -> bool

              Examples:
              > (is-charset CHARSET_ASCII "hello world")
              > (is-charset CHARSET_ASCII "I am nÖt ascii")
              > (is-charset CHARSET_LATIN1 "I am nÖt ascii, but I am latin1!")
    |])
   ,("is-principal", [text|
            native `is-principal`

              Tell whether PRINCIPAL string conforms to the principal format without proving
              validity.

              Type:
              principal:string -> bool

              Examples:
              (enforce   (is-principal 'k:462e97a099987f55f6a2b52e7bfd52a36b4b5b470fed0816a3d9b26f9450ba69)   "Invalid account structure: non-principal account")
    |])
   ,("keylog", [text|
            native `keylog`

              Return updates to TABLE for a KEY in transactions at or after TXID, in a list
              of objects indexed by txid.

              Type:
              table:table:<{row}> key:string txid:integer -> [object:*]

              Examples:
              (keylog accounts "Alice" 123485945)
    |])
   ,("keys", [text|
            native `keys`

              Return all keys in TABLE.

              Type:
              table:table:<{row}> -> [string]

              Examples:
              (keys accounts)
    |])
   ,("keys-2", [text|
            native `keys-2`

              Keyset predicate function to match at least 2 keys in keyset.

              Type:
              count:integer matched:integer -> bool

              Examples:
              > (keys-2 3 1)
    |])
   ,("keys-all", [text|
            native `keys-all`

              Keyset predicate function to match all keys in keyset.

              Type:
              count:integer matched:integer -> bool

              Examples:
              > (keys-all 3 3)
    |])
   ,("keys-any", [text|
            native `keys-any`

              Keyset predicate function to match any (at least 1) key in keyset.

              Type:
              count:integer matched:integer -> bool

              Examples:
              > (keys-any 10 1)
    |])
   ,("keyset-ref-guard", [text|
            native `keyset-ref-guard`

              Creates a guard for the keyset registered as KEYSET-REF with 'define-keyset'.
              Concrete keysets are themselves guard types; this function is specifically to
              store references alongside other guards in the database, etc.

              Type:
              keyset-ref:string -> guard
    |])
   ,("length", [text|
            native `length`

              Compute length of X, which can be a list, a string, or an object.

              Type:
              x:<a[[<l>],string,object:<{o}>]> -> integer

              Examples:
              > (length [1 2 3])
              > (length "abcdefgh")
              > (length { "a": 1, "b": 2 })
    |])
   ,("list", [text|
            native `list`

              Create list from ELEMS. Deprecated in Pact 2.1.1 with literal list support.

              Type:
              elems:* -> [*]

              Examples:
              > (list 1 2 3)
    |])
   ,("list-modules", [text|
            native `list-modules`

              List modules available for loading.

              Type:
               -> [string]
    |])
   ,("ln", [text|
            native `ln`

              Natural log of X.

              Type:
              x:<a[integer,decimal]> -> <a[integer,decimal]>

              Examples:
              > (round (ln 60) 6)
    |])
   ,("log", [text|
            native `log`

              Log of Y base X.

              Type:
              x:<a[integer,decimal]> y:<a[integer,decimal]> -> <a[integer,decimal]>
              x:<a[integer,decimal]> y:<b[integer,decimal]> -> decimal

              Examples:
              > (log 2 256)
    |])
   ,("make-list", [text|
            native `make-list`

              Create list by repeating VALUE LENGTH times.

              Type:
              length:integer value:<a> -> [<a>]

              Examples:
              > (make-list 5 true)
    |])
   ,("map", [text|
            native `map`

              Apply APP to each element in LIST, returning a new list of results.

              Type:
              app:x:<b> -> <a> list:[<b>] -> [<a>]

              Examples:
              > (map (+ 1) [1 2 3])
    |])
   ,("minutes", [text|
            native `minutes`

              N minutes, for use with 'add-time'.

              Type:
              n:decimal -> decimal
              n:integer -> decimal

              Examples:
              > (add-time (time "2016-07-22T12:00:00Z") (minutes 1))
    |])
   ,("mod", [text|
            native `mod`

              X modulo Y.

              Type:
              x:integer y:integer -> integer

              Examples:
              > (mod 13 8)
    |])
   ,("namespace", [text|
            native `namespace`

              Set the current namespace to NAMESPACE. All expressions that occur in a
              current transaction will be contained in NAMESPACE, and once committed, may be
              accessed via their fully qualified name, which will include the namespace.
              Subsequent namespace calls in the same tx will set a new namespace for all
              declarations until either the next namespace declaration, or the end of the
              tx.

              Type:
              namespace:string -> string

              Examples:
              (namespace 'my-namespace)
    |])
   ,("not", [text|
            native `not`

              Boolean not.

              Type:
              x:bool -> bool

              Examples:
              > (not (> 1 2))
    |])
   ,("not?", [text|
            native `not?`

              Apply logical 'not' to the results of applying VALUE to APP.

              Type:
              app:x:<r> -> bool value:<r> -> bool

              Examples:
              > (not? (> 20) 15)
    |])
   ,("or", [text|
            native `or`

              Boolean logic with short-circuit.

              Type:
              x:bool y:bool -> bool

              Examples:
              > (or true false)
    |])
   ,("or?", [text|
            native `or?`

              Apply logical 'or' to the results of applying VALUE to A and B, with
              short-circuit.

              Type:
              a:x:<r> -> bool b:x:<r> -> bool value:<r> -> bool

              Examples:
              > (or? (> 20) (> 10) 15)
    |])
   ,("pact-id", [text|
            native `pact-id`

              Return ID if called during current pact execution, failing if not.

              Type:
               -> string
    |])
   ,("pact-version", [text|
            native `pact-version`

              Obtain current pact build version.

              Type:
               -> string

              Examples:
              > (pact-version)
    |])
   ,("pairing-check", [text|
            native `pairing-check`

              Perform pairing and final exponentiation points in G1 and G2 in BN254, check
              if the result is 1

              Type:
              points-g1:[<a>] points-g2:[<b>] -> bool
    |])
   ,("parse-time", [text|
            native `parse-time`

              Construct time from UTCVAL using FORMAT. See ["Time Formats"
              docs](pact-reference.html#time-formats) for supported formats.

              Type:
              format:string utcval:string -> time

              Examples:
              > (parse-time "%F" "2016-09-12")
    |])
   ,("point-add", [text|
            native `point-add`

              Add two points together that lie on the curve BN254. Point addition either in
              Fq or in Fq2

              Type:
              type:string point1:<a> point2:<a> -> <a>

              Examples:
              > (point-add 'g1 {'x: 1, 'y: 2}  {'x: 1, 'y: 2})
    |])
   ,("poseidon-hash-hack-a-chain", [text|
            native `poseidon-hash-hack-a-chain`

              Poseidon Hash Function. Note: This is a reference version of the Poseidon hash
              function used by Hack-a-Chain.

              Type:
              i:integer j:integer k:integer l:integer m:integer n:integer o:integer p:integer -> integer

              Examples:
              > (poseidon-hash-hack-a-chain 1)
              > (poseidon-hash-hack-a-chain 1 2)
              > (poseidon-hash-hack-a-chain 1 2 3 4 5 6)
              > (poseidon-hash-hack-a-chain 1 2 3 4 5 6 7 8)
    |])
   ,("public-chain-data", [text|
            (defschema
            public-chain-data
            "Schema type for data returned from 'chain-data'."

            [ chain-id:string
            , block-height:integer
            , block-time:time
            , prev-block-hash:string
            , sender:string
            , gas-limit:integer
            , gas-price:decimal ])
    |])
   ,("read", [text|
            native `read`

              Read row from TABLE for KEY, returning database record object, or just COLUMNS
              if specified.

              Type:
              table:table:<{row}> key:string -> object:<{row}>
              table:table:<{row}> key:string columns:[string] -> object:<{row}>

              Examples:
              (read accounts id ['balance 'ccy])
    |])
   ,("read-decimal", [text|
            native `read-decimal`

              Parse KEY string or number value from top level of message data body as
              decimal.

              Type:
              key:string -> decimal

              Examples:
              (defun exec ()
                 (transfer (read-msg "from") (read-msg "to") (read-decimal "amount")))
    |])
   ,("read-integer", [text|
            native `read-integer`

              Parse KEY string or number value from top level of message data body as
              integer.

              Type:
              key:string -> integer

              Examples:
              (read-integer "age")
    |])
   ,("read-keyset", [text|
            native `read-keyset`

              Read KEY from message data body as keyset ({ "keys": KEYLIST, "pred": PREDFUN
              }). PREDFUN should resolve to a keys predicate.

              Type:
              key:string -> keyset

              Examples:
              (read-keyset "admin-keyset")
    |])
   ,("read-msg", [text|
            native `read-msg`

              Read KEY from top level of message data body, or data body itself if not
              provided. Coerces value to their corresponding pact type: String -> string,
              Number -> integer, Boolean -> bool, List -> list, Object -> object.

              Type:
               -> <a>
              key:string -> <a>

              Examples:
              (defun exec ()
                 (transfer (read-msg "from") (read-msg "to") (read-decimal "amount")))
    |])
   ,("read-string", [text|
            native `read-string`

              Parse KEY string or number value from top level of message data body as
              string.

              Type:
              key:string -> string

              Examples:
              (read-string "sender")
    |])
   ,("remove", [text|
            native `remove`

              Remove entry for KEY from OBJECT.

              Type:
              key:string object:object:<{o}> -> object:<{o}>

              Examples:
              > (remove "bar" { "foo": 1, "bar": 2 })
    |])
   ,("require-capability", [text|
            native `require-capability`

              Specifies and tests for existing grant of CAPABILITY, failing if not found in
              environment.

              Type:
              capability: -> bool -> bool

              Examples:
              (require-capability (TRANSFER src dest))
    |])
   ,("resume", [text|
            native `resume`

              Special form binds to a yielded object value from the prior step execution in
              a pact. If yield step was executed on a foreign chain, enforce endorsement via
              SPV.

              Type:
              binding:binding:<{r}> -> <a>
    |])
   ,("reverse", [text|
            native `reverse`

              Reverse LIST.

              Type:
              list:[<a>] -> [<a>]

              Examples:
              > (reverse [1 2 3])
    |])
   ,("round", [text|
            native `round`

              Performs Banker's rounding value of decimal X as integer, or to PREC precision
              as decimal.

              Type:
              x:decimal prec:integer -> decimal
              x:decimal -> integer

              Examples:
              > (round 3.5)
              > (round 100.15234 2)
    |])
   ,("scalar-mult", [text|
            native `scalar-mult`

              Multiply a point that lies on the curve BN254 by an integer value

              Type:
              type:string point1:<a> scalar:integer -> <a>

              Examples:
              > (scalar-mult 'g1 {'x: 1, 'y: 2} 2)
    |])
   ,("select", [text|
            native `select`

              Select full rows or COLUMNS from table by applying WHERE to each row to get a
              boolean determining inclusion.

              Type:
              table:table:<{row}> where:row:object:<{row}> -> bool -> [object:<{row}>]
              table:table:<{row}> columns:[string] where:row:object:<{row}> -> bool -> [object:<{row}>]

              Examples:
              (select people ['firstName,'lastName] (where 'name (= "Fatima")))
              (select people (where 'age (> 30)))?
    |])
   ,("shift", [text|
            native `shift`

              Shift X Y bits left if Y is positive, or right by -Y bits otherwise. Right
              shifts perform sign extension on signed number types; i.e. they fill the top
              bits with 1 if the x is negative and with 0 otherwise.

              Type:
              x:integer y:integer -> integer

              Examples:
              > (shift 255 8)
              > (shift 255 -1)
              > (shift -255 8)
              > (shift -255 -1)
    |])
   ,("sort", [text|
            native `sort`

              Sort a homogeneous list of primitive VALUES, or objects using supplied FIELDS
              list.

              Type:
              values:[<a>] -> [<a>]
              fields:[string] values:[object:<{o}>] -> [object:<{o}>]

              Examples:
              > (sort [3 1 2])
              > (sort ['age] [{'name: "Lin",'age: 30} {'name: "Val",'age: 25}])
    |])
   ,("sqrt", [text|
            native `sqrt`

              Square root of X.

              Type:
              x:<a[integer,decimal]> -> <a[integer,decimal]>

              Examples:
              > (sqrt 25)
    |])
   ,("str-to-int", [text|
            native `str-to-int`

              Compute the integer value of STR-VAL in base 10, or in BASE if specified.
              STR-VAL can be up to 512 chars in length. BASE must be between 2 and 16, or 64
              to perform unpadded base64url conversion. Each digit must be in the correct
              range for the base.

              Type:
              str-val:string -> integer
              base:integer str-val:string -> integer

              Examples:
              > (str-to-int 16 "abcdef123456")
              > (str-to-int "123456")
              > (str-to-int 64 "q80")
    |])
   ,("str-to-list", [text|
            native `str-to-list`

              Takes STR and returns a list of single character strings

              Type:
              str:string -> [string]

              Examples:
              > (str-to-list "hello")
              > (concat (map (+ " ") (str-to-list "abcde")))
    |])
   ,("take", [text|
            native `take`

              Take COUNT values from LIST (or string), or entries having keys in KEYS from
              OBJECT. If COUNT is negative, take from end. If COUNT exceeds the interval
              (-2^63,2^63), it is truncated to that range.

              Type:
              count:integer list:<a[[<l>],string]> -> <a[[<l>],string]>
              keys:[string] object:object:<{o}> -> object:<{o}>

              Examples:
              > (take 2 "abcd")
              > (take (- 3) [1 2 3 4 5])
              > (take ['name] { 'name: "Vlad", 'active: false})
    |])
   ,("time", [text|
            native `time`

              Construct time from UTCVAL using ISO8601 format (%Y-%m-%dT%H:%M:%SZ).

              Type:
              utcval:string -> time

              Examples:
              > (time "2016-07-22T11:26:35Z")
    |])
   ,("try", [text|
            native `try`

              Attempt a pure ACTION, returning DEFAULT in the case of failure. Pure
              expressions are expressions which do not do i/o or work with non-deterministic
              state in contrast to impure expressions such as reading and writing to a
              table.

              Type:
              default:<a> action:<a> -> <a>

              Examples:
              > (try 3 (enforce (= 1 2) "this will definitely fail"))
              (expect "impure expression fails and returns default" "default" (try "default" (with-read accounts id {'ccy := ccy}) ccy))
    |])
   ,("tx-hash", [text|
            native `tx-hash`

              Obtain hash of current transaction as a string.

              Type:
               -> string

              Examples:
              > (tx-hash)
    |])
   ,("txids", [text|
            native `txids`

              Return all txid values greater than or equal to TXID in TABLE.

              Type:
              table:table:<{row}> txid:integer -> [integer]

              Examples:
              (txids accounts 123849535)
    |])
   ,("txlog", [text|
            native `txlog`

              Return all updates to TABLE performed in transaction TXID.

              Type:
              table:table:<{row}> txid:integer -> [object:*]

              Examples:
              (txlog accounts 123485945)
    |])
   ,("typeof", [text|
            native `typeof`

              Returns type of X as string.

              Type:
              x:<a> -> string

              Examples:
              > (typeof "hello")
    |])
   ,("typeof-principal", [text|
            native `typeof-principal`

              Return the protocol type of a given PRINCIPAL value. If input value is not a
              principal type, then the empty string is returned.

              Type:
              principal:string -> string

              Examples:
              (typeof-principal 'k:462e97a099987f55f6a2b52e7bfd52a36b4b5b470fed0816a3d9b26f9450ba69)
    |])
   ,("update", [text|
            native `update`

              Write entry in TABLE for KEY of OBJECT column data, failing if data does not
              exist for KEY.

              Type:
              table:table:<{row}> key:string object:object:~<{row}> -> string

              Examples:
              (update accounts id { "balance": (+ bal amount), "change": amount, "note": "credit" })
    |])
   ,("validate-keypair", [text|
            native `validate-keypair`

              Enforce that the Curve25519 keypair of (PUBLIC,SECRET) match. Key values are
              base-16 strings of length 32.

              Type:
              public:string secret:string -> bool

              Examples:
              (validate-keypair pubkey privkey)
    |])
   ,("validate-principal", [text|
            native `validate-principal`

              Validate that PRINCIPAL unambiguously identifies GUARD.

              Type:
              guard:guard principal:string -> bool

              Examples:
              (enforce (validate-principal (read-keyset 'keyset) account) "Invalid account ID")
    |])
   ,("verify-spv", [text|
            native `verify-spv`

              Performs a platform-specific spv proof of type TYPE on PAYLOAD. The format of
              the PAYLOAD object depends on TYPE, as does the format of the return object.
              Platforms such as Chainweb will document the specific payload types and return
              values.

              Type:
              type:string payload:object:<in> -> object:<out>

              Examples:
              (verify-spv "TXOUT" (read-msg "proof"))
    |])
   ,("where", [text|
            native `where`

              Utility for use in 'filter' and 'select' applying APP to FIELD in VALUE.

              Type:
              field:string app:x:<a> -> bool value:object:<{row}> -> bool

              Examples:
              > (filter (where 'age (> 20)) [{'name: "Mary",'age: 30} {'name: "Juan",'age: 15}])
    |])
   ,("with-capability", [text|
            native `with-capability`

              Specifies and requests grant of _acquired_ CAPABILITY which is an application
              of a 'defcap' production. Given the unique token specified by this
              application, ensure that the token is granted in the environment during
              execution of BODY. 'with-capability' can only be called in the same module
              that declares the corresponding 'defcap', otherwise module-admin rights are
              required. If token is not present, the CAPABILITY is evaluated, with
              successful completion resulting in the installation/granting of the token,
              which will then be revoked upon completion of BODY. Nested 'with-capability'
              calls for the same token will detect the presence of the token, and will not
              re-apply CAPABILITY, but simply execute BODY. 'with-capability' cannot be
              called from within an evaluating defcap. Acquire of a managed capability
              results in emission of the equivalent event.

              Type:
              capability: -> bool body:[*] -> <a>

              Examples:
              (with-capability (UPDATE-USERS id) (update users id { salary: new-salary }))
    |])
   ,("with-default-read", [text|
            native `with-default-read`

              Special form to read row from TABLE for KEY and bind columns per BINDINGS over
              subsequent body statements. If row not found, read columns from DEFAULTS, an
              object with matching key names.

              Type:
              table:table:<{row}> key:string defaults:object:~<{row}> bindings:binding:~<{row}> -> <a>

              Examples:
              (with-default-read accounts id { "balance": 0, "ccy": "USD" } { "balance":= bal, "ccy":= ccy }
                (format "Balance for {} is {} {}" [id bal ccy]))
    |])
   ,("with-read", [text|
            native `with-read`

              Special form to read row from TABLE for KEY and bind columns per BINDINGS over
              subsequent body statements.

              Type:
              table:table:<{row}> key:string bindings:binding:<{row}> -> <a>

              Examples:
              (with-read accounts id { "balance":= bal, "ccy":= ccy }
                (format "Balance for {} is {} {}" [id bal ccy]))
    |])
   ,("write", [text|
            native `write`

              Write entry in TABLE for KEY of OBJECT column data.

              Type:
              table:table:<{row}> key:string object:object:<{row}> -> string

              Examples:
              (write accounts id { "balance": 100.0 })
    |])
   ,("xor", [text|
            native `xor`

              Compute bitwise X xor Y.

              Type:
              x:integer y:integer -> integer

              Examples:
              > (xor 127 64)
              > (xor 5 -7)
    |])
   ,("yield", [text|
            native `yield`

              Yield OBJECT for use with 'resume' in following pact step. With optional
              argument TARGET-CHAIN, target subsequent step to execute on targeted chain
              using automated SPV endorsement-based dispatch.

              Type:
              object:object:<{y}> -> object:<{y}>
              object:object:<{y}> target-chain:string -> object:<{y}>

              Examples:
              (yield { "amount": 100.0 })
              (yield { "amount": 100.0 } "some-chain-id")
    |])
   ,("zip", [text|
            native `zip`

              Combine two lists with some function f, into a new list, the length of which
              is the length of the shortest list.

              Type:
              f:x:<a> y:<b> -> <c> list1:[<a>] list2:[<b>] -> [<c>]

              Examples:
              > (zip (+) [1 2 3 4] [4 5 6 7])
              > (zip (-) [1 2 3 4] [4 5 6])
              > (zip (+) [1 2 3] [4 5 6 7])
    |])
   ,("|", [text|
            native `|`

              Compute bitwise X or Y.

              Type:
              x:integer y:integer -> integer

              Examples:
              > (| 2 3)
              > (| 5 -7)
    |])
   ,("~", [text|
            native `~`

              Reverse all bits in X.

              Type:
              x:integer -> integer

              Examples:
              > (~ 15)
    |])
    ]
