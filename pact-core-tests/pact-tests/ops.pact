;; ops.repl : tests for operators, math

"===== +"
(expect "+ integer integer" 4 (suspend (+ 2 2)))
;  (expect "+ integer decimal" 4.0 (+ 1 3.0))
;  (expect "+ decimal integer" 4.0 (+ 1.0 3))
(expect "+ decimal decimal" 7.0 (suspend (+ 1.2 5.8)))
(expect "+ string string" "foobar" (suspend (+ "foo" "bar")))
(expect "+ list list" [1, 2, 3, 4, 5] (suspend (+ [1, 2, 3] [4, 5])))

"===== -"
(expect "- integer, - integer integer" (negate 2) (suspend (- 0 2)))
(expect "- decimal, - decimal decimal" (negate 4.5) (suspend (- 3.5 8.0)))
(expect "neg integer lit, - integer integer" -2 (suspend (- 0 2)))
(expect "neg dec lit, - decimal decimal" -4.5 (suspend (- 3.5 8.0)))

"===== *"
(expect "* integer integer" 6 (suspend (* 2 3)))
(expect "* decimal decimal" 18.0 (suspend (* 1.5 12.0)))

"===== /"
(expect "/ integer integer" 2 (suspend (/ 11 5)))
(expect "/ decimal decimal" 6.0 (suspend (/ 15.0 2.5)))
(expect-failure "div by zero" (suspend (/ 11 0)))
(expect-failure "div by zero" (suspend (/ 11.0 0.0)))

"===== logic"
(expect "andTT" true (suspend (and true true)))
(expect "andTF" false (suspend (and true false)))
(expect "andFT" false (suspend (and false true)))
(expect "andFF" false (suspend (and false false)))
(expect "orTT" true (suspend (or true true)))
(expect "orTF" true (suspend (or true false)))
(expect "orFT" true (suspend (or false true)))
(expect "orFF" false (suspend (or false false)))
(expect "not" true (suspend (not false)))
;; Todo: these
 (expect "and short-circuit" false
         (suspend (and false (error "shouldn't happen"))))
 (expect "or short-circuit" true
         (suspend (or true (error "shouldn't happen"))))

"===== comparators, eq"
(expect "< decimal decimal" true (suspend (< 10.0 11.0)))
(expect "<= decimal decimal" true (suspend (<= 11.0 11.0)))
(expect "> decimal decimal" true (suspend (> 11.0 10.0)))
(expect ">= decimal decimal" true (suspend (>= 11.1 11.0)))
(expect "= decimal decimal" true (suspend (== 10.0 10.0)))
(expect "!= decimal decimal" true (suspend (!= 10.0 10.1)))

(expect "< integer integer" true (suspend (< 10 11)))
(expect "<= integer integer" true (suspend (<= 11 11)))
(expect "> integer integer" true (suspend (> 11 10)))
(expect ">= integer integer" true (suspend (>= 12 11)))
(expect "= integer integer" true (suspend (== 10 10)))
(expect "!= integer integer" true (suspend (!= 10 88)))

; Todo: typecheck failures here
;; (expect-failure "< decimal integer" (< 10.0 11))
;; (expect-failure "<= decimal integer" (<= 11.0 11))
;; (expect-failure "> decimal integer" (> 11.0 10))
;; (expect-failure ">= decimal integer" (>= 12.0 11))
;; (expect "= decimal integer false" false (= 10.0 10))

; Todo: typecheck failures here
;; (expect-failure "< integer decimal" (< 10 11.0))
;; (expect-failure "<= integer decimal" (<= 11 11.0))
;; (expect-failure "> integer decimal" (> 11 10.0))
;; (expect-failure ">= integer decimal" (>= 12 11.0))
;; (expect "= integer decimal false" false (= 10 10.0))

(expect "< string string" true (suspend (< "a" "b")))
(expect "<= string string" true (suspend (<= "a" "a")))
(expect "> string string" true (suspend (> "b" "a")))
(expect ">= string string" true (suspend (>= "ba" "a")))
(expect "= string string" true (suspend (== "hello" "hello")))
(expect "!= string string" true (suspend (!= "hello" "goodbye")))

(expect "= bool bool" true (suspend (== true true)))
(expect "!= bool bool" true (suspend (!= false true)))

;; (interface ix (defun f:bool ()))
;; (interface iy (defun f:bool ()))
;; (module mx g (defcap g () true) (implements ix) (defun f:bool () true))
;; (module my g (defcap g () true) (implements iy) (defun f:bool () true))
;; (module ny g (defcap g () true) (implements iy) (defun f:bool () true))

;; (expect "= module{ix} module{ix}" true (= mx mx))
;; (expect "!= module{ix} module{ix}" true (!= my mx))

"===== math"
(expect "sqrt decimal" 4.0 (suspend (sqrt 16.0)))
(expect "sqrt integer" 4.0 (suspend (sqrt 16)))
(expect "mod" 3 (suspend (mod 8 5)))
;  (expect "log" 4.0 (suspend (log 2.0 16)))
;  (expect "ln" 4.094 (suspend (round (ln 60) 3)))
;  (expect "exp" 20.086 (suspend (round (exp 3) 3)))
;  (expect "abs" 10.23 (suspend (abs (- 10.23))))

"===== truncating"
(expect "round" 100 (suspend (round 100.1)))
(expect "ceiling" 101 (suspend (ceiling 100.001)))
(expect "floor" 101 (suspend (ceiling 100.001)))
;  (expect "round 3 prec" 100.124 (suspend (round 100.1239 3)))
;  (expect "ceiling 2 prec" 101.95 (suspend (ceiling 101.9401 2)))
;  (expect "floor 1 prec" 100.1 (suspend (ceiling 100.01 1)))

"===== list equality"
(expect "= list list" true (suspend (== [1, 2, 3] [1, 2, 3])))
;  (expect "not = list list" false (= [1, 3, 2] [1, 2, 3]))
;  (expect "not != list list" false (!= [1, 2, 3] [1, 2, 3]))
;  (expect "!= list list" true (!= [1, 3, 2] [1, 2, 3]))

;; "===== object equality"
;; (expect "= object object" true (= { "a": 1 } { "a": 1 }))
;; (expect "not = object object" false (= { "a": 1 } { "a": 1, "b": 2 }))
;; (expect "not != object object" false (!= { "a": 1 } { "a": 1 }))
;; (expect "!= object object" true (!= { "a": 1 } { "a": 1, "b": 2 }))

;; "===== keyset equality"
;; (env-data { "k1": ["k1"], "k2": ["k2"] })
;; (expect "= keyset keyset" true (= (read-keyset "k1") (read-keyset "k1")))
;; (expect "not = keyset keyset" false (= (read-keyset "k1") (read-keyset "k2")))
;; (expect "not != keyset keyset" false (!= (read-keyset "k1") (read-keyset "k1")))
;; (expect "!= keyset keyset" true (!= (read-keyset "k1") (read-keyset "k2")))

;; "===== keyset ref equality"
;; (env-exec-config ["DisablePact44"])
;; (env-data { "k1": ["k1"], "k2": ["k2"] })
;; (env-keys ["k1" "k2"])
;; (define-keyset 'k1 (read-keyset "k1"))
;; (define-keyset 'k2 (read-keyset "k2"))
;; (expect "= keysetRef keysetRef" true (= (keyset-ref-guard "k1") (keyset-ref-guard "k1")))
;; (expect "not = keysetRef keysetRef" false (= (keyset-ref-guard "k1") (keyset-ref-guard "k2")))
;; (expect "not != keysetRef keysetRef" false (!= (keyset-ref-guard "k1") (keyset-ref-guard "k1")))
;; (expect "!= keysetRef keysetRef" true (!= (keyset-ref-guard "k1") (keyset-ref-guard "k2")))

;; (module tm G
  ;; (defcap G () true)
  ;; (defun mk (id) (create-module-guard id))
  ;; (defpact p (id1 id2)
    ;; (step [(create-pact-guard id1) (create-pact-guard id2)]))
  ;; (defun ug (id) true)
  ;; )

;; "===== module guard equality"
;; (expect "= moduleGuard moduleGuard" true (= (tm.mk "1") (tm.mk "1")))
;; (expect "not = moduleGuard moduleGuard" false (= (tm.mk "2") (tm.mk "1")))
;; (expect "not != moduleGuard moduleGuard" false (!= (tm.mk "1") (tm.mk "1")))
;; (expect "!= moduleGuard moduleGuard" true (!= (tm.mk "2") (tm.mk "1")))

;; "===== pact guard equality"
;; (env-hash "YQo")
;; (let ((pgs (tm.p "1" "2")))
  ;; (expect "= pactGuard pactGuard" true (= (at 0 pgs) (at 0 pgs)))
  ;; (expect "not = pactGuard pactGuard" false (= (at 1 pgs) (at 0 pgs)))
  ;; (expect "not != pactGuard pactGuard" false (!= (at 0 pgs) (at 0 pgs)))
  ;; (expect "!= pactGuard pactGuard" true (!= (at 1 pgs) (at 0 pgs)))
  ;; )


;; "===== userGuard equality"
;; (expect "= userGuard userGuard" true (= (create-user-guard (tm.ug "1")) (create-user-guard (tm.ug "1"))))
;; (expect "not = userGuard userGuard" false (= (create-user-guard (tm.ug "2")) (create-user-guard (tm.ug "1"))))
;; (expect "not != userGuard userGuard" false (!= (create-user-guard (tm.ug "1")) (create-user-guard (tm.ug "1"))))
;; (expect "!= userGuard userGuard" true (!= (create-user-guard (tm.ug "2")) (create-user-guard (tm.ug "1"))))


;; "===== bitwise"
;; (expect "bitwise 2 and 3" 2 (& 2 3))
;; (expect "bitwise 5 and -7" 1 (& 5 -7))
;; (expect "bitwise 2 or 3" 3 (| 2 3))
;; (expect "bitwise 5 or -7" -3 (| 5 -7))
;; (expect "bitwise 2 xor 4" 6 (xor 2 4))
;; (expect "bitwise 5 xor -7" -4 (xor 5 -7))
;; (expect "complement 15" -16 (~ 15))
;; (expect "shift 255 8" 65280 (shift 255 8))
;; (expect "shift -255 8" -65280 (shift -255 8))
;; (expect "shift 255 -1" 127 (shift 255 -1))
;; (expect "shift -255 -1" -128 (shift -255 -1))

;; "===== drop"
;; (expect "pos drop within range" [3 4 5] (drop 2 [1 2 3 4 5]))
;; (expect "neg drop within range" [1 2 3] (drop -2 [1 2 3 4 5]))
;; (expect "pos drop beyond range" [] (drop 10 [1 2 3 4 5]))
;; (expect "neg drop beyond range" [] (drop -10 [1 2 3 4 5]))
;; (expect "pos drop at 63-bit boundary" (drop 9223372036854775807 [1 2 3 4 5]) (drop 9223372036854775808 [1 2 3 4 5]))
;; (expect "neg drop at 63-bit boundary" (drop -9223372036854775807 [1 2 3 4 5]) (drop -9223372036854775808 [1 2 3 4 5]))

;; "===== take"
;; (expect "pos take within range" [1 2] (take 2 [1 2 3 4 5]))
;; (expect "neg take within range" [4 5] (take -2 [1 2 3 4 5]))
;; (expect "pos take beyond range" [1 2 3 4 5] (take 10 [1 2 3 4 5]))
;; (expect "neg take beyond range" [1 2 3 4 5] (take -10 [1 2 3 4 5]))
;; (expect "pos take at 63-bit boundary" (take 9223372036854775807 [1 2 3 4 5]) (take 9223372036854775808 [1 2 3 4 5]))
;; (expect "neg take at 63-bit boundary" (take -9223372036854775807 [1 2 3 4 5]) (take -9223372036854775808 [1 2 3 4 5]))
