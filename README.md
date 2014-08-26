# ylang [![Build Status](http://img.shields.io/travis/VoQn/ylang.svg?style=flat-square)](https://travis-ci.org/VoQn/ylang) [![Gitter](http://img.shields.io/badge/discuss-Gitter-brightgreen.svg?style=flat-square)](https://gitter.im/VoQn/ylang) [![Apache 2 License](http://img.shields.io/badge/license-Apache_2-brightgreen.svg?style=flat-square)](https://tldrlegal.com/license/apache-license-2.0-(apache-2.0)
ylang is Programming Language. Scheme like, but it is *NOT* _Lisp family_

## Motivation
### Simplify to think "What is this?"

**ylang** aims to simplify design for programming.

**Programming** has many concepts easy to confuse, and too easy to bind self (regacy own code, uncontrollable dependency, language spec, etc ...)

**ylang** is designing to "Easy to Re-design own".
- To be _as possible as_ easy to _rewrite own_
- To be _as possible as_ easy to _restruction own_

## Syntax Base
### Foundation
#### TopLevel
```scheme
; <- Comment

; Apply Function
(<FACTOR> <EXPR>)

; Eval Expression
(<EXPR>)
```
#### Literal
```scheme
([])  ; Empty List
(1)   ; Integer
(0.5) ; Floating
(1/2) ; Rational
(yes) ; Boolean (true)
(no)  ; Boolean (false)
('a') ; Charactor
("a") ; String
```

#### Builtin Operator
##### Definition
```scheme
(:) ; Declare Type Binding (: x Int)
(=) ; Define [Value / Function] (= (add x y) (+ x y))
(->) ; Arrow (-> a a) -> FunctionType (a -> a)
```
##### Function
```scheme
(\) ; Lambda (((\ x) x) 1) -> (1)
(.) ; Compose ((. f g) x) -> (f (g x))
```
##### Collection
```scheme
(,) ; Pair (, x []) -> [x]
```
##### Ordering
```scheme
(>?) ; GT (>? 1 2) -> no
(<?) ; LT (<? 1 2) -> yes
(=?) ; EQ (=? 2 2) -> yes
```
##### Numeric
```scheme
(+) ; Add (+ 1 1) -> (1)
(-) ; Sub (- 1 1) -> (0)
(*) ; Mul (* 2 2) -> (4)
(/) ; Div (/ 1 2) -> (1/2)
```
##### Boolean
```scheme
(&) ; And (& yes no) -> (no)
(|) ; Or  (| no yes) -> (yes)
(~) ; Not (~ yes)    -> (no)
```
### Let, Closure
```scheme
(= (: x Int) 10)
(= y 20)

(+ x 1)
; (: 11 Int)

(+ x y)
; (: 30 Int)

(((\ x) (+ x x)) 1)
; (: 2 Int)
(((\ x) (+ x y)) 1)
; (: 21 Int)
```

### Declare, Define, Apply
```lisp
(: (length [a]) Int)
(= (length []) 0)
(= (length (, x xs)) (+ (length xs) 1))

(length [1 2 3 4 5])
; (: 5 Int)
```

## License
See [License file]( https://github.com/VoQn/ylang/blob/master/LICENSE)
