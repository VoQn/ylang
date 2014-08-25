# ylang : Programming Language
Scheme like, but it is **NOT** _Lisp family_

## Build Status
[![Build Status](https://travis-ci.org/VoQn/ylang.svg?branch=master)](https://travis-ci.org/VoQn/ylang)

## Motivation
### Simplify to think "What is this?"

**ylang** aims to simplify design for programming.

**Programming** has many concepts easy to confuse, and too easy to bind self (regacy own code, uncontrollable dependency, language spec, etc ...)

**ylang** is designing to "Easy to Re-design own".
- To be _as possible as_ easy to _rewrite own_
- To be _as possible as_ easy to _restruction own_

## Syntax Base

### Let, Closure
```
(x : Int = 10)
(y = 20)

(+ x 1)
; (11 : Int)
(x + 1)
; (11 : Int)

(x + y)
; (30 : Int)

((x -> x + x) 1)
; (2 : Int)
((x -> x + y) 1)
; (21 : Int)
```

### Declare, Define, Apply
```
(length : [a] -> Int)
(length [] = 0)
(length (x,xs) = length xs + 1)

(length [1 2 3 4 5])
; (5 : Int)
```

## License
See [License file]( https://github.com/VoQn/ylang/blob/master/LICENSE)
