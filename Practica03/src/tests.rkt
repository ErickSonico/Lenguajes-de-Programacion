#lang plai

(require "grammars.rkt")
(require "parser.rkt")
(require "interp.rkt")

(define (prueba e)
  (interp (parse e)))

(test (parse 'x) (id 'x))
(test (parse '2) (num 2))
(test (parse '#t) (bool #t))
(test (parse '"Hola") (strinG "Hola"))
(test (parse '{with ([x 2] [y 3]) ( + x y)}) (with (list (binding 'x (num 2)) (binding 'y (num 3))) (op + (list (id 'x) (id 'y)))))
(test (parse '{sub1 2}) (op sub1 (list (num 2))))
(test/exn (parse '{sub1 2 3}) "parse: La operación sub1 debe ser ejecutada con 1 argumentos.")
(test/exn (parse '{min}) "parse: La operación min debe ser ejecutada con mas de 0 argumentos.")

(test/exn (prueba 'foo) "interp: Variable libre: 'foo")
(test (prueba '1234) 1234)
(test (prueba '#t) #t)
(test (prueba '#f) #f)
(test (prueba '"Hi") "Hi")
(test (prueba '{+ 1 2 3}) 6)
(test (prueba '{- 1 2 3}) -4)
(test (prueba '{* 1 2 -3}) -6)
(test (prueba '{* 1 2 -3 0}) 0)
(test (prueba '{/ 1 2}) (/ 1 2))
(test (prueba '{min 20 90 -1 0}) -1)
(test (prueba '{max 20 90 -1 0}) 90)
(test (prueba '{sqrt 20}) 4.47213595499958)
(test (prueba '{modulo 3 2}) 1)
(test (prueba '{sub1 3}) 2)
(test (prueba '{sub1 (add1 (expt 3 3))}) 27)
(test (prueba '{str-length "Hello"}) 5)
(test (prueba '{> 10 9}) #t)
(test (prueba '{> 1 2}) #f)
(test (prueba '{> 10 9 8 7 6 5 4 3 2 1}) #t)
(test (prueba '{> 1 2 3 4 5 6 7 8}) #f)
(test (prueba '{= 1 2 3 4 5 6 7 8}) #f)
(test (prueba '{= 10 10}) #t)
(test (prueba '{zero? 10}) #f)
(test (prueba '{zero? 0}) #t)
(test (prueba '{num? 10}) #t)
(test (prueba '{bool? 10}) #f)
(test (prueba '{bool? {and {zero? {add1 1}}
                           {num? "Hello"}}}) #t)
(test (prueba '{or {zero? {+ -1 1}}
                   {num? {sub1 0}}
                   {bool? {str? "Hi"}}}) #t)
(test/exn (prueba 'a) "interp: Variable libre: 'a")
(test (prueba '{with {[x 2] [y 3]} {+ x 3 y}}) 8)
(test (prueba '{with {{x 5} {y 1}} {+ x y}}) 6)
(test (prueba '{with* {{x 5} {y 1}} {+ x y}}) 6)
(test (prueba '{with* {{x 5} {y {+ x 1}}} {+ x y}}) 11)
(test/exn (prueba'{with {{a 2} {b {+ a a}}} b}) "interp: Variable libre: 'a")
(test (prueba'{with* {{a 2} {b {+ a a}}} b}) 4)
(test/exn (prueba'{with {{y 1} {x y}} x}) "interp: Variable libre: 'y")
(test (prueba'{with* {{x y} {y 1}} x}) 1)
(test (prueba '{with {{x 5}}
                     {with {{x 2}}
                           {+ x x}}}) 4)
(test (prueba '{with ([x 2] [y 1] [z 3]) (+ x y z)}) 6)
(test (prueba'{with ([x 2] [y 1] [z 3]) (/ x y z)}) 2/3)

(test (prueba '{with ([x 1]
                      [y 1])
                     {with ([x 5]
                            [y x])
                           (+ x y)}}) 6)
(test (prueba '{with ([x 1]
                      [y 1])
                     {with* ([x 5]
                             [y x])
                            (+ x y)}}) 10)

(test/exn (prueba '{with ([x 1]
                          [x 2])
                         {+ x x}})
          "parse: El identificador x está declarado más de una vez")

(test (prueba '{with {{x 10}}
                     {with* {{y x} {z {+ x x}} {x 0} {a {add1 x}}}
                            {+ x {with {{b a}}
                                       a}}}}) 1)