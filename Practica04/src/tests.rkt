#lang plai

(require "grammars.rkt")
(require "parser.rkt")
(require "desugar.rkt")
(require "interp.rkt")

(define (prueba exp)
  (interp (desugar (parse exp))
          (mt-env)))

(test/exn (prueba '{fun {x x} {add1 x}})
          "parse: Parámetro 'x definido dos veces.")

(test (parse '{{fun {x y} {* x y}} 10 8})
      (appS (funS '(x y) (opS * (list (idS 'x) (idS 'y))))
            (list (numS 10) (numS 8))))

(test (parse '{with {{x 10} {y 20}}
                    {with* {{z {+ x y}}
                            {w {sub1 z}}}
                           {+ x y z w}}})
      (withS
       (list (binding 'x (numS 10)) (binding 'y (numS 20)))
       (with*S
        (list
         (binding 'z (opS + (list (idS 'x) (idS 'y))))
         (binding 'w (opS sub1 (list (idS 'z)))))
        (opS + (list (idS 'x) (idS 'y) (idS 'z) (idS 'w))))))

(test (parse '{{fun {x y} {expt x {y 15}}}
               10 {fun {x} {modulo x 2}}})
      (appS
       (funS
        '(x y)
        (opS expt (list (idS 'x) (appS (idS 'y) (list (numS 15))))))
       (list
        (numS 10)
        (funS '(x) (opS modulo (list (idS 'x) (numS 2)))))))

(test (parse '{with* {{x 20}
                      {foo {fun {y} {y x}}}
                      {x 50}}
                     {foo {fun {x} x}}})
      (with*S (list (binding 'x (numS 20))
                    (binding 'foo (funS '(y)
                                        (appS (idS 'y)
                                              (list (idS 'x)))))
                    (binding 'x (numS 50)))
              (appS (idS 'foo)
                    (list (funS '(x) (idS 'x))))))

(test/exn (parse '{if {= x 20} x})
          "parse: Falta la else-expression.")

(test (parse '{if {= x 20} x y})
      (iFS (opS = (list (idS 'x) (numS 20)))
           (idS 'x)
           (idS 'y)))

(test/exn (parse '{cond
                {else x}})
      "parse: La expresión conds debe contar con 1 o mas condiciones y una expresión else.")

(test (parse '{cond
                {{zero? x} x}
                {else 30}})
      (conDS (list (condition (opS zero? (list (idS 'x)))
                              (idS 'x)))
             (numS 30)))

(test (desugar (parse '{with {{x 20} {y 2} {z 210}}
                             {+ x y z}}))
      (app (fun '(x y z) (op + (list (id 'x)
                                     (id 'y)
                                     (id 'z))))
           (list (num 20) (num 2) (num 210))))


(test (desugar (parse '{with {{x 30}}
                             {with {{y x}}
                                   {* x y}}}))
      (app (fun '(x)
                (app (fun '(y) (op * (list (id 'x) (id 'y))))
                     (list (id 'x))))
           (list (num 30))))

(test (desugar (parse '{with* {{y 20} {x {+ y y}}}
                              {- x y}}))
      (app (fun '(y)
                (app (fun '(x) (op - (list (id 'x)
                                           (id 'y))))
                     (list (op + (list (id 'y) (id 'y))))))
           (list (num 20))))

(test (desugar (parse '{cond {x y} {else 20}}))
      (iF (id 'x) (id 'y) (num 20)))

(test (desugar (parse '{cond
                         {x y}
                         {{zero? x} x}
                         {{= 0 y} w}
                         {else 20}}))
      (iF (id 'x)
          (id 'y)
          (iF (op zero? (list (id 'x)))
              (id 'x)
              (iF (op = (list (num 0) (id 'y)))
                  (id 'w)
                  (num 20)))))

(test (desugar (parse '{with {{x 10} {y 20}}
                             {cond
                               {{zero? {- x y}} 0}
                               {{= x y} x}
                               {else {with* {{foo {fun {x} {- x x}}}
                                             {a {foo y}}}
                                            {+ a 20}}}}}))
      (app (fun '(x y)
                (iF (op zero? (list (op - (list (id 'x) (id 'y)))))
                    (num 0)
                    (iF (op = (list (id 'x) (id 'y)))
                        (id 'x)
                        (app (fun '(foo)
                                  (app (fun '(a)
                                            (op + (list (id 'a)
                                                        (num 20))))
                                       (list (app (id 'foo)
                                                  (list (id 'y))))))
                             (list (fun '(x) (op - (list (id 'x)
                                                         (id 'x)))))))))
           (list (num 10) (num 20))))

(test (prueba '3) (num-v 3))

(test (prueba '#t) (bool-v #t))

(test (prueba '"Hi") (string-v "Hi"))

(test/exn (prueba 'y)  "interp: Variable libre y")

(test/exn (prueba '{{fun {x} x} 10 20}) "interp: Numero de argumentos y parametros distinto")

(test/exn (prueba '{if {fun {} 1} 10 30}) "interp: La condición de una expresión if debe ser un booleano.")

(test (prueba '{{fun {} 30}}) (num-v 30))

(test (prueba '{cond
                 {{= 0 1} 10}
                 {{zero? 0} 20}
                 {{> 10 2} 30}
                 {else 40}})
      (num-v 20))

(test (prueba '{cond
                 {{= 0 1} 10}
                 {{zero? 1} 20}
                 {{> 10 2} 30}
                 {else 40}})
      (num-v 30))

(test (prueba '{cond
                 {{= 0 1} 10}
                 {{zero? 1} 20}
                 {{< 10 2} 30}
                 {else 40}})
      (num-v 40))

(test (prueba '{with {{x 10} {y 20}}
                     {+ x y}})
      (num-v 30))

(test/exn (prueba '{with {{x 10} {y x}}
                     {+ x y}})
          "interp: Variable libre x")

(test/exn (prueba '{with* {{x y} {y 1}}
                          x})
          "interp: Variable libre y")

(test (prueba '{with* {{x 10}
                       {foo {fun {y} {+ y x}}}
                       {x 20}}
                      {foo 1}})
      (num-v 11))

(test (prueba '{with* {{x "hello"}
                       {foo {fun {} {cond
                                      {{zero? {str-length x}} "Si"}
                                      {else "No"}}}}}
                      {foo}})
      (string-v "No"))

(test/exn (prueba '{with* {{x 20}
                           {foo {fun {} {+ x y}}}
                           {y 10}}
                          {foo}})
          "interp: Variable libre y")

(test (prueba '{with {{x 10}}
                     {with {{foo {fun {x} {+ x x}}}}
                           {with {{x 30}}
                                 {foo 2}}}})
      (num-v 4))