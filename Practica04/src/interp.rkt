#lang plai

(require "grammars.rkt")
(require "parser.rkt")
(require "desugar.rkt")

(define-type CFSBAE-Val
  [num-v (n number?)]
  [bool-v (b boolean?)]
  [string-v (s string?)]
  [closure-v (args (listof symbol?)) (body CFSBAE?) (env Env?)])

(define-type Env
  [mt-env]
  [cons-env (id symbol?) (value CFSBAE-Val?) (rest-env Env?)])

;; interp :: CFSBAE x Env -> CFSBAE-Val
(define (interp expr env)
  (type-case CFSBAE expr
    ;; Añade casos
    [id (i) (lookup i env)]
    [num (n) (num-v n)]
    [bool (b) (bool-v b)]
    [strinG (s) (string-v s)]
    [fun (params body) (closure-v params body env)]
    [op (f args) (interp (desugar (parse
                                   (apply f (eval (map (lambda (e) (interp e env)) args))))))]
    [app (f args)
         (let ([valor-func (interp f env)])
           (type-case CFSBAE-Val valor-func
             [closure-v (formales cuerpo env-cierre)
                        (if (= (length formales) (length args))
                            ;; Evaluar el cuerpo de la función en el ambiente del cierre
                            ;; después de extenderlo con los valores de los argumentos
                            (let ([nuevo-env (foldl (lambda (id-arg valor-arg env-acum)
                                                      (cons-env id-arg (interp valor-arg env) env-acum))
                                                    env-cierre
                                                    formales
                                                    args)])
                              (interp cuerpo nuevo-env))
                            (error 'interp "El número de parámetros formales no coincide con el número de parámetros actuales"))]
             [else (error 'interp "Intentando aplicar un valor que no es función")]))]
    [iF (test-e then-e else-e)
        (type-case CFSBAE-Val (interp test-e env)
          [bool-v (b)
                  (if b
                      (interp then-e env)
                      (interp else-e env))]
          [else (error 'interp "La condición de una expresión if debe ser un booleano.")])]))

;; lookup :: symbol x Env -> CFSBAE-Val
(define (lookup sub-id env)
  (match env
    [(mt-env) (error 'lookup "Identificador libre")]
    [(cons-env id val rest)
              (if (equal? id sub-id)
                  val
                  (lookup sub-id rest))]))

;;eval :: CFSBAE-Val -> Val
(define (eval args)
  (if (empty? args)
      '()
      (type-case CFSBAE-Val (car args)
        [num-v (n) (cons n (eval (cdr args)))]
        [bool-v (b) (cons b (eval (cdr args)))]
        [string-v (s) (cons s (eval (cdr args)))]
        [closure-v (args body env) error 'interp "Expresión no válida"] )))
