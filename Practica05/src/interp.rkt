#lang plai

(require "grammars.rkt")
(require "parser.rkt")


;; Verifica si un valor esta contenido en una caja y es del tipo RCFSBAE-Val.
(define (boxed-RCFSBAE-Val? b)
  (and (box? b) (RCFSBAE-Val? (unbox b))))

;; Define los valores posibles en el lenguaje RCFSBAE, incluyendo numeros, booleanos, strings y cierres.
(define-type RCFSBAE-Val
  [num-v (n number?)]
  [bool-v (b boolean?)]
  [string-v (s string?)]
  [closure-v (args (listof symbol?)) (body RCFSBAE?) (env Env?)])

;; Define el entorno de evaluacion con ambientes vacios, cons y recursivos.
(define-type Env
  [mt-env]
  [cons-env (id symbol?) (value RCFSBAE-Val?) (rest-env Env?)]
  [rec-cons-env (id symbol?) (value boxed-RCFSBAE-Val?) (rest-env Env?)])

;; RCFSBAE x Env -> RCFSBAE-Val
;; Interpreta una expresion RCFSBAE en un entorno dado.
(define (interp expr env)
  (match expr
    [(id i) (lookup i env)]
    [(num n) (num-v n)]
    [(bool b) (bool-v b)]
    [(strinG s) (string-v s)]
    [(op f args) (let* ([lista
                         (map (lambda (v) (let ([eval (interp v env)])
                                       (cond
                                           [(num-v? eval) (num-v-n eval)]
                                           [(bool-v? eval) (bool-v-b eval)]
                                           [(string-v? eval) (string-v-s eval)])))
                              args)]
                        [result (cond
                               [(list-numbers? lista)
                                (apply f lista)]
                               [(list-booleans? lista)
                                (apply f lista)]
                               [(list-strings? lista)
                                (apply f lista)]
                               [else (error 'interp
                                     "Los argumentos no son de un mismo tipo de dato")])])
                   (cond
                     [(number? result) (num-v result)]
                     [(boolean? result) (bool-v result)]
                     [(string? result) (string-v result)]))]
    [(iF test-expr then-expr else-expr) (if (bool-v-b (interp expr env))
                                            (interp then-expr env)
                                            (interp else-expr env))]
    [(fun params body) (closure-v params body env)]
    [(rec bindings body) (interp body (rec-env bindings env))]
    [(app f args)
     (let ([valor-func (interp f env)])
       (type-case RCFSBAE-Val valor-func
         [closure-v (formales cuerpo env-cierre)
                    (if (= (length formales) (length args))
                        ;; Evaluar el cuerpo de la funcion en el ambiente del cierre
                        ;; despues de extenderlo con los valores de los argumentos
                        (let ([nuevo-env (foldl (lambda (id-arg valor-arg env-acum)
                                                  (cons-env (id-arg) (interp valor-arg env) (env-acum)))
                                                (env-cierre)
                                                (formales)
                                                (args))])
                          (interp cuerpo nuevo-env))
                        (error 'interp "El numero de parametros formales no coincide con el numero de parametros actuales"))]
         [else (error 'interp "Intentando aplicar un valor que no es funcion")]))]
    ))

;; Verifica si una lista esta compuesta exclusivamente de numeros.
(define (list-numbers? xs)
  (cond
    [(empty? xs) #t]
    [(number? (car xs)) (list-numbers? (cdr xs))]
    [else #f]))

;; Verifica si una lista esta compuesta exclusivamente de booleanos.
(define (list-booleans? xs)
  (cond
    [(empty? xs) #t]
    [(boolean? (car xs)) (list-booleans? (cdr xs))]
    [else #f]))


;; Verifica si una lista esta compuesta exclusivamente de strings.
(define (list-strings? xs)
  (cond
    [(empty? xs) #t]
    [(string? (car xs)) (list-strings? (cdr xs))]
    [else #f]))


;; symbol x Env -> RCFSBAE-Val
;; Busca un simbolo en un entorno y devuelve su valor correspondiente.
(define (lookup sub-id env)
  (match env
    [(mt-env) (error 'interp (format "Variable libre ~a" sub-id))]
    [(cons-env id value rest-env)
     (if (symbol=? id sub-id)
         value
         (lookup sub-id rest-env))]
    [(rec-cons-env id value rest-env)
     (if (symbol=? id sub-id)
         (unbox value)
         (lookup sub-id rest-env))]))


;; Construye un entorno recursivo a partir de una lista de bindings.
(define (rec-env bindings env)
  (if (empty? bindings)
      env
      (let* ([caja-rec (box (num-v 1))]
             [nuevo-env (rec-cons-env (binding-id (car bindings)) caja-rec env)]
             [val-nuevo-env (interp (binding-value (car bindings)) nuevo-env)])
        (begin
          (set-box! caja-rec val-nuevo-env)
          (rec-env (cdr bindings) nuevo-env)))))
