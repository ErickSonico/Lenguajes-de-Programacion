#lang plai

(require "grammars.rkt")
(require "parser.rkt")

;; Función interp del lenguaje para el análisis semántico.
;; Recibe un dato del tipo WAE definido en grammars.rkt
;;
;; interp :: WAE -> WAE
(define (interp expr)
  (type-case WAE expr
    [id (i) (error 'interp (format "Variable libre: ~a" (eval expr)))]
    ; Si recibe id, num, bool o strinG simplemente las regresa
    [num (expr) expr]
    [bool (expr) expr]
    [strinG (expr) expr]
    ; si la WAE es una op, aplica la expr a cada elemento de args interpretado
    [op (expr args) (apply expr (map(lambda(expr)(interp expr)) args))]
    [with (assigns body) 
            (interp (foldl
                (lambda (bdg expr-res)
                    (subst (binding-id bdg) (binding-value bdg) expr-res)
                )
                (with-body expr)
                (with-assigns expr))
            )
        ]
    [with* (assigns body) (interp (with*-a-with (with* assigns body)))]
    ))

; Función para convertir un with* a with:
;;
;; with*-a-with :: with* -> with
(define (with*-a-with w)
  (cond
    [(empty? (with*-assigns w)) (with*-body w)]
    [else (with (list (car (with*-assigns w))))
          (with*-a-with (with* (cdr (with*-assigns w)) (with*-body w)))]))

; Función que sustituye una lista de bindings por valores
(define (subst-list-bindings assigns body)
  (if (empty? assigns)
      assigns
      (let ([head (car assigns)])
        (subst-list-bindings (cdr assigns)
                             (subst (binding-id head)
                                    (binding-value head)
                                    body)))))

; Función para sustituir un identificador por un valor en una expresión
(define (subst sub-id value expr)
  ; Verificar si la expresión es una lista
  (cond
    [(list? expr)
     ; Si es una lista, manejar de forma recursiva cada elemento
     (map (lambda (e) (subst sub-id value e)) expr)]
    ; Si la expresión coincide con el identificador, sustituir
    [(eq? expr sub-id)
     value]
    ; De lo contrario, devolver la expresión tal cual
    [else
     expr]))
