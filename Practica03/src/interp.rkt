#lang plai

(require "grammars.rkt")
(require "parser.rkt")

;; Función interp del lenguaje para el análisis semántico.
(define (interp expr)
  (match expr
    [(id i) (error 'interp "Variable libre" id)]
    ; Si recibe id, num, bool o strinG simplemente las regresa
    [(num expr) expr]
    [(bool expr) expr]
    [(strinG expr) expr]
    ; si la WAE es una op, aplica la expr a cada elemento de args interpretado
    [(op expr args) (apply expr (map(lambda(expr)(interp expr)) args))]
    [(with assigns body) 
            (interp (foldl
                (lambda (bdg expr-res)
                    (subst (binding-id bdg) (binding-value bdg) expr-res)
                )
                (with-body expr)
                (with-assigns expr))
            )
        ]
    [(with* assigns body) #t]
    ; Si la expresión no es ninguna de las anteriores, interp no puede trabajar con ella
    ; por lo que regresa un error
    [else (error 'interp "La expresión no es válida")]
    ))

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
