#lang plai

(require "grammars.rkt")
(require "parser.rkt")

;(define (interp expr))

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
