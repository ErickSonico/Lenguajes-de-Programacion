#lang plai

(require "grammars.rkt")
; verificar la aridad 1 de sqrt
;

; Función para analizar sintácticamente una expresión s-exp
(define (parse s-exp)
  ; Verificar si la expresión es una lista
  (cond
    [(symbol? s-exp) (id s-exp)]
    [(number? s-exp) (num s-exp)]
    [(boolean? s-exp) (bool s-exp)]
    [(string? s-exp) (strinG s-exp)]
    [(list? s-exp)
     ; Si es una lista, extraer la operación y los argumentos
     (let ([proc (first s-exp)]
           [args (rest s-exp)])
       (cond
         ; Manejar el caso especial de la operación 'with'
         [(eq? proc 'with)
          (let ([bindings (first args)]
                [body (second args)])
            ; Verificar si hay identificadores duplicados en los bindings
            (if (has-duplicate-bindings? bindings)
                (error "parse: El identificador está declarado más de una vez")
                (with (map parse-binding bindings) (parse body))))
          ]
         ; Verificar las funciones con aridad 1
         [(or (eq? proc 'sub1) (eq? proc 'add1) (eq? proc 'not) (eq? proc 'zero?) (eq? proc 'num?) 
              (eq? proc 'string?) (eq? proc 'bool?) (eq? proc 'string-length))
          (if (= (length args) 1)
              (op (eval proc) (map parse args))
              (error "parse: La operación ~a espera 1 argumento. Número de argumentos dados: ~a." proc (length args)))
          ]
         ; Verificar las funciones con aridad 2
         [(or (eq? proc 'modulo) (eq? proc 'expt))
          (if (= (length args) 2)
              (op (eval proc) (map parse args))
              (error "parse: La operación ~a espera 2 argumentos. Número de argumentos dados: ~a." proc (length args)))
          ]
         ; Para otras funciones, simplemente verificar que no estén vacías
         [else
          (if (> (length args) 0)
              (op (eval proc) (map parse args))
              (error "parse: La operación ~a espera al menos 1 argumento." proc))
          ]))]
    ; Si s-exp no es una lista, simplemente devolverla
    [else
     s-exp]))

; Función para verificar si hay identificadores duplicados en los bindings
(define (has-duplicate-bindings? bindings)
  (let ([ids (map first bindings)])
    (not (= (length ids) (length (remove-duplicates ids))))))

; Función para analizar un binding
(define (parse-binding binding)
  ; Verificar la estructura del binding
  (if (= (length binding) 2)
      (binding (first binding) (parse (second binding)))
      (error "parse: Estructura de binding incorrecta: ~a" binding)))
