#lang plai

(require "grammars.rkt")
; verificar la aridad 1 de sqrt
;

; Función para analizar sintácticamente una expresión s-exp
(define (parse s-exp)
  ; Verificar si la expresión es una lista
  (cond
    
    [(number? s-exp) (num s-exp)]
    [(boolean? s-exp) (bool s-exp)]
    [(string? s-exp) (strinG s-exp)]
    [(symbol? s-exp) (id s-exp)]
    [(list? s-exp)
     ; Si es una lista, extraer la operación y los argumentos
     (let ([proc (first s-exp)]
           [args (rest s-exp)])
       (cond
         ; Manejar el caso especial de la operación 'with'
         [(eq? proc 'with)
          (let* ([bindings (first args)]
                [body (second args)])
            ; Verificar si hay identificadores duplicados en los bindings
            (if (has-duplicate-bindings? bindings)
                (error 'parse "El identificador está declarado más de una vez")
                (with (map parse-binding bindings) (parse body))))
          ]

         ; Manejar el caso especial de la operación 'with'
         [(eq? proc 'with*)
          (let* ([bind (first args)]
                [bod (map list-to-binding bind)])
            ; Verificar si hay identificadores duplicados en los bindings
            (with* bod (parse (second args))))
          ]
         
         ; Verificar las funciones con aridad 1
         [(or (eq? proc 'sub1) (eq? proc 'add1) (eq? proc 'not) (eq? proc 'zero?) (eq? proc 'num?) 
              (eq? proc 'str?) (eq? proc 'bool?) (eq? proc 'str-length) (eq? proc 'strinG))
          (if (= (length args) 1)
              (op (eval proc) (map parse args))
              (error 'parse (format "La operación ~a debe ser ejecutada con 1 argumentos." proc)))
          ]
         ; Verificar las funciones con aridad 2
         [(or (eq? proc 'modulo) (eq? proc 'expt))
          (if (= (length args) 2)
              (op (eval proc) (map parse args))
              (error 'parse (format "La operación ~a espera 1 argumento. Número de argumentos dados: ~a." proc (length args))))
          ]
         ; Para otras funciones, simplemente verificar que no estén vacías
         [else
          (if (procedure? (eval proc))
              (if (> (length s-exp) 1)
                  (op (eval proc) (map parse args))
                  (error (format "parse: La operación ~a debe ser ejecutada con mas de 0 argumentos." proc)))
              (parse proc))
          ]))]
    ; Si s-exp no es una lista, simplemente devolverla
    [else
     s-exp]))

; Función para verificar si hay identificadores duplicados en los bindings
(define (has-duplicate-bindings? bindings)
  (let ([ids (map first bindings)])
    (not (= (length ids) (length (remove-duplicates ids))))))

; Función para analizar un binding
(define (parse-binding bindings)
  ; Verificar la estructura del binding
  (if (= (length bindings) 2)
      (binding (car bindings) (parse (cdr bindings)))
      (error 'parse (format "Estructura de binding incorrecta: ~a" bindings))))

; Función auxiliar que pasa una lista a binding
(define (list-to-binding ls)
  (cond
    [(empty? ls) (error 'list-to-bindings "La lista es vacía")]
    [(not (= (length ls) 2)) (error 'list-to-bindings "No son 2 elementos")]
    [else (binding (car ls) (parse (second ls)))]
    ))