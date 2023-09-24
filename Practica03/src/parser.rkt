#lang plai

(require "grammars.rkt")

(define (parse s-exp))

; Función para analizar sintácticamente una expresión s-exp
(define (parse s-exp)
  ; Verificar si la expresión es una lista
  (cond
    [(list? s-exp)
     ; Si es una lista, extraer la operación y los argumentos
     (let ([op (first s-exp)]
           [args (rest s-exp)])
       (cond
         ; Manejar el caso especial de la operación 'with'
         [(eq? op 'with)
          (let ([bindings (first args)]
                [body (second args)])
            ; Verificar si hay identificadores duplicados en los bindings
            (if (has-duplicate-bindings? bindings)
                (error "parse: El identificador está declarado más de una vez")
                (with (map parse-binding bindings) (parse body))))
          ]
         ; Verificar las funciones con aridad 1
         [(or (eq? op 'sub1) (eq? op 'add1) (eq? op 'not) (eq? op 'zero?) (eq? op 'num?) 
              (eq? op 'str?) (eq? op 'bool?) (eq? op 'str-length))
          (if (= (length args) 1)
              (op (procedure op) (map parse args))
              (error "parse: La operación ~a espera 1 argumento. Número de argumentos dados: ~a." op (length args)))
          ]
         ; Verificar las funciones con aridad 2
         [(or (eq? op 'modulo) (eq? op 'expt))
          (if (= (length args) 2)
              (op (procedure op) (map parse args))
              (error "parse: La operación ~a espera 2 argumentos. Número de argumentos dados: ~a." op (length args)))
          ]
         ; Para otras funciones, simplemente verificar que no estén vacías
         [else
          (if (> (length args) 0)
              (op (procedure op) (map parse args))
              (error "parse: La operación ~a espera al menos 1 argumento." op))
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
