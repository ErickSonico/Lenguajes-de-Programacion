#lang plai

; Tipo de dato abstracto Bindings para asignar valores a variables
(define-type Binding
  [binding (id symbol?) (value WAE?)])

; Tipo de dato abstracto WAE para definir nuestros propios tipos de datos que representan el ASA
(define-type WAE
  [id (i symbol?)]
  [num (n number?)]
  [bool (b boolean?)]
  [strinG (s string?)]
  [with (assigns (listof Binding?)) (body WAE?)]
  [with* (assigns (listof Binding?)) (body WAE?)]
  [op (f procedure?) (args (listof WAE?))])

; Función que crea el procedimiento str?
(define (str? s)
  string? s)

; Función que crea el procedimiento str-length
(define (str-length s)
  string-length s)

; Función auxiliar para hacer la función anD que verifica si todos los elementos de una lista son iguales
(define (aux-anD ls)
  (cond
    [(or (null? ls)
         (null? (cdr ls))) #t]
    [else (and (equal? (car ls) (cadr ls)) (aux-anD (cdr ls)))]))

; Función anD que hace lo mismo que el operador lógico and, pero, la podemos usar como procedimineto en WAE
(define (anD . xs)
  (cond
    [(null? xs) #t]
    [else (if (equal? (aux-anD (cons (car xs) (cdr xs))) #t)
              (equal? (car xs) #t)
              #f)]))

; Función auxiliar para hacer la función oR que verifica si un elemento de una lista es igual a true
(define (aux-oR ls)
  (cond
    [(null? ls) #f]
    [else (if (equal? (car ls) #t)
              #t
              (aux-oR (cdr ls)))]))

; Función oR que hace lo mismo que el operador lógico or, pero, la podemos usar como procedimineto en WAE
(define (oR . xs)
  (cond
    [(null? xs) #t]
    [else (aux-oR (cons (car xs) (cdr xs)))]))