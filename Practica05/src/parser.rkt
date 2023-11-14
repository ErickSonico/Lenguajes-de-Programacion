#lang plai

(require "grammars.rkt")

;; s-expr -> RCFSBAE
;; Convierte una expresion s-expr a su correspondiente forma en RCFSBAE.
(define (parse s-exp)
  (cond
    [(symbol? s-exp) (id s-exp)]
    [(number? s-exp) (num s-exp)]
    [(boolean? s-exp) (bool s-exp)]
    [(string? s-exp) (strinG s-exp)]
    [(empty? s-exp) (error 'parse "No es una expresión valida")]
    [(list? s-exp)
     (case (car s-exp)
       [(#t) (bool #t)]
       [(#f) (bool #f)]
       [(+ - * / min max < <= = > >= and or)
        (if (not (> (length s-exp) 1))
            (error 'parse (format "La operación ~a debe ser ejecutada con mas de 0 argumentos." (car s-exp)))
            (op (aridad-muchos (car s-exp)) (map parse (cdr s-exp))))]
       [(modulo expt)
        (if (not (equal? (length s-exp) 3))
            (error 'parse (format "La operación ~a debe ser ejecutada con 2 argumentos." (car s-exp)))
            (op (aridad-dos (car s-exp))
                 (list (parse (second s-exp))
                       (parse (third s-exp)))))]
       [(sqrt add1 sub1 not zero? num? str? bool? str-length)
        (if (not (equal? (length s-exp) 2))
            (error 'parse (format "La operación ~a debe ser ejecutada con 1 argumentos." (car s-exp)))
            (op (aridad-uno (car s-exp))
                 (list (parse (second s-exp)))))]
       [(if)
        (iF (parse (cadr s-exp))
             (parse (caddr s-exp))
             (parse (cadddr s-exp)))]
       [(fun)
        (fun (second s-exp) (parse (third s-exp)))]
       [(rec)
        (rec (parse-binding (second s-exp)) (parse (third s-exp)))]
       [else
        (app (parse (first s-exp))
              (map (lambda (arg) (parse arg)) (cdr s-exp)))])]))

;; Define la operacion dependiendo del numero de argumentos (muchos).
(define (aridad-muchos op)
  (case op
    [(+) +]
    [(-) -]
    [(/) /]
    [(*) *]
    [(min) min]
    [(max) max]
    [(<) <]
    [(<=) <=]
    [(=) =]
    [(>) >]
    [(>=) >=]
    [(or) or]
    [(and) and]))

;; Define la operacion para dos argumentos.
(define (aridad-dos op)
  (case op
    [(modulo) modulo]
    [(expt) expt]))

;; Define la operacion para un solo argumento.
(define (aridad-uno op)
  (case op
    [(sqrt) sqrt]
    [(add1) add1]
    [(sub1) sub1]
    [(not) not]
    [(zero?) zero?]
    [(num?) is-num?]
    [(str?) str?]
    [(bool?) is-bool?]
    [(str-length) str-length]))

;; Parsea una lista de bindings para la construccion 'rec'.
(define (parse-binding list-of-bindings)
      (map (lambda (b) (binding (car b) (parse (cadr b))))
           list-of-bindings))

;; Implementa la operacion logica 'or' con una lista de expresiones.
(define (aux-or ls)
  (cond
    [(null? ls) #f]
    [else (if (equal? (car ls) #t)
              #t
              (aux-or (cdr ls)))]))

;; Define la operacion 'or' para multiples argumentos.
(define (or . xs)
  (cond
    [(null? xs) #t]
    [else (aux-or (cons (car xs) (cdr xs)))]))

;; Implementa la operacion logica 'and' con una lista de expresiones.
(define (aux-and ls)
  (cond
    [(null? ls) #t]
    [else (if (equal? (car ls) #f)
              #f
              (aux-and (cdr ls)))]))

;; Define la operacion 'and' para multiples argumentos.
(define (and . xs)
  (cond
    [(null? xs) #t]
    [else (if (equal? (aux-and (cons (car xs) (cdr xs))) #t)
              (equal? (car xs) #t)
              #f)]))

;; Verifica si un valor es una cadena de texto.
(define (str? s)
  (string? s))


;; Devuelve la longitud de una cadena de texto.
(define (str-length s)
  (string-length s))

;; Verifica si un valor es un numero.
(define (is-num? n)
  (number? n))

;; Verifica si un valor es un booleano
(define (is-bool? b)
  (boolean? b))