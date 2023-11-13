#lang plai

(require "grammars.rkt")

;; s-expr -> RCFSBAE
(define (parse s-exp)
  (cond
    [(symbol? s-exp) (id s-exp)]
    [(number? s-exp) (num s-exp)]
    [(boolean? s-exp) (bool s-exp)]
    [(string? s-exp) (strinG s-exp)]
    [(empty? s-exp) (error 'parse "No es una expresión válida")]
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

(define (aridad-dos op)
  (case op
    [(modulo) modulo]
    [(expt) expt]))

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

(define (parse-binding list-of-bindings)
      (map (lambda (b) (binding (car b) (parse (cadr b))))
           list-of-bindings))

(define (aux-or ls)
  (cond
    [(null? ls) #f]
    [else (if (equal? (car ls) #t)
              #t
              (aux-or (cdr ls)))]))

(define (or . xs)
  (cond
    [(null? xs) #t]
    [else (aux-or (cons (car xs) (cdr xs)))]))

(define (aux-and ls)
  (cond
    [(null? ls) #t]
    [else (if (equal? (car ls) #f)
              #f
              (aux-and (cdr ls)))]))

(define (and . xs)
  (cond
    [(null? xs) #t]
    [else (if (equal? (aux-and (cons (car xs) (cdr xs))) #t)
              (equal? (car xs) #t)
              #f)]))

(define (str? s)
  (string? s))

(define (str-length s)
  (string-length s))

(define (is-num? n)
  (number? n))

(define (is-bool? b)
  (boolean? b))