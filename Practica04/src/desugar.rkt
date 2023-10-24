#lang plai

(require "grammars.rkt")
(require "parser.rkt")

;; desugar :: CFWSBAE -> CFSBAE
(define (desugar expr)
  (type-case CFWSBAE expr
    [numS (n) (num n)]
    [idS (i) (id i)]
    [boolS (b) (bool b)]
    [strinGS (str) (strinG str)]
    [opS (f args) (op f (map (lambda (a) (desugar a)) args))]
    [funS (param bo) (fun param (desugar bo))]
    [appS (f args) (app (desugar f) (map (lambda (a) (desugar a)) args))]
    [with*S (bs bo) (desugar (with*-a-with bs bo))]
    [withS (bs bo)
          (let ([pairBS (bs-a-par bs (cons '() '()))])
            (app (fun (reverse (car pairBS)) (desugar bo)) (reverse (cdr pairBS))))]
    [iFS (test then else) (iF (desugar test) (desugar then) (desugar else))]
    [conDS (cnd else) (desugar (cond-a-if cnd else))]))

(define (with*-a-with bs bo)
  (cond
    [(empty? bs) bo]
    [else (withS (list (car bs)) (with*-a-with (cdr bs) bo))]))

(define (bs-a-par bs acc)
  (if (empty? bs)
      acc
      (type-case Binding (car bs)
        [binding (id value) (bs-a-par (cdr bs) (cons (cons id (car acc))
                                  (cons (desugar value) (cdr acc))))])))

(define (cond-a-if cnd else)
  (if (empty? cnd)
      else
      (type-case Condition (car cnd)
        [condition (test then) (iFS test then (cond-a-if (cdr cnd) else))])))