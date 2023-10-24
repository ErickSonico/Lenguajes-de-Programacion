#lang plai

(require "grammars.rkt")
(require "parser.rkt")
(require "desugar.rkt")

(define-type CFSBAE-Val
  [num-v (n number?)]
  [bool-v (b boolean?)]
  [string-v (s string?)]
  [closure-v (args (listof symbol?)) (body CFSBAE?) (env Env?)])

(define-type Env
  [mt-env]
  [cons-env (id symbol?) (value CFSBAE-Val?) (rest-env Env?)])

;; interp :: CFSBAE x Env -> CFSBAE-Val
(define (interp expr env))

;; lookup :: symbol x Env -> CFSBAE-Val
(define (lookup sub-id env)
  (match env
    [(mt-env) (error 'lookup "Identificador libre")]
    [(cons-env id val rest)
              (if (equal? id sub-id)
                  val
                  (lookup sub-id rest))]))
