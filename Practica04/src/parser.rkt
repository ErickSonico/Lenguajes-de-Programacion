#lang plai

(require "grammars.rkt")

;; Función auxiliar para verificar si hay identificadores duplicados
(define (has-duplicates? lst)
  (if (null? lst)
      #f
      (or (member (car lst) (cdr lst))
          (has-duplicates? (cdr lst)))))

;; parse :: s-exp -> CFWSBAE
(define (parse s-exp)
  (cond
    [(list? s-exp)
     (let ([first-elem (car s-exp)])
       (cond
         [(eq? first-elem 'with)
          (let ([bindings (cadr s-exp)]
                [body (caddr s-exp)])
            (if (has-duplicates? (map car bindings))
                (error "Duplicate parameter names are not allowed")
                (withS (map parse-binding bindings) (parse body))))]
         [(eq? first-elem 'fun)
          (let ([params (cadr s-exp)]
                [body (caddr s-exp)])
            (if (has-duplicates? params)
                (error "Duplicate parameter names are not allowed in 'fun'")
                (funS params (parse body))))]
         [(eq? first-elem 'app)
          (let ([f (cadr s-exp)]
                [args (cddr s-exp)])
            (appS (parse f) (map parse args)))]
         [(eq? first-elem 'if)
          (let ([test-expr (cadr s-exp)]
                [then-expr (caddr s-exp)]
                [else-expr (cadddr s-exp)])
            (iFS (parse test-expr) (parse then-expr) (parse else-expr)))]
         ;; mas condiciones 
         
         [else (error "Unknown expression: ~a" s-exp)]))]
    [(number? s-exp) (numS s-exp)]
    [(symbol? s-exp) (idS s-exp)]
    [(boolean? s-exp) (boolS s-exp)]
    [(string? s-exp) (strinGS s-exp)]
    [else (error "Invalid expression: ~a" s-exp)]))

;; parse-binding :: s-exp -> Binding
(define (parse-binding binding-exp)
  (if (and (list? binding-exp) (= (length binding-exp) 2))
      (let ([id (car binding-exp)]
            [value (cadr binding-exp)])
        (if (symbol? id)
            (binding id (parse value))
            (error "Invalid binding identifier: ~a" id)))
      (error "Invalid binding expression: ~a" binding-exp)))
