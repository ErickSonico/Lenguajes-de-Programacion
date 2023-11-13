#lang plai

(define-type Binding
  [binding (id symbol?) (value RCFSBAE?)])

(define-type RCFSBAE
  [num (n number?)]
  [id (i symbol?)]
  [bool (b boolean?)]
  [strinG (s string?)]
  [op (f procedure?) (args (listof RCFSBAE?))]
  [fun (params (listof symbol?)) (body RCFSBAE?)]
  [app (f RCFSBAE?) (args (listof RCFSBAE?))]
  [iF (test-expr RCFSBAE?) (then-expr RCFSBAE?) (else-expr RCFSBAE?)]
  [rec (bindings (listof Binding?)) (body RCFSBAE?)])
