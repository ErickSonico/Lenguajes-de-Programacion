#lang plai

(define-type Binding
  [binding (id symbol?) (value CFWSBAE?)])

(define-type Condition
  [condition (test-expr CFWSBAE?) (then-expr CFWSBAE?)])

;; Lenguaje con azucar sintactica
(define-type CFWSBAE
  [numS (n number?)]
  [idS (i symbol?)]
  [boolS (b boolean?)]
  [strinGS (s string?)]
  [opS (f procedure?) (args (listof CFWSBAE?))]
  [withS (bindings (listof Binding?)) (body CFWSBAE?)]
  [with*S (bindings (listof Binding?)) (body CFWSBAE?)]
  [funS (params (listof symbol?)) (body CFWSBAE?)]
  [appS (f CFWSBAE?) (args (listof CFWSBAE?))]
  [iFS (test-expr CFWSBAE?) (then-expr CFWSBAE?) (else-expr CFWSBAE?)]
  [conDS (conds (listof Condition?)) (else-expr CFWSBAE?)])

(define-type CFSBAE
  [num (n number?)]
  [id (i symbol?)]
  [bool (b boolean?)]
  [strinG (s string?)]
  [op (f procedure?) (args (listof CFSBAE?))]
  [fun (params (listof symbol?)) (body CFSBAE?)]
  [app (f CFSBAE?) (args (listof CFSBAE?))]
  [iF (test-expr CFSBAE?) (then-expr CFSBAE?) (else-expr CFSBAE?)])