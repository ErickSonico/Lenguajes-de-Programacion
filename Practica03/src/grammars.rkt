#lang plai

(define-type Binding
  [binding (id symbol?) (value WAE?)])

(define-type WAE
  [id (i symbol?)]
  [num (n number?)]
  [with (assigns (listof Binding?)) (body WAE?)]
  [with* (assigns (listof Binding?)) (body WAE?)])