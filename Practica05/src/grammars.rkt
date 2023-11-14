#lang plai

;; 'Binding' asocia un identificador con un valor.
(define-type Binding
  [binding (id symbol?) (value RCFSBAE?)]) 

;; 'RCFSBAE' representa diferentes tipos de expresiones y valores en un lenguaje de programacion.
(define-type RCFSBAE
  [id (i symbol?)]                           
  [bool (b boolean?)]                         
  [strinG (s string?)]
  [num (n number?)]
  [op (f procedure?) (args (listof RCFSBAE?))] ;; Representa una operacion con funcion y argumentos.
  [fun (params (listof symbol?)) (body RCFSBAE?)] ;; Representa una funcion.
  [app (f RCFSBAE?) (args (listof RCFSBAE?))] ;; Aplica una funcion a argumentos.
  [iF (test-expr RCFSBAE?) (then-expr RCFSBAE?) (else-expr RCFSBAE?)] ;; Representa una expresion condicional.
  [rec (bindings (listof Binding?)) (body RCFSBAE?)]) ;; Representa una estructura recursiva.
