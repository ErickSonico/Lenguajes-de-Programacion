#lang plai

(define (any? x)
  #t)

(define-type Punto
  [punto (x number?) (y number?)])

;; Ejercicio 1.a) La función recibe dos argumentos del tipo punto.
;;                utiliza las funciones que regala el lenguaje para
;;                acceder a los parámetros del tipo punto.
;;                Con esa información aplica la fórmula del punto medio.
;;
;; punto-medio :: Punto Punto -> Punto
(define (punto-medio p q)
  (if (or (not (punto? p)) (not (punto? q)))
      (error 'Error "Todos los argumentos deben ser puntos")
      (punto
       (/ (+ (punto-x p) (punto-x q)) 2)
       (/ (+ (punto-y p) (punto-y q)) 2))))


;; Ejercicio 1.b) La función primero verifica que ambos parámetros
;;                sean del tipo punto usando el predicado que regala el lenguaje
;;                y un not. Si los dos son puntos, aplica la fórmula para
;;                calcular la distancia entre ellos, si no, lanza un error.
;;               
;; distancia :: Punto Punto -> number     
(define (distancia p q)
  (if(or (not (punto? p)) (not (punto? q)))
          (error 'Error "Todos los argumentos deben ser puntos")
          (sqrt(+ (expt(- (punto-x q)(punto-x p)) 2) (expt(- (punto-y q)(punto-y p)) 2)))))

(define-type Lista
    [Vacia]
    [Cons (cabeza any?) (resto Lista?)])

;; Ejercicio 2.a) Recibe un parámetro de tipo Lista.
;;                La función utiliza un type-case para
;;                sumar 1 cuando la lista no es vacia
;;                y 0 cuando sí lo es.
;;
;; longitud :: Lista -> number
(define (longitud ls)
  (type-case Lista ls
    [Vacia () 0]
    [Cons (cabeza resto) (+ 1 (longitud resto))]))


;; Ejercicio 2.b)
(define (pertenece? e ls)
  (type-case Lista ls
    [Vacia () #false]
    [Cons (cabeza resto) (if (equal? e cabeza)
                             #true
                             (pertenece? e resto))])) 

;; Ejercicio 2.c)
(define (intercala ls ks)
  (type-case Lista ls
    [Vacia () ks]
    [Cons (cabeza resto) (Cons (cabeza (intercala resto ks)))]))

;; Ejercicio 2.d)
(define (aplana ls)
  (error 'aplana "Sin implementar"))

(define-type ArbolBinarioDeBusqueda
    [ArbolVacio]
    [ABB (elemento number?)
         (izq ArbolBinarioDeBusqueda?)
         (der ArbolBinarioDeBusqueda?)])

;; Ejercicio 3.a)
(define (elimina e a)
  (error 'elimina "Sin implementar"))

;; Ejercicio 3.b)
(define (mapea-arbol ab f)
  (error 'mapea-arbol "Sin implementar"))

;; Ejercicio 3.c)
(define (hojas ar)
  (error 'hojas "Sin implementar"))

;; Punto Extra
(define (mas-repetido ls)
  (error 'mas-repetido "Sin implementar"))
