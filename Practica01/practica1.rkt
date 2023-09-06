#lang plai

#|
Integrantes:
    Estrada García Luis Gerardo - 319013832
    Evangelista Tiburcio Jose Manuel - 422007939
    García Velasco Erick Iram - 318044309
|#


;; Ejercicio 1: la función toma el parámetro diámetro y lo divide entre 2
;;              para obtener el radio, posteriormente lo multiplica por la generatriz
;;              y por pi, luego lo suma con pi por radio al cuadrado.
;; area-total :: number number -> number
(define (area-total generatriz diametro)
  (define r (/ diametro 2))
  (+ (* pi r generatriz) (* pi r r)))


;; Ejercicio 2: Predicado que indica si, dados cuatro números, 
;;              están o no en orden decreciente o de forma
;;              decremental.
;; decremental :: number number number number -> boolean
(define (decremental? a b c d)
  (and (> a b) (> b c) (> c d)))


;; Ejercicio 3: esta función recursiva tiene como caso base
;;              la lista vacía, que regresa 1. Si la lista no es vacía,
;;              multpilica la cabeza por el resultado de llamar recursivamente
;;              a esta función con la cola de la lista.
;; multiplica :: (listof number) -> number
(define (multiplica lst)
  (if (null? lst)
      1
      (* (car lst) (multiplica (cdr lst)))))


;; Ejercicio 4: Calcula el área de un triángulo a partir de la 
;;              medida de cada uno de sus lados, utilizando la 
;;              fórmula de Herón.
;; area-heron :: number number number -> number
(define (area-heron a b c)
  (define S (/ (+ a b c) 2))
  (sqrt (* S (- S a) (- S b) (- S c))))


;; Ejercicio 5: el predicado primero revisa si la lista es vacía,
;;              si lo es, regresa #f. Si no es vacía, toma la cabeza
;;              de la lista y verifica si el elemento es par usando
;;              un predicado del lenguaje, luego se llama recursivamente con la cola de la lista.
;; pares? :: (listof number) -> boolean
(define (pares? lst)
  (if (null? lst)
      #t
      (if (even? (car lst))
          (pares? (cdr lst))
          #f)))


;; Ejercicio 6: Filtra el contenido de una lista de cualquier tipo
;;              dado un predicado del tipo de elemento que quieres
;;              filtrar de la lista dada.
;; filtra-lista :: (listof any) -> (listof any)
(define (filtra-lista p ls)
  (if (null? ls)
      '()
      (if (p (car ls))
          (cons (car ls) (filtra-lista p (cdr ls)))
          (filtra-lista p (cdr ls)))))
