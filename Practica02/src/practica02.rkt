#lang plai
#|
Integrantes:
    Estrada García Luis Gerardo - 319013832
    Evangelista Tiburcio Jose Manuel - 422007939
    García Velasco Erick Iram - 318044309
|#

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
;; Función que determina si un elemento e está presente en una lista ls.
(define (pertenece? e ls)
  (cond
    ;; Si la lista es vacía, devolvemos un false.
    [(Vacia? ls) #f]
    ;; Si el elemento e es igual a la cabeza de ls, entonces devolvemos un true.
    [(equal? e (Cons-cabeza ls)) #t]
    ;; Si el elemento e no es igual a la cabeza de ls, entonces hace una llamada recursiva al resto de ls para comparar a e con el resto de elementos de ls.
    [else (pertenece? e (Cons-resto ls))]))



;; Ejercicio 2.c)
;; Función que intercala los elementos de dos listas ls y ks.
(define (intercala ls ks)
  (cond
    ;; Si ls es vacía devuelve la otra lista ks.
    [(Vacia? ls) ks]
    ;; Si ks es vacía devuelve la otra lista ls.
    [(Vacia? ks) ls]
    ;; Si no son vacía ninguna de las listas, concatena la cabeza de ls con la de ks y hace la llamada recursiva al resto de las listas.
    [else (Cons (Cons-cabeza ls) 
                (Cons (Cons-cabeza ks) 
                      (intercala (Cons-resto ls) (Cons-resto ks))))]))



;; Ejercicio 2.d)
;; Función auxiliar que hace lo mismo que append, pero para la definición de Lista que tenemos.
(define (append-Lista ls ks)
  (if (Vacia? ls)
      ks
      (Cons (Cons-cabeza ls) (append-Lista (Cons-resto ls) ks))))

;; Función que aplana una lista de listas ls en una única lista.
(define (aplana ls)
  (cond
    ;; Si la lista es vacía, devuelve Vacia, que es parte de la definición de Lista.
    [(Vacia? ls) (Vacia)]
    ;; Si la cabeza de lista ls es otra Lista, entonces, hace append-Lista para la lista de la cabeza de ls y el resto de ls.
    [(Lista? (Cons-cabeza ls))
     (append-Lista (Cons-cabeza ls) (aplana (Cons-resto ls)))]
    ;; Si la cabeza de ls no es otra Lista, entonces, solo concatena la cabeza de ls con la llamada recursiva de aplana con el resto de ls.
    [else (Cons (Cons-cabeza ls) (aplana (Cons-resto ls)))]))

(define-type ArbolBinarioDeBusqueda
    [ArbolVacio]
    [ABB (elemento number?)
         (izq ArbolBinarioDeBusqueda?)
         (der ArbolBinarioDeBusqueda?)])



;; Ejercicio 3.a)
;; Función que elimina un elemento e de un árbol binario de búsqueda a.
(define (elimina e a)
  (cond
    ;; Si el árbol es vacío, simplemente devolvemos un árbol vacío.
    [(ArbolVacio? a) ArbolVacio]
    
    ;; Si encontramos el elemento a eliminar
    [(= e (ABB-elemento a))
     (cond
       ;; Si el nodo no tiene hijos, simplemente devolvemos un árbol vacío.
       [(and (ArbolVacio? (ABB-izq a)) (ArbolVacio? (ABB-der a))) ArbolVacio]
       
       ;; Si el nodo tiene sólo un hijo derecho
       [(ArbolVacio? (ABB-izq a)) (ABB-der a)]
       
       ;; Si el nodo tiene sólo un hijo izquierdo
       [(ArbolVacio? (ABB-der a)) (ABB-izq a)]
       
       ;; Si el nodo tiene dos hijos
       [else
        (let [(sucesor (encontrarMinimo (ABB-der a)))]
          (ABB sucesor 
               (ABB-izq a) 
               (elimina sucesor (ABB-der a))))])]
    
    ;; Si el elemento a eliminar es menor que el elemento actual, eliminamos en el subárbol izquierdo.
    [(< e (ABB-elemento a)) 
     (ABB (ABB-elemento a) (elimina e (ABB-izq a)) (ABB-der a))]
    
    ;; Si el elemento a eliminar es mayor que el elemento actual, eliminamos en el subárbol derecho.
    [else 
     (ABB (ABB-elemento a) (ABB-izq a) (elimina e (ABB-der a)))]
  )
)

;; Función auxiliar que encuentra el valor mínimo en un árbol binario de búsqueda.
(define (encontrarMinimo a)
  (if (ArbolVacio? (ABB-izq a))
      (ABB-elemento a)
      (encontrarMinimo (ABB-izq a))))



;; Ejercicio 3.b)
;; Función que mapea una función f sobre todos los elementos de un árbol binario de búsqueda ab.
(define (mapea-arbol ab f)
  (cond
    ;; Si el árbol es vacío, simplemente devolvemos un árbol vacío.
    [(ArbolVacio? ab) ArbolVacio]
    
    ;; Aplicamos la función f al elemento actual y llamamos recursivamente a mapea-arbol para los subárboles.
    [else (ABB (f (ABB-elemento ab)) 
               (mapea-arbol (ABB-izq ab) f) 
               (mapea-arbol (ABB-der ab) f))]))



;; Ejercicio 3.c)
;; Función que devuelve todas las hojas de un árbol binario de búsqueda ar.
(define (hojas ar)
  (cond
    ;; Si el árbol es vacío, devolvemos una lista vacía.
    [(ArbolVacio? ar) '()]
    
    ;; Si el árbol es una hoja (no tiene hijos), devolvemos una lista con sólo este nodo.
    [(and (ArbolVacio? (ABB-izq ar)) (ArbolVacio? (ABB-der ar))) (list (ABB-elemento ar))]
    
    ;; Si no es una hoja, llamamos recursivamente a hojas para los subárboles y concatenamos los resultados.
    [else (append (hojas (ABB-izq ar)) (hojas (ABB-der ar)))]))

