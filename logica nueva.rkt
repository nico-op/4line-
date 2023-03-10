#lang racket


;; crea-matriz: permite crear una matriz n x m
;; entrada: n,m -> n tamaño de la fila, m tamaño de la columna 
;;

(define (crear-matriz-aux n m matriz)
  (cond ((equal? 0 m) matriz)
        (else (crear-matriz-aux n (- m 1) (append (list (agregafila'() n) matriz))))))

(define (agregafila fila n)
  (cond ((equal? n 0) fila)
        (else (agregafila (cons '0 fila) (- n 1)))))

;; es viable permite conocer si el componente es viable, es decir que no haya sido elegido anteriormente

;; selección obtiene entre los puntajes, el más alto será donde se vaya a colocar la ficha

;; objetivo calcula el valor de cada componente

;*********************************************************************************************************************************
;; solución el valor del puntaje total 
;; cantidad-filas:
;; entrada: mat -> una matriz de n x m
;; salida: 
(define (cantidad-filas mat)
  (cond ((null? mat)0)
        (else (+ 1 (cantidad-filas (cdr mat))))))

;*********************************************************************************************************************************
;; cantidad-columnas:
;; entrada: mat -> una matriz de n x m
;; salida:
(define (cantidad-columnas mat)
  (cond ((null? mat)0)
        (else (length (car mat)))))

;*********************************************************************************************************************************
;obtener-ele:
;entrada: una lista y un índice 
;salida: retorna el elemento que se encuentra en el índice indicado 
(define (obtener-ele lista idx)
  (cond ((null? lista) (error "La lista está vacía"))
    ; Si el índice es 0, devolver el primer elemento de la lista
    ((= idx 0) (car lista))
    ; Si el índice es mayor que 0, llamar recursivamente a la función con el resto de la lista y el índice reducido en 1
    (else (obtener-ele (cdr lista) (- idx 1)))))

;*********************************************************************************************************************************
;obtnernum:función que se encarga de obtener el número de una matriz en determinada posición 
;entrada: una matriz y las posiciones (i, j)
;salida: el elemento correspondiente en la posición (i, j) de la matriz.
(define (obtenernum mat i j)
  (cond ((null? mat) (error "La matriz está vacía"))
        ((= i 0) (obtener-ele (car mat) j))
        (else (obtenernum (cdr mat) (- i 1) j))))

;*********************************************************************************************************************************
;;vacio: verifica si en la matriz hay algun elemento vacio en la matriz
;;entrada: mat,i,j -> una matriz n x m, indice de fila, indice de columna
;;salida: retorna #t en caso de encontrar vacio, sino #f en situación contraria
(define (vacio mat i j)
  (equal? (obtenernum mat i j) '()))

;*********************************************************************************************************************************
;largo: función recursiva para obtener el largo de una lista
;entrada: una lista
;salida: retorna el largo que posee la lista 
(define (largo lista)
  (cond ((null? lista) 0) ; si la lista es vacía, devuelve 0
        (else (+ 1 (largo (cdr lista)))))) ; en otro caso, suma 1 y llama recursivamente a la función con la cola de la lista

;*********************************************************************************************************************************
;invertir: función que invierte el orden de una lista
;entrada: una lista 
;salida: una lista con sus datos invertidos 
(define (invertir lista)
  (cond ((null? lista) '())
        ((null? (cdr lista)) lista)
        (else (append (invertir (cdr lista)) (lista (car lista)))))) 

;*********************************************************************************************************************************

;;(define matriz-prueba '((1 2 3) (4 5 6) (7 8 9)))