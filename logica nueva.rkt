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

;; solución el valor del puntaje total 


(define (get-element lst idx)
  (cond ((null? lst) (error "La lista está vacía"))
    ; Si el índice es 0, devolver el primer elemento de la lista
    ((= idx 0) (car lst))
    ; Si el índice es mayor que 0, llamar recursivamente a la función con el resto de la lista y el índice reducido en 1
    (else (get-element (cdr lst) (- idx 1)))))



(define (obtenernum mat i j)
  (cond ((null? mat) (error "La matriz está vacía"))
        ((= i 0) (get-element (car mat) j))
        (else (obtenernum (cdr mat) (- i 1) j))))


;;vacio: verifica si en la matriz hay algun elemento vacio en la matriz
;;entrada: mat,i,j -> una matriz n x m, indice de fila, indice de columna
;;salida: retorna #t en caso de encontrar vacio, sino #f en situación contraria
(define (vacio mat i j)
  (equal? (obtenernum mat i j) '()))



;; cantidad-filas:
;; entrada: mat -> una matriz de n x m
;; salida: 
(define (cantidad-filas mat)
  (cond ((null? mat)0)
        (else (+ 1 (cantidad-filas (cdr mat))))))

;; cantidad-columnas:
;; entrada: mat -> una matriz de n x m
;; salida:
(define (cantidad-columnas mat)
  (cond ((null? mat)0)
        (else (length (car mat)))))



;;(define matriz-prueba '((1 2 3) (4 5 6) (7 8 9)))