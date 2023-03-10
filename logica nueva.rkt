#lang racket


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

