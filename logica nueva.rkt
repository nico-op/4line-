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

(define (vacio mat i j)
  (equal? (obtenernum mat i j) '()))
