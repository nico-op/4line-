#lang racket/gui

 
(define (suma-valores valores)
  (cond ((null? valores) 0)
        (else (+ (car valores) (suma-valores (cdr valores))))))


;; viabilidad: determina la viabilidad.
;; entrada: candidate -> recibe una lista de conjuntos candidatos.
;; salida: booleano -> en #t en caso de ser mayor a cero, #f en caso de ser menor a cero.
(define (viabilidad candidate)
  (cond ((> (suma-valores candidate) 0) #t)
        (else #f)))

;; como usar:
;; (viabilidad '(-1 0 1 3 4 5))



(define (matrix-columns matrix)
  (reverse
   (my-foldl (lambda (row cols)
               (cons (cons (car row) (car cols)) (cdr cols)))
             (list (list))
             matrix)))

(define (is-viable-columns matrix)
  (reverse
   (my-foldl (lambda (col viable)
               (cons (viabilidad col) viable))
             (list)
             (matrix-columns matrix))))



(define (my-foldl f init lst)
  (cond ((null? lst)init)
        (else(my-foldl f (f (car lst) init) (cdr lst)))))


