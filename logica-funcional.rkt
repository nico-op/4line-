#lang racket/gui

(define (sum-values values)
  (cond ((null? values) 0)
        (else (+ (car values) (sum-values (cdr values))))))


;; viabilidad: determina la viabilidad
(define (viabilidad candidate)
  (cond ((> (sum-values candidate) 0) #t)
        (else #f)))
;;como usar:
;;(viabilidad '(-1 0 1 3 4 5))
