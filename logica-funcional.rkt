#lang racket/gui

(define (sum-values values)
  (cond ((null? values) 0)
        (else (+ (car values) (sum-values (cdr values))))))

(define (is-viable candidate)
  (cond ((> (sum-values candidate) 0) #t)
        (else #f)))


