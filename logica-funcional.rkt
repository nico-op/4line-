#lang racket/gui


;hola 
(define (suma-valores valores)
  (cond ((null? valores) 0)
        (else (+ (car valores) (suma-valores (cdr valores))))))


;; viabilidad: determina la viabilidad.
;; entrada: candidate -> recibe una lista de conjuntos candidatos.
;; salida: booleano -> en #t en caso de ser mayor a cero, #f en caso de ser menor a cero.
(define (viabilidad candidate)
  (cond ((> (suma-valores candidate) 0) #t)
        (else #f)))


(define (posicion link)
  (cond ((equal? link 0) 9)
        (else (10))))
;; este cambio a realizar mientras se sube otro
;; como usar:
;; (viabilidad '(-1 0 1 3 4 5))
