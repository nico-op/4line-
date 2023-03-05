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



;; recorreColumnas: recorre cada columna como una lista
;; entrada: matriz -> recibe la matriz para recorrer a manera de columnas
;; salida: pasa cada una de las columnas que hay, para hacer la comparación en esviablecolumns 
(define (recorreColumnas matriz)
  (reverse
   (listaColumnas (lambda (fila cols)
               (cons (cons (car fila) (car cols)) (cdr cols)))
             (list (list))
             matriz)))

;; esviablecolumns: verifica si en la matriz hay una 
;; entrada: matriz -> recibe una matriz que contiene valores
;; salida: booleano -> indica si hay un espacio en la matriz que sea viable
(define (esviablecolumns matriz)
  (reverse
   (listaColumnas (lambda (col viable)
               (cons (viabilidad col) viable))
             (list)
             (recorreColumnas matriz))))
;;(esviablecolumns '((9 0 -9)(3 -1 -5)(2 -7 -5)(1 -2 -5)))

;; listaColumnas: mete la columna en una lista
;; entrada: colList,inicial,lista ->
;; salida: colLst ->
(define (listaColumnas colLst inicial lista)
  (cond ((null? lista)inicial)
        (else(listaColumnas colLst (colLst (car lista) inicial) (cdr lista)))))




;;Plan para mañana 4/3/23, si un espacio es viable entonces colocar la ficha.