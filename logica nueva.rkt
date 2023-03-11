#lang racket

; holaaa
; crea-matriz: permite crear una matriz n x m
; entrada: n,m -> n tamaño de la fila, m tamaño de la columna 
; salida: matriz del tamaño seleccionado 

(define (crear-matriz n m)
  (crear-matriz-aux m n '()))

(define (crear-matriz-aux n m mat)
  (cond ((equal? 0 m) mat)
        (else (crear-matriz-aux n (- m 1) (append (list (agregafila'() n)) mat)))))

(define (agregafila fila n)
  (cond ((equal? n 0) fila)
        (else (agregafila (cons '0 fila) (- n 1)))))

; (crear-matriz 2 3)

; es viable permite conocer si el componente es viable, es decir que no haya sido elegido anteriormente

; selección obtiene entre los puntajes, el más alto será donde se vaya a colocar la ficha

; objetivo calcula el valor de cada componente
;*********************************************************************************************************************************

; viabilidad: indicar en cuales columnas hay una opción disponible para agregar la ficha
; entrada: una matriz 
; salida: retorna la fila y columna que se encuentra vacía 

(define (viabilidad mat)
  (viabilidad-aux mat 1))

(define (viabilidad-aux mat i)
  (cond ((null? mat)'())
        (else (append (busca-en-fila (car mat) i 1 '())
                      (viabilidad-aux (cdr mat) (+ i 1))))))

(define (busca-en-fila fila i j acum)
  (cond ((null? fila) acum)
        ((zero? (car fila)) (busca-en-fila (cdr fila) i (+ j 1) (append acum (list (list i j)))))
        (else (busca-en-fila (cdr fila) i (+ j 1) acum))))
;*********************************************************************************************************************************

; objetivo: se calcula el valor de cada columna disponible
;
;
(define (objetivo columnas matriz)
  (cond ((null? columnas) '())
        ((Pair? columnas)
         (cons (list (car columnas)
                     (heuristica (numeroFila (elemento (car columnas) matriz))
                                 (car columnas) matriz))
               (objetivo (cdr columnas) matriz)))))

; Pair?: 
(define (Pair? x)
  (cond ((null? x) #f)
        ((not (list? x)) #f)
        (else #t)))

;pertenece a objetivo
(define (elemento index lista)
  (cond ((or (null? lista) (<= index 0))
         #f)
        (else
         (elemento (- index 1) (cdr lista)) (cdr lista)
         (car lista))))

(define (numeroFila columna)
  (define (numeroFila-aux columna indice)
    (cond ((null? columna) #f)
          ((or (eq? (car columna) 0) (eq? (car columna) #f)) indice)
          (else (numeroFila-aux (cdr columna) (+ indice 1)))))
  (numeroFila-aux columna 0))


; heuristica
(define (heuristica fila columna matriz)
  (+
   (combo (verifHorizontal (agregar 2 columna matriz) fila columna))
   (combo (verifVertical (agregar 2 columna matriz) fila columna))
   (combo (verifDiagonal_suma (agregar 2 columna matriz) fila columna))
   (combo_vertical (verifHorizontal (agregar 1 columna matriz) fila columna))
   (combo_vertical (verifVertical (agregar 1 columna matriz) fila columna))
   (combo (verifDiagonal_suma (agregar 1 columna matriz) fila columna))))



; verificaDiagonal
(define (verifDiagonal_suma matriz fila columna)
  (cond ((or (= fila 1) (= columna 1))
         (calcularDiagonal 1 1 matriz))
        ((or (= fila (length matriz)) (= columna (length (car matriz))))
         (calcularDiagonal fila columna matriz))
        (else 0)))

(define (calcularDiagonal fila columna matriz)
  (cond ((null? matriz) 0)
        ((= fila 1)
         (cond ((= columna 1) (+ (car (car matriz)) (calcularDiagonal (+ fila 1) (+ columna 1) (cdr matriz))))
               (else (calcularDiagonal fila (+ columna 1) (cdr matriz)))))
        (else (calcularDiagonal (+ fila 1) 1 (cdr matriz)))))


;verificaVertical
(define (verifVertical matriz fila columna)
  (cond ((null? matriz) 0)
        ((= fila 0)
         (+ 1 (verifVertical (cdr matriz) (- fila 1) columna)))
        ((= (numeroFila (car matriz)) columna)
         (+ 1 (verifVertical (cdr matriz) (- fila 1) columna)))
        (else (verifVertical (cdr matriz) (- fila 1) columna))))


;verificaHorizontal 
(define (verifHorizontal matriz fila columna)
  (cond ((null? matriz) 0)
        ((and (equal? (car (car matriz)) fila) (equal? (cdr (car matriz)) columna))
         (+ 1 (verifHorizontal (cdr matriz) fila columna)))
        (else (verifHorizontal (cdr matriz) fila columna))))


; agregar es parte de la función heuristica


(define (combo num)
  (cond ((<= num 2) num)
        (else (+ 1 (combo (- num 1))))))

(define (combo_vertical num)
  (cond ((> num 1) (+ num 2))
        ((= num 1) 1)
        (else 0)))

(define (agregar elemento columna matriz)
  (cond ((null? matriz) '())
        ((zero? columna) (cons (cons elemento (car matriz)) (cdr matriz)))
        (else (cons (car matriz) (agregar elemento (- columna 1) (cdr matriz))))))


;*********************************************************************************************************************************
; solución el valor del puntaje total 
; cantidad-filas:
; entrada: mat -> una matriz de n x m
; salida: 
(define (cantidad-filas mat)
  (cond ((null? mat)0)
        (else (+ 1 (cantidad-filas (cdr mat))))))

;*********************************************************************************************************************************
; cantidad-columnas:
; entrada: mat -> una matriz de n x m
; salida:
(define (cantidad-columnas mat)
  (cond ((null? mat)0)
        (else (length (car mat)))))

;*********************************************************************************************************************************
;obtener-ele:
;entrada: una lista y un índice 
;salida: retorna el elemento que se encuentra en el índice indicado 
(define (obtener-ele lista idx)
  (cond ((null? lista) '())
    ; Si el índice es 0, devolver el primer elemento de la lista
    ((= idx 1) (car lista))
    ; Si el índice es mayor que 0, llamar recursivamente a la función con el resto de la lista y el índice reducido en 1
    (else (obtener-ele (cdr lista) (- idx 1)))))

;*********************************************************************************************************************************
;obtnernum:función que se encarga de obtener el número de una matriz en determinada posición 
;entrada: una matriz y las posiciones (i, j)
;salida: el elemento correspondiente en la posición (i, j) de la matriz.
(define (obtenernum mat i j)
  (cond ((null? mat) '())
        ((= i 0) (obtener-ele (car mat) j))
        (else (obtenernum (cdr mat) (- i 1) j))))

;*********************************************************************************************************************************
; vacio: verifica si en la matriz hay algun elemento vacio en la matriz
; entrada: mat,i,j -> una matriz n x m, indice de fila, indice de columna
; salida: retorna #t en caso de encontrar vacio, sino #f en situación contraria
(define (vacio? mat i j)
  (equal? (obtenernum mat i j) '()))

;*********************************************************************************************************************************
; largo: función recursiva para obtener el largo de una lista
; entrada: una lista
; salida: retorna el largo que posee la lista 
(define (largo lista)
  (cond ((null? lista) 0) ; si la lista es vacía, devuelve 0
        (else (+ 1 (largo (cdr lista)))))) ; en otro caso, suma 1 y llama recursivamente a la función con la cola de la lista

;*********************************************************************************************************************************
; invertir: función que invierte el orden de una lista
; entrada: una lista 
; salida: una lista con sus datos invertidos 
(define (invertir lista)
  (cond ((null? lista) '())
        ((null? (cdr lista)) lista)
        (else (append (invertir (cdr lista)) (lista (car lista)))))) 

;*********************************************************************************************************************************
