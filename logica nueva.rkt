#lang racket

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
; funcion-objetivo: se calcula el valor de cada columna disponible
; entrada: colomnas que corresponde al número de columnas que hay en la matriz, y una matriz 
; salida: devuelve el número de columna y el valor heurístico
(define (funcion-objetivo columnas matriz)
  (cond ((null? columnas) '())
        ((Pair? columnas)
         (cons (list (car columnas)
                     (funcion-heuristicastica (valor-fila (elemento-matriz (car columnas) matriz))
                                 (car columnas) matriz))
               (funcion-objetivo (cdr columnas) matriz)))))

; Pair?: verifica si la entrada es una matriz (una lista de listas)
; entrada: x -> es una matiz
; salida: True en caso de que sea una matriz, sino false 
(define (Pair? mat)
  (cond ((null? mat) #f)
        ((not (list? mat)) #f)
        (else #t)))

; elemento-matriz: busca un elemento dentro de la matriz 
; entrada: un índice y una lista
; salida:devuelve el elemento en el índice dado de la matriz
(define (elemento-matriz index lista)
  (cond ((or (null? lista) (<= index 0))
         #f)
        (else
         (elemento-matriz (- index 1) (cdr lista)) (cdr lista)
         (car lista))))

; valor-fila: verifica que índices son diferentes de 0
; entrada: una columna y u índice 
; salida: devuelve el índice del primer elemento en la lista que no es igual a 0 o sino false 
(define (valor-fila columna)
  (define (valor-fila-aux columna indice)
    (cond ((null? columna) #f)
          ((or (equal? (car columna) 0) (equal? (car columna) #f)) indice)
          (else (valor-fila-aux (cdr columna) (+ indice 1)))))
  (valor-fila-aux columna 0))


; funcion-heuristicastica: asignación de valores a las columnas 
;entrada: numero de fila y columna, y la matriz 
;salida: valor de cada columna
(define (funcion-heuristicastica fila columna matriz)
  (+
   (distancia (heu-horizontal  (agregar-ficha 2 columna matriz) fila columna))
   (distancia (heu-vertical  (agregar-ficha 2 columna matriz) fila columna))
   (distancia (total-diagonal  (agregar-ficha 2 columna matriz) fila columna))
   (distancia-vertical (heu-horizontal  (agregar-ficha 1 columna matriz) fila columna))
   (distancia-vertical (heu-vertical  (agregar-ficha 1 columna matriz) fila columna))
   (distancia (total-diagonal  (agregar-ficha 1 columna matriz) fila columna))))



;heu-horizontal: función que verifica si hay un cuatro en línea de manera horizontal
;entrada: matriz, una fila y una columna 
;salida: la cantidad de fichas dentro de la fila 
(define (heu-horizontal  matriz fila columna)
  (cond ((null? matriz) 0)
        ((and (equal? (car (car matriz)) fila) (equal? (cdr (car matriz)) columna))
         (+ 1 (heu-horizontal  (cdr matriz) fila columna)))
        (else (heu-horizontal  (cdr matriz) fila columna))))

;heu-vertical: función que verifica si hay un cuatro en línea de manera vertical 
;entrada: matriz, una fila y una columna 
;salida: la cantidad de fichas dentro de la columna  
(define (heu-vertical  matriz fila columna)
  (cond ((null? matriz) 0)
        ((= fila 0)
         (+ 1 (heu-vertical  (cdr matriz) (- fila 1) columna)))
        ((= (valor-fila (car matriz)) columna)
         (+ 1 (heu-vertical  (cdr matriz) (- fila 1) columna)))
        (else (heu-vertical  (cdr matriz) (- fila 1) columna))))


;total-diagonal: función que verifica si hay un cuatro en línea de manera diagonal 
;entrada: matriz, una fila y una columna 
;salida: la cantidad de fichas de forma diagonal  
(define (total-diagonal  matriz fila columna)
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


; distancia:  función se utiliza para calcular la distancia
; entrada: un número (num)
; salida: distancia entre el número digitado y el número con valor dos de la condición 
(define (distancia num)
  (cond ((<= num 2) num)
        (else (+ 1 (distancia (- num 1))))))

; distancia-vertical:  función se utiliza para calcular la distancia
; entrada: un número (num)
; salida: distancia entre el número digitado y el número con valor uno de la condición 
(define (distancia-vertical num)
  (cond ((> num 1) (+ num 2))
        ((= num 1) 1)
        (else 0)))

; agregar-ficha: agrega un elemento especificado a la columna de la matriz
; entradas: elemento, columna, matriz
(define (agregar-ficha elemento columna matriz)
  (cond ((null? matriz) '())
        ((zero? columna) (cons (cons elemento (car matriz)) (cdr matriz)))
        (else (cons (car matriz) (agregar-ficha elemento (- columna 1) (cdr matriz))))))

;*********************************************************************************************************************************
; seleccion: permite elegir cual de las heuristicas calculadas en cada columna es mejor
; entrada: columnas_heuristica -> una lista de tuplas compuestas por el numero de columna y su puntaje
; salida: retorna el indice de la columna donde tiene el maximo valor de la heuristica
(define (seleccion columnas_heuristica)
  (define (seleccion-aux columnas_heuristica max-heuristica max-indice)
    (cond ((null? columnas_heuristica) max-indice)
          ((>= (cadar columnas_heuristica) max-heuristica)
           (seleccion-aux (cdr columnas_heuristica) (cadar columnas_heuristica) (caar columnas_heuristica)))
          (else (seleccion-aux (cdr columnas_heuristica) max-heuristica max-indice))))
  (seleccion-aux columnas_heuristica 0 #f))

;*********************************************************************************************************************************
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
