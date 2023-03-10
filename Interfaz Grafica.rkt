#lang racket/gui    ;Se utilizará la interfaz grafica de Racket
;(require lang/posn) ;se utiliza para importar el módulo posn que proporciona funciones para trabajar con coordenadas de posición en dos dimensiones.
(require 2htdp/image) ;libreria que permite cargar imagenes

;(require (lib "graphics.ss" "graphics")) ;libreria de graficos simples
;(open-graphics) ;aqui se llama a la libreria
;(define v_principal (open-viewport "Ventana Principal" 700 600))   ;;se crea la ventana principal, y se pasan los parametros (ancho,alto)
;(define oculta (open-pixmap "Ventana Principal" 700 600))  ; crea una ventana oculta
;;(define imagen-fondo (bitmap/file "C:/Users/cecil/OneDrive/Documentos/4line-/2.png"))
;(define panel (new horizontal-panel% [parent frame]))
;(define canvas (new canvas% [parent panel]))

;---------------------------------------------------------------
                    ;VENTANA PRINCIPAL
;---------------------------------------------------------------

; Crea el frame principal instanciando la clase frame%                                   
(define frame_p (new frame% [label "Frame Principal"]
                   [width 300] [height 400]))

(send frame_p show #t) ;muestra el frame al llamarlo con el metodo show

;Crea un canvas con mensaje de bienvenido
(define canvas1 (new canvas% [parent frame_p]
             [paint-callback
              (lambda (canvas dc)
                (send dc set-scale 1 1)
                (send dc set-text-foreground "blue")
                (send dc draw-text "BIENVENIDO A 4 LINE" 130 30))]))


;Crea un canvas con una instruccion para el usuario
(define canvas2 (new canvas% [parent frame_p]
             [paint-callback
              (lambda (canvas dc)
                (send dc set-scale 1 1)
                (send dc set-text-foreground "black")
                (send dc draw-text "Defina el tamaño del tablero" 110 10))]))


;Se crea un panel horizontal para colocar los text fields
(define panel1( new horizontal-panel%[parent frame_p] [alignment '(center center)]))


;Crea los campos de texto        ;row fila ,column columna
(define tf_columnas(new text-field% [label "Numero de columnas"] [parent panel1] [min-width 10] [min-height 10] [vert-margin 10] [horiz-margin 10]))
(define tf_filas (new text-field% [label "Numero de filas"] [parent panel1] [min-width 10] [min-height 10] [vert-margin 10] [horiz-margin 10]))


;Se crea un panel horizontal para el boton
(define panel2( new horizontal-panel%[parent frame_p] [alignment '(center center)]))


;Se crea un panel horizontal para colocar el mensaje de error en caso de ingresar un valor de fila o columna mayor a 16
(define panel3( new horizontal-panel%[parent frame_p] [alignment '(center center)]))


;Funcion que define los eventos que ocurriran al dal click al boton empezar (cierra el frame principal)
;Ademas de guardar el contenido de los text

(define(respuesta-button a b)
  (let((filas(send tf_filas get-value))(columnas(send tf_columnas get-value)))
  (cond
    [(> (string->number filas) 16) (new message% [parent panel3] [label "Error! El numero maximo de filas es de 16"])]
    [(> (string->number columnas) 16) (new message% [parent panel3] [label "Error! El numero maximo de columnas es de 16"])]
    [(< (string->number filas) 8) (new message% [parent panel3] [label "Error! El numero minimo de filas es de 8"])]
    [(< (string->number columnas) 8) (new message% [parent panel3] [label "Error! El numero minimo de columnas es de 8"])]
    [else (Juego #t(send tf_columnas get-value)(send tf_filas get-value))(send frame_p show #f)])))   ;cierra la ventana principal y abre la de juego




;Funcion que crea un boton en el frame principal
(define button (new button% [parent panel2]
             [label "Empezar"] [min-width 100] 
                    [min-height 30] [vert-margin 5]
                          [horiz-margin 50] [callback respuesta-button]))



;--------------------------------------------------
               ;VENTANA DE JUEGO
;--------------------------------------------------
 
;Funcionalidad del juego
(define(Juego Estado columna fila)
  (cond((equal? Estado #t)
        
        (define alto 0)  ;va a definir el alto del tablero
        (define ancho 0)  ;va a definir el ancho del tablero

        (for([i (in-range (string->number columna))])   ;segun la cantidad de columnas ese sera el ancho del tablero
          (set! ancho ( + 80 ancho)))

        (for([j (in-range (string->number fila))])  ;segun la cantidad de filas ese sera el alto del tablero
          (set! alto ( + 80 alto)))   ;por cada fila y columna va a añadir 80 pixeles hacia el alto y ancho de la ventana
 
; Coloca las fichas en el tablero
(define (SetToken color posX posY dc)
  (send dc set-brush color 'solid)
  (send dc set-pen color 4 'solid)
  (send dc draw-ellipse (* 80 posY) (* 80 posX) 50 50))

; Con esta funcion se puede conocer la posicion del mouse, obteniendo las coordenadas en "y" y "x" de la ventana
(define (mouse-pos event)
           (define-values (x) (send event get-x))
           (define-values (y) (send event get-y))
           (values x y))

(define Columna 0)
(define Selected #f)

(define (GetColumna x)
  (set! Columna (quotient x 80)))

;Crea el frame del juego instanciado la clase frame%
(define frame_juego (new frame% [label "4 Line"]
                   [width ancho] [height alto]))   ;se pasan las variables de alto y ancho


; Segmentar el tablero con lineas
         (define (draw-canvas canvas dc)
           (send dc set-pen "blue" 4 'solid)
           
           (for ([j (in-range 0 (string->number columna))])
             (send dc draw-line (* j 80) 0 (* j 80) alto))
          
           (cond ((equal? Selected #t) (SetToken "yellow" 7 1 dc)))    ;el 7,7 es donde se coloca la ficha (comienza a contar a partir de cero)
           
           (for ([i (in-range 0 (string->number fila))])
             (send dc draw-line 0 (* i 80) ancho (* i 80))))

; Define a new class of canvas to control what is going to be drawn in the screen. Contains some code to declare the x and y coordinates and set them
         ; to the actual position of the mouse when the left button is clicked.
         (define my-canvas% (class canvas%
                              (super-new)
                              (define/override (on-event event)
                                (match (send event get-event-type)
                                  ['left-down
                                   (let-values (((x y) (mouse-pos event)))
                                     (GetColumna x) (displayln Columna) (set! Selected #t) (send frame_juego refresh) (send Canva flush))]
                                  [else (void)]))))

         ; Instantiate a new canvas to draw all stuff on the window
         (define Canva (new my-canvas% [parent frame_juego] [paint-callback draw-canvas] [min-width ancho] [min-height alto]))

         ;Funcion que hace el evento de abrir la ventana de juego
         (send frame_juego show #t))


        



))








