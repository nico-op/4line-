#lang racket/gui    ;Se utilizará la interfaz grafica de Racket
;(require lang/posn) ;se utiliza para importar el módulo posn que proporciona funciones para trabajar con coordenadas de posición en dos dimensiones.
(require 2htdp/image) ;libreria que permite cargar imagenes
(require "logica.rkt")

;(require (lib "graphics.ss" "graphics")) ;libreria de graficos simples
;(open-graphics) ;aqui se llama a la libreria
;(define v_principal (open-viewport "Ventana Principal" 700 600))   ;;se crea la ventana principal, y se pasan los parametros (ancho,alto)
;(define oculta (open-pixmap "Ventana Principal" 700 600))  ; crea una ventana oculta
;;(define imagen-fondo (bitmap/file "C:/Users/cecil/OneDrive/Documentos/4line-/2.png"))
;(define panel (new horizontal-panel% [parent frame]))
;(define canvas (new canvas% [parent panel]))

;; Por definir, tomar las variables que son columnas y filas del tablero
;;
;---------------------------------------------------------------
                    ;VENTANA PRINCIPAL
;---------------------------------------------------------------

; Crea el frame principal instanciando la clase frame%                                   
(define frame_p (new frame% [label "Principal 4Line"]
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
                (send dc draw-text "Defina el Tamaño del Tablero" 110 10))]))


;Se crea un panel horizontal para colocar los text fields
(define panel1( new horizontal-panel%[parent frame_p] [alignment '(center center)]))


;Crea los campos de texto  ;row fila ,column columna
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
    [else (Juego #t(send tf_columnas get-value)(send tf_filas get-value))(send frame_p show #f)])))   ;cierra la ventana principal y abre la de juego, obtiene los valores ingresados a los textfields




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
        (define matriz (crear-matriz (string->number fila) (string->number columna)))
        (define alto 0)  ;va a definir el alto del tablero
        (define ancho 0)  ;va a definir el ancho del tablero

        (for([i (in-range (string->number columna))])   ;segun la cantidad de columnas ese sera el ancho del tablero
          (set! ancho ( + 80 ancho)))

        (for([j (in-range (string->number fila))])  ;segun la cantidad de filas ese sera el alto del tablero
          (set! alto ( + 80 alto)))   ;por cada fila y columna va a añadir 80 pixeles hacia el alto y ancho de la ventana
(display matriz)
(newline)


; Coloca las fichas en el tablero
(define (Colocar-ficha color posX posY dc)
  (send dc set-brush color 'solid) ;se rellena el circulo
  (send dc set-pen color 4 'solid)
  (send dc draw-ellipse (* 80 posY) (* 80 posX) 50 50) ;se dibuja el circulo amarillo
  (send dc draw-ellipse (* 80 posY) (* 80 posX) 50 50)) ;se dibuja el circulo rojo


; Con esta funcion se puede conocer la posicion del mouse, obteniendo las coordenadas en "y" y "x" de la ventana
(define (posicion-mouse event)
           (define-values (x) (send event get-x))
           (define-values (y) (send event get-y))
           (values x y))

(define Columna 0)
(define Seleccion #f)

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
           
           (for ([i (in-range 0 (string->number fila))])
           (send dc draw-line 0 (* i 80) ancho (* i 80)))

           (cond ((equal? Seleccion #t) (Colocar-ficha "yellow" 7 1 dc) (Colocar-ficha "red" 5 1 dc))))  ;el 7,7 es donde se coloca la ficha (comienza a contar a partir de cero)
         

;Funcion que abre un mensaje informando la situacion (ganador o perdedor)
         
         (define (Winner Status)
  (send frame_juego delete-child Canva)
  (cond [(equal? Status 1)
         (new message% [parent frame_juego]
                       [label "Ha ganado la partida :)"])]
        [(equal? Status 0)
         (new message% [parent frame_juego]
                       [label "Ha perdido la partdida :("])]
        [(equal? Status 2)
          (new message% [parent frame_juego]
                       [label "Hay un empate"])]
        ))    

         
         
;En este canvas se va a ir refrescando la pantalla de juego con los nuevos cambios o eventos, y se hace un enlace de las coordenadas "x" y "y" reales del mouse
;cuando el usuario vaya haciendo click en el canva

         (define my-canvas% (class canvas%
                              (super-new)
                              (define/override (on-event event)
                                (match (send event get-event-type)
                                  ['left-down
                                   (let-values (((x y) (posicion-mouse event)))
                                     (GetColumna x) (displayln Columna) (set! Seleccion #t) (send frame_juego refresh) (send Canva flush))]
                                  [else (void)]))))

         ; Se crea un canvas nuevo donde se van a colocar todos los elementos graficos, lineas, solidos
         (define Canva (new my-canvas% [parent frame_juego] [paint-callback draw-canvas] [min-width ancho] [min-height alto]))

         ;Funcion que hace el evento de abrir la ventana de juego
         (send frame_juego show #t))


       
))







