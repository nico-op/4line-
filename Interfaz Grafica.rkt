#lang racket/gui    ;Se utilizará la interfaz grafica de Racket
(require lang/posn) ;se utiliza para importar el módulo posn que proporciona funciones para trabajar con coordenadas de posición en dos dimensiones.
(require 2htdp/universe) ;libreria encargada de los efecetos de movimiento
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


;Funcion que define los eventos que ocurriran al dal click al boton empezar (abre el frame del juego y cierra el frame principal)
;Ademas de guardar el contenido de los text

(define(respuesta-button a b)
  (let((filas(send tf_filas get-value))(columnas(send tf_columnas get-value)))
  (cond
    [(> (string->number filas) 16) (new message% [parent panel3] [label "Error! El numero maximo de filas es de 16"])]
    [(> (string->number columnas) 16) (new message% [parent panel3] [label "Error! El numero maximo de columnas es de 16"])]
    [else (send frame1 show #t)(send frame_p show #f)])))


;Funcion que crea un boton en el frame principal
(define button (new button% [parent panel2]
             [label "Empezar"] [min-width 100] 
                    [min-height 30] [vert-margin 5]
                          [horiz-margin 50] [callback respuesta-button]))



;--------------------------------------------------
               ;VENTANA DE JUEGO
;--------------------------------------------------

;Crea el frame del juego instanciado la clase frame%
(define frame1 (new frame% [label "4 Line"]
                   [width 700] [height 600]))




