#lang racket/gui
; Se utilizará la interfaz grafica de Racket

(require 2htdp/universe) ;libreria encargada de los efecetos de movimiento
(require 2htdp/image) ;libreria que permite cargar imagenes

(require (lib "graphics.ss" "graphics")) ;libreria de graficos simples
(open-graphics) ;aqui se llama a la libreria


(define v_principal (open-viewport "Ventana Principal" 700 600))   ;;se crea la ventana principal, y se pasan los parametros (ancho,alto)
(define oculta (open-pixmap "Ventana Principal" 700 600))  ; crea una ventana oculta



;;(define imagen-fondo (bitmap/file "C:/Users/cecil/OneDrive/Documentos/4line-/2.png"))
; Make a frame by instantiating the frame% class
(define frame (new frame% [label "Example"]
                   [width 700] [height 600]))

(define frame1 (new frame% [label "Prueba"]
                   [width 700] [height 600]))


(define panel (new horizontal-panel% [parent frame]))

(define canvas (new canvas% [parent panel]))

; Make a button in the frame
(new button% [parent frame]
             [label "Empezar"]

             ; Callback procedure for a button click:
             [callback (lambda (button event)
                         (send frame1 show #t))])





 ; Show the frame by calling its show method
(send frame show #t)



 

