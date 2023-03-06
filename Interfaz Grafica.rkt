#lang racket/gui
; Se utilizará la interfaz grafica de Racket


(require 2htdp/universe) ;libreria encargada de los efecetos de movimiento
(require 2htdp/image) ;libreria que permite cargar imagenes


(require (lib "graphics.ss" "graphics")) ;libreria de graficos simples
(open-graphics) ;aqui se llama a la libreria


;(define v_principal (open-viewport "Ventana Principal" 700 600))   ;;se crea la ventana principal, y se pasan los parametros (ancho,alto)
;(define oculta (open-pixmap "Ventana Principal" 700 600))  ; crea una ventana oculta
;;(define imagen-fondo (bitmap/file "C:/Users/cecil/OneDrive/Documentos/4line-/2.png"))
;(define panel (new horizontal-panel% [parent frame]))
;(define canvas (new canvas% [parent panel]))



; Crea el frame principal instanciando la clase frame%                                   
(define frame (new frame% [label "Frame Principal"]
                   [width 700] [height 600]))
(send frame show #t) ;muestra el frame al llamarlo con el metodo show


;Crea el frame del juego instanciado la clase frame%
(define frame1 (new frame% [label "4 Line"]
                   [width 700] [height 600]))


;Funcion que crea un boton en el frame principal
(define button (new button% [parent frame]
             [label "Empezar"] 
            

             ;Se define el evento que se genera al dar click al boton empezar (abre el frame del juego y cierra el frame principal)
             [callback (lambda (button event)
                         (send frame1 show #t) (send frame show #f))]))





 

