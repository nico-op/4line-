#lang racket
; Se utilizará la interfaz grafica de Racket

(require 2htdp/universe) ;libreria encargada de los efecetos de movimiento
(require 2htdp/image) ;libreria que permite cargar imagenes
(require (lib "graphics.ss" "graphics")) ;libreria de graficos simples
(open-graphics) ;aqui se llama a la libreria


(define v_principal (open-viewport "Ventana Principal" 700 600))   ;;se crea la ventana principal, y se pasan los parametros (ancho,alto)
(define oculta (open-pixmap "Ventana Principal" 700 600)) 