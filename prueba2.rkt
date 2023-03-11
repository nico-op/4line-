#lang racket/gui
(require racket/draw)
#|Objetivo Crear la matriz para las fichas|#




;;Primero, crear la pantalla de juego
;;Frame
(define frame (new frame% [label "4 Line"][width 500][height 400]))

;;Segundo, crear un canva
(define canvas (new canvas% [parent frame]))
(define dc (send canvas get-dc))


;;Tercero, crear los colores y lapices

;; Pens and Brushes
(define bluePen (make-object pen% "BLUE" 3 'solid))
(define blackPen (make-object pen% "BLACK" 2 'solid))
(define blueBrush (make-object brush% "BLUE" 'solid))
(define yellowBrush (make-object brush% "YELLOW" 'solid))
(define redBrush (make-object brush% "RED" 'solid))
(define no-brush (make-object brush% "BLACK" 'transparent))


;;Crear divisiones en la pantalla de juego
(define (drawGrid dc)
  (send dc set-pen bluePen)
  (send dc set-brush no-brush)

  ;; draw grid
  (send dc draw-rectangle 145 60 210 180)
  ;;     draw horizontal
  (send dc draw-line 145 90 353 90)
  (send dc draw-line 145 120 353 120)
  (send dc draw-line 145 150 353 150)
  (send dc draw-line 145 180 353 180)
  (send dc draw-line 145 210 353 210)
  ;;     draw vertical
  (send dc draw-line 175 60 175 237)
  (send dc draw-line 205 60 205 237)
  (send dc draw-line 235 60 235 237)
  (send dc draw-line 265 60 265 237)
  (send dc draw-line 295 60 295 237)
  (send dc draw-line 325 60 325 237)
)

(send frame show #t)
(sleep/yield 1)
(drawGrid dc)

