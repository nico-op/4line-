#lang racket/gui

(require 2htdp/image)

(define (draw-lines row col)
  (define canvas (empty-scene 200 200)) ; crea un canvas vacío de 200x200 píxeles
  (define (draw-row r)
    (line 0 (* r 20) 200 (* r 20))) ; dibuja una línea horizontal en la fila r
  (define (draw-col c)
    (line (* c 20) 0 (* c 20) 200)) ; dibuja una línea vertical en la columna c
  (for ([i (in-range row)])
    (draw-row i)) ; dibuja todas las filas
  (for ([j (in-range col)])
    (draw-col j)) ; dibuja todas las columnas
  canvas) ; devuelve el canvas con las líneas dibujadas

