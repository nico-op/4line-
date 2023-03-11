;;Dennis Yu - NetID: dy189
;;Description: This is 2 player Connect4
;;Connect 4 is basically like tic tac toe except on a 6 x 7 grid that requires 4 in a row to win
;;The buttons simply represent which column either player wishes to put a piece in
;;A lot of frustration with this project came from relearning how to code in scheme
;;and the little amount of resources I had. This game isn't high leveled but the implementation
;;in scheme was a lot tougher and complex since scheme is a functional language.
;;However, doing this project was a good learning experience because of how enjoyable it
;;was to play around with frames, canvas and drawing, similar to creating a mobile app.

#lang racket/gui
(require racket/draw)

;; Global
(define totalTurns 0) ;; max 42
(define player 0) ;; 0 = p2, 1 = p1
(define columnCount (make-vector 7 0))
(define grid
  (lambda (i j bool)
    (build-vector i (lambda (x) (make-vector j bool)))))

;; Grid
(define gridRef
  (lambda (vec i j)
  (vector-ref (vector-ref vec i) j)))
(define gridSet
  (lambda (vec i j n)
    (let ((v (vector-ref vec (car i))))
      (begin (vector-set! v j n)
             (vector-set! vec (car i) v)))))
(define board (grid 6 7 #f))
             

;; Frame
(define frame (new frame% [label "Connect 4"]
                            [width 500]
                            [height 400]))
;; Canvas
(define canvas (new canvas% [parent frame]))
(define dc (send canvas get-dc))
;; Pens and Brushes
(define bluePen (make-object pen% "BLUE" 3 'solid))
(define blackPen (make-object pen% "BLACK" 2 'solid))
(define blueBrush (make-object brush% "BLUE" 'solid))
(define yellowBrush (make-object brush% "YELLOW" 'solid))
(define redBrush (make-object brush% "RED" 'solid))
(define no-brush (make-object brush% "BLACK" 'transparent))


;; Create physical Grid
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

(define horizontal
  (lambda (vec i)
  (cond [(and (number? (vector-ref vec i))
              (number? (vector-ref vec (+ i 1)))
              (number? (vector-ref vec (+ i 2)))
              (number? (vector-ref vec (+ i 3))))
         (cond [(= (vector-ref vec i)
                   (vector-ref vec (+ i 1))
                   (vector-ref vec (+ i 2))
                   (vector-ref vec (+ i 3))
                   1) 1]
               [(= (vector-ref vec i)
                   (vector-ref vec (+ i 1))
                   (vector-ref vec (+ i 2))
                   (vector-ref vec (+ i 3)) 0) 0]
               [else #f])]
        [else #f])))
(define vertical
  (lambda (vec i j)
  (cond [(and (number? (vector-ref (vector-ref vec i) j))
              (number? (vector-ref (vector-ref vec (+ i 1)) j))
              (number? (vector-ref (vector-ref vec (+ i 2)) j))
              (number? (vector-ref (vector-ref vec (+ i 3)) j)))
         (cond[(= (vector-ref (vector-ref vec i) j)
                  (vector-ref (vector-ref vec (+ i 1)) j)
                  (vector-ref (vector-ref vec (+ i 2)) j)
                  (vector-ref (vector-ref vec (+ i 3)) j) 1) 1]
              [(= (vector-ref (vector-ref vec i) j)
                  (vector-ref (vector-ref vec (+ i 1)) j)
                  (vector-ref (vector-ref vec (+ i 2)) j)
                  (vector-ref (vector-ref vec (+ i 3)) j) 0) 0]
              [else #f])]
        [else #f])))
(define drDiag
  (lambda (vec i j)
  (cond [ (and (number? (vector-ref (vector-ref vec i) j))
               (number? (vector-ref (vector-ref vec (+ i 1)) (+ j 1)))
               (number? (vector-ref (vector-ref vec (+ i 2)) (+ j 2)))
               (number? (vector-ref (vector-ref vec (+ i 3)) (+ j 3))))
          (cond [ (= (vector-ref (vector-ref vec i) j)
                     (vector-ref (vector-ref vec (+ i 1)) (+ j 1))
                     (vector-ref (vector-ref vec (+ i 2)) (+ j 2))
                     (vector-ref (vector-ref vec (+ i 3)) (+ j 3)) 1) 1]
                [ (= (vector-ref (vector-ref vec i) j)
                     (vector-ref (vector-ref vec (+ i 1)) (+ j 1))
                     (vector-ref (vector-ref vec (+ i 2)) (+ j 2))
                     (vector-ref (vector-ref vec (+ i 3)) (+ j 3)) 0) 0]
                [else #f])]
        [else #f])))
(define dlDiag
  (lambda (vec i j)
  (cond [ (and (number? (vector-ref (vector-ref vec i) j))
               (number? (vector-ref (vector-ref vec (- i 1)) (+ j 1)))
               (number? (vector-ref (vector-ref vec (- i 2)) (+ j 2)))
               (number? (vector-ref (vector-ref vec (- i 3)) (+ j 3))))
          (cond [ (= (vector-ref (vector-ref vec i) j)
                     (vector-ref (vector-ref vec (- i 1)) (+ j 1))
                     (vector-ref (vector-ref vec (- i 2)) (+ j 2))
                     (vector-ref (vector-ref vec (- i 3)) (+ j 3)) 1) 1]
                [ (= (vector-ref (vector-ref vec i) j)
                     (vector-ref (vector-ref vec (- i 1)) (+ j 1))
                     (vector-ref (vector-ref vec (- i 2)) (+ j 2))
                     (vector-ref (vector-ref vec (- i 3)) (+ j 3)) 0) 0]
                [else #f])]
        [else #f])))

(define checkWin
  (lambda (x)
    (define hor
      (lambda (vec i j)
    (cond [(and (<= i 5) (< j 3))
           (if (number? (horizontal (vector-ref vec i) j))
               (horizontal (vector-ref vec i) j)
               (hor vec i (+ j 1)))]
          [(and (< i 5) (= j 3))
           (if (number? (horizontal (vector-ref vec i) j))
               (horizontal (vector-ref vec i) j)
               (hor vec (+ i 1) 0))]
          [(and (= i 5) (= j 3))
           (if (number? (horizontal (vector-ref vec i) j))
                                    (horizontal (vector-ref vec i) j)
               #f)])))
  (define vert
    (lambda (vec i j)
    (cond [(and (< i 3) (<= j 6))
           (if (number? (vertical vec i j))
               (vertical vec i j)
               (vert vec (+ i 1) j))]
          [(and (= i 3) (<= j 5))
           (if (number? (vertical vec 0 (+ j 1)))
               (vertical vec 0 (+ j 1))
               (vert vec 1 (+ j 1)))])))
  (define dr
    (lambda (vec i j)
    (cond [(and (< i 2) (< j 4))
           (if (number? (drDiag vec i j))
               (drDiag vec i j)
               (dr vec (+ i 1) j))]
          [(and (= i 2) (< j 4))
           (if (number? (drDiag vec i j))
               (drDiag vec i j)
               (dr vec 0 (+ j 1)))])))
  (define dl
    (lambda  (vec i j)
    (cond [(and (> i 3) (< j 4))
           (if (number? (dlDiag vec i j))
               (dlDiag vec i j)
               (dl vec (- i 1) j))]
          [(and (= i 3) (< j 4))
           (if (number? (dlDiag vec i j))
               (dlDiag vec i j)
               (dl vec 5 (+ j 1)))])))
  (cond [(= totalTurns 42) (sleep/yield 900)]
        [(number? (hor x 0 0)) (hor x 0 0)]
        [(number? (vert x 0 0)) (vert x 0 0)]
        [(number? (dr x 0 0)) (dr x 0 0)]
        [(number? (dl x 5 0)) (dl x 5 0)] 
        [else #f])))
      
(define msg
  (new message% [parent frame]
                 [label "Welcome to Connect 4!"]))

(define place
  (lambda (i j)
    (send dc draw-ellipse (+ 145 (* (car i) 30)) (- 240 (* j 30)) 30 30)
    (gridSet board i j player)
    (if (number? (checkWin board))
        ((if (= (checkWin board) 1)
            (send msg set-label "Player 1 Wins!")
            (send msg set-label "Player 2 Wins!"))
         (begin (send num1 enable #f)
               (send num2 enable #f)
               (send num3 enable #f)
               (send num4 enable #f)
               (send num5 enable #f)
               (send num6 enable #f)
               (send num7 enable #f)))
        (void))))

;; Play
(define play
  (lambda i
    (set! totalTurns (add1 totalTurns))
    (send dc set-pen blackPen)
    (if (= (modulo totalTurns 2) 1) ;; Player 1
      (begin (send dc set-brush redBrush)(set! player 1)(send msg set-label "Player 1's turn"))
      (begin (send dc set-brush yellowBrush)(set! player 0)(send msg set-label "Player 2's turn")))
    (place i (vector-ref columnCount (car i)))))

;; Buttons
(define input (new horizontal-panel% 
                  [parent frame]
                  [min-height 10]
                  [stretchable-height #f]))

(define num1 (new button%
                  [parent input]
                  [label "1"]
                  [callback (lambda (button event)
                              (if (< (vector-ref columnCount 0) 6)
                              (begin (vector-set! columnCount 0 (add1 (vector-ref columnCount 0)))
                              (play 0))
                              (#f)))]))
(define num2 (new button%
                  [parent input]
                  [label "2"] 
                  [callback (lambda (button event)
                              (if (< (vector-ref columnCount 1) 6)
                              (begin (vector-set! columnCount 1 (add1 (vector-ref columnCount 1)))
                              (play 1))
                              (#f)))]))
(define num3 (new button%
                  [parent input]
                  [label "3"]
                  [callback (lambda (button event)
                              (if (< (vector-ref columnCount 2) 6)
                              (begin (vector-set! columnCount 2 (add1 (vector-ref columnCount 2)))
                              (play 2))
                              (#f)))]))
(define num4 (new button%
                  [parent input]
                  [label "4"]
                  [callback (lambda (button event)
                              (if (< (vector-ref columnCount 3) 6)
                              (begin (vector-set! columnCount 3 (add1 (vector-ref columnCount 3)))
                              (play 3))
                              (#f)))]))
(define num5 (new button%
                  [parent input]
                  [label "5"]
                  [callback (lambda (button event)
                              (if (< (vector-ref columnCount 4) 6)
                              (begin (vector-set! columnCount 4 (add1 (vector-ref columnCount 4)))
                              (play 4))
                              (#f)))]))
(define num6 (new button%
                  [parent input]
                  [label "6"]                  
                  [callback (lambda (button event)
                              (if (< (vector-ref columnCount 5) 6)
                              (begin (vector-set! columnCount 5 (add1 (vector-ref columnCount 5)))
                              (play 5))
                              (#f)))]))
(define num7 (new button%
                  [parent input]
                  [label "7"]            
                  [callback (lambda (button event)
                              (if (< (vector-ref columnCount 6) 6)
                              (begin (vector-set! columnCount 6 (add1 (vector-ref columnCount 6)))
                              (play 6))
                              (#f)))]))

(send frame show #t)
(sleep/yield 1)
(drawGrid dc)

;; game