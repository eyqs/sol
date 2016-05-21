;;;; Minesweeper solver

;; =================
;; Constants:

(define NUMROWS 15)
(define NUMCOLS NUMROWS)
(define NUMCELL (* NUMROWS NUMCOLS))
(define CELL-WIDTH 20)
(define CELL-HEIGHT CELL-WIDTH)
(define BOARD-WIDTH (* NUMCOLS CELL-WIDTH))
(define BOARD-HEIGHT (* NUMROWS CELL-HEIGHT))
(define device (make-graphics-device (car (enumerate-graphics-types))))



;; =================
;; Data definitions:

;; Value is Natural[0, 9) or #f
;; interp. the number of mines adjacent to a cell,
;;         adjacent means orthogonal or diagonal
(define VM #f)
(define V0 0)
(define V1 1)
(define V2 2)
(define V3 3)
(define V4 4)
(define V5 5)
(define V6 6)
(define V7 7)
(define V8 8)
(define (is-value? v)
  (or (false? v)
      (and (integer? v) (<= 0 v 8))))

(define-structure cell value visible)
;; Cell is (make-cell Value Boolean)
;; interp. (make-cell value state) is a cell, where
;;         value is the value of the cell, as a Value
;;         visible is #t if it is visible, otherwise #f
(define CM (make-cell VM #t))
(define C0 (make-cell V0 #t))
(define C1 (make-cell V1 #f))
(define C2 (make-cell V2 #t))
(define C3 (make-cell V3 #f))
(define C4 (make-cell V4 #t))
(define C5 (make-cell V5 #f))
(define C6 (make-cell V6 #t))
(define C7 (make-cell V7 #f))
(define C8 (make-cell V8 #t))
(define (is-cell? c)
  (and (cell? c)
       (is-value? (cell-value c))
       (boolean? (cell-visible c))))

;; Board is (listof Cell)
;; interp. a list of every cell in the board
(define B0 (list C0 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0
                 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0
                 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0
                 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0
                 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0
                 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0
                 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0
                 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0
                 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0
                 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0
                 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0
                 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0
                 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0
                 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0
                 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0 C0))
(define B1 (list C1 C2 C3 C2 C1 C0 C1 C1 C2 C2 C2 C1 C1 C1 C1
                 C2 CM CM CM C2 C1 C2 CM C2 CM CM C4 C3 CM C2
                 C3 CM C8 CM C4 C2 CM C2 C2 C3 CM CM CM C4 CM
                 C2 CM CM CM C3 CM C4 C3 C1 C1 C4 CM C7 CM C3
                 C1 C2 C3 C2 C2 C3 CM CM C2 C1 C3 CM CM CM C3
                 C1 C1 C2 C1 C1 C3 CM C6 CM C2 C2 CM C5 C4 CM
                 C1 CM C3 CM C2 C3 CM C4 CM C4 C3 C3 CM C3 C2
                 C2 C4 CM C3 C2 CM C4 C4 C3 CM CM C3 C2 C3 CM
                 C1 CM CM C3 C2 C3 CM CM C2 C3 C3 C3 CM C2 C1
                 C2 C3 C4 CM C1 C2 CM C3 C2 C2 CM C2 C2 C2 C1
                 C1 CM C4 C4 C2 C2 C2 C3 C3 CM C2 C1 C1 CM C2
                 C1 C2 CM CM CM C1 C1 CM CM C2 C1 C0 C1 C2 CM
                 C0 C1 C2 C4 C3 C2 C1 C2 C3 C2 C0 C0 C0 C1 C1
                 C1 C1 C1 C1 CM C3 C1 C2 CM C2 C1 C1 C1 C1 C1
                 C1 CM C1 C1 C2 CM CM C2 C1 C2 CM C1 C1 CM C1))
(define B2 (list C0 C1 CM C1 C2 CM C3 C3 CM C2 C0 C0 C0 C0 C0
                 C0 C1 C1 C1 C2 CM CM C5 CM C3 C0 C0 C0 C0 C0
                 C1 C1 C0 C1 C2 C3 C3 CM CM C4 C2 C1 C1 C1 C1
                 CM C1 C0 C1 CM C1 C1 C2 C4 CM CM C1 C1 CM C2
                 C1 C1 C1 C2 C2 C1 C1 C1 C3 CM C4 C2 C3 C3 CM
                 C1 C2 C2 CM C1 C0 C1 CM C2 C1 C2 CM C2 CM C2
                 CM C3 CM C2 C2 C1 C2 C1 C1 C0 C1 C1 C2 C1 C1
                 C2 CM C3 C2 C2 CM C2 C0 C0 C0 C0 C0 C1 C1 C1
                 C1 C2 CM C1 C2 CM C3 C2 C2 C1 C0 C0 C1 CM C2
                 C0 C1 C2 C2 C2 C1 C2 CM CM C2 C0 C1 C2 C3 CM
                 C0 C0 C2 CM C3 C1 C2 C3 CM C2 C0 C1 CM C2 C1
                 C1 C2 C4 CM C3 CM C2 C3 C2 C2 C0 C1 C1 C1 C0
                 C1 CM CM C2 C2 C2 CM C2 CM C1 C0 C0 C0 C0 C0
                 C1 C2 C3 C2 C2 C2 C2 C2 C1 C1 C0 C0 C0 C0 C0
                 C0 C0 C1 CM C2 CM C1 C0 C0 C0 C0 C0 C0 C0 C0))
(define (is-board? b)
  (define (is-loc? b acc)
    (if (null? b)
        (= NUMCELL acc)
        (and (is-cell? (car b))
             (is-loc? (cdr b) (+ acc 1)))))
  (is-loc? b 0))

;; Position is Natural[0, NUMCELL)
;; interp. the position of a cell on the board



;; =================
;; Data conversions:

;; Cell -> String
;; convert Cell to String
(define (cell->str c)
  (cond ((false? (cell-visible c)) "")
        ((false? (cell-value c)) "x")
        (else (number->string (cell-value c)))))

;; Position -> Natural[0, NUMCOLS)
;; Position -> Natural[0, NUMROWS)
;; convert Position to zero-indexed row and column
(define (pos->row p)
  (quotient p NUMROWS))
(define (pos->col p)
  (remainder p NUMCOLS))

;; Natural[0, NUMCOLS) Natural[0, NUMROWS) -> Position
;; convert zero-indexed row and column to Position
(define (rc->pos r c)
  (+ (* r NUMCOLS) c))

;; Board Position -> Cell
;; produce the value at the given position on the board
(define (read-cell b p)
  (list-ref b p))

;; Board Position Cell -> Board
;; produce a new board with the given cell at the given position
(define (fill-cell b p c)
  (append (take b p)
          (list c)
          (drop b (+ p 1))))



;; =================
;; Functions:

(define (main b)
  (begin (graphics-operation device 'set-internal-border-width 20)
         (graphics-set-coordinate-limits
           device 0 0 BOARD-WIDTH BOARD-HEIGHT)
         (sleep-current-thread 100)
         (render b)))

(define (render b)
  (begin (graphics-clear device)
         (do ((i 0 (+ i 1)))
           ((= i NUMCELL))
           (render-cell i (cell->str (read-cell b i))))))

(define (render-cell p c)
  (define x (* CELL-WIDTH (pos->row p)))
  (define y (* CELL-HEIGHT (pos->col p)))
  (begin (draw-rectangle x y (+ x CELL-WIDTH) (+ y CELL-HEIGHT))
         (graphics-draw-text device x y c)))

(define (draw-rectangle x1 y1 x2 y2)
  (begin (graphics-draw-line device x1 y1 x2 y1)
				 (graphics-draw-line device x2 y1 x2 y2)
         (graphics-draw-line device x2 y2 x1 y2)
         (graphics-draw-line device x1 y2 x1 y1)))

