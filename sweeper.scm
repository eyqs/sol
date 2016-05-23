;;;; Minesweeper solver

;; =================
;; Constants:

(define NUMROWS 15)
(define NUMCOLS NUMROWS)
(define NUMCELL (* NUMROWS NUMCOLS))



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
                 C1 CM C4 C4 C3 C2 C2 C3 C3 CM C2 C1 C1 CM C2
                 C1 C2 CM CM CM C1 C1 CM CM C2 C1 C0 C1 C2 CM
                 C0 C1 C2 C4 C3 C2 C1 C3 C3 C2 C0 C0 C0 C1 C1
                 C1 C1 C1 C1 CM C3 C2 C2 CM C2 C1 C1 C1 C1 C1
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

;; Cell -> Boolean
;; produce #t if Cell is a mine, otherwise #f
(define (is-mine? c)
  (false? (cell-value c)))

;; Cell -> String
;; convert Cell to String
(define (cell->str c)
  (cond ((false? (cell-visible c)) "_ ")
        ((is-mine? c) "* ")
        (else (string-append (number->string (cell-value c)) " "))))

;; Position -> Natural[0, NUMROWS)
;; Position -> Natural[0, NUMCOLS)
;; convert Position to zero-indexed row and column
(define (pos->row p)
  (quotient p NUMCOLS))
(define (pos->col p)
  (remainder p NUMCOLS))

;; Natural[0, NUMROWS) Natural[0, NUMCOLS) -> Position
;; convert zero-indexed row and column to Position
(define (rc->pos r c)
  (+ (* r NUMCOLS) c))

;; Number Number -> Position or #f
;; convert two numbers to Position only if they are
;; a valid zero-indexed row column pair, otherwise #f
(define (rc?->pos r c)
  (cond ((and (<= 0 r) (<= 0 c) (> NUMROWS r) (> NUMCOLS c))
         (rc->pos r c))
        (else #f)))

;; Board Position -> Cell
;; produce the cell at the given position on the board
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

;; start with a new board
(define (main b)
  (render (reset b)))

;; Board -> Display
;; display a text representation of the given board
(define (render b)
  ;; Board Position -> Display
  ;; display a text representation of the row of cells
  ;; on the given board starting from the given position
  (define (render-row b i)
    (do ((j 0 (+ j 1)))
        ((= j NUMCOLS))
        (display (cell->str (read-cell b (+ i j))))))
  (do ((i 0 (+ i 1)))
      ((= i NUMROWS))
      (render-row b (* NUMCOLS i))
      (newline)))

;; Board -> Board
;; produce the given board with all cells not visible
;; and all non-mine cells with the proper number value
(define (reset b)
  ;; Board Position -> Board
  ;; set the cell in the given position on the given board
  ;; to the proper number value and make it not visible
  (define (reset-board b i)
    (cond ((= i NUMCELL) b)
          ((is-mine? (read-cell b i))
           (reset-board (fill-cell b i (make-cell #f #f)) (+ i 1)))
          (else
           (reset-board (fill-cell b i (make-cell (get-number b i 0 (neighbours i)) #f)) (+ i 1)))))
  ;; Board Position Number (listof Position) -> Number
  ;; produce the number of mines in the cells in lop
  (define (get-number b p n lop)
    (cond ((null? lop) n)
          ((is-mine? (read-cell b (car lop))) (get-number b p (+ n 1) (cdr lop)))
          (else (get-number b p n (cdr lop)))))
  ;; Position -> (listof Position)
  ;; produce a list of all positions adjacent to p
  (define (neighbours p)
    (let ((r (pos->row p))
          (c (pos->col p)))
      (filter (lambda (p) (not (false? p)))
              (list (rc?->pos (+ r 1) (+ c 1))
                    (rc?->pos (+ r 1) (+ c 0))
                    (rc?->pos (+ r 1) (- c 1))
                    (rc?->pos (+ r 0) (+ c 1))
                    (rc?->pos (+ r 0) (- c 1))
                    (rc?->pos (- r 1) (+ c 1))
                    (rc?->pos (- r 1) (+ c 0))
                    (rc?->pos (- r 1) (- c 1))))))
  (reset-board b 0))

