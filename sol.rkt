#lang racket/gui
(provide (all-defined-out))

;; =================
;; Constants:

(define FONT-SIZE 16)
(define CELL-WIDTH 20)
(define CELL-HEIGHT CELL-WIDTH)
(define LINE-WIDTH 2)
(define LINE-HEIGHT LINE-WIDTH)
(define BOX-WIDTH (* 3 CELL-WIDTH))
(define BOX-HEIGHT (* 3 CELL-HEIGHT))
(define BOARD-WIDTH (+ (* 3 BOX-WIDTH) (* 2 LINE-WIDTH)))
(define BOARD-HEIGHT (+ (* 3 BOX-HEIGHT) (* 2 LINE-HEIGHT)))
(define EMPTY-COLOUR "White")
(define SLICE-COLOUR "Moccasin")
(define FOCUS-COLOUR "LightSalmon")
(define SPACE-COLOUR "LightCyan")
(define PLACE-COLOUR "LightGreen")



;; =================
;; Interface frames:

(define frame (new frame% [label "sol"]))
(define menu-bar (new menu-bar% [parent frame]))
(define menu (new menu% [label "&Settings"] [parent menu-bar]))
(new menu-item% [parent menu] [label "Quit"]
     [callback (lambda (menu-item event) (exit))])
(define canvas (new canvas% [parent frame] [style '(transparent)]
                    [min-width BOARD-WIDTH] [min-height BOARD-HEIGHT]))
(define DC (send canvas get-dc))
(define panel (new horizontal-panel% [parent frame] [alignment '(center center)]))
(define prev-btn (new button% [parent panel] [label "<<"]
                      [callback (lambda (button event) (prev))]))
(define current (new message% [parent panel] [label "1"]
                     [min-width (exact-round (* 3 (send DC get-char-width)))]))
(define next-btn (new button% [parent panel] [label ">>"]
                      [callback (lambda (button event) (next))]))
(send frame show true)



;; =================
;; Data definitions:

;; Value is Natural[1, 9] or false
;; interp. the number inside a cell, or
;;         false if the cell is blank
(define V0 false)
(define V1 1)
(define V2 2)
(define V3 3)
(define V4 4)
(define V5 5)
(define V6 6)
(define V7 7)
(define V8 8)
(define V9 9)

(define-struct cell (value colour))
;; Cell is (make-cell Value String)
;; interp. (make-cell value colour) is a cell, where
;;         value is the value of the cell, as a Value
;;         colour is the colour of the cell, as a String
(define C0 (make-cell V0 EMPTY-COLOUR))
(define C1 (make-cell V1 EMPTY-COLOUR))
(define C2 (make-cell V2 EMPTY-COLOUR))
(define C3 (make-cell V3 EMPTY-COLOUR))
(define C4 (make-cell V4 EMPTY-COLOUR))
(define C5 (make-cell V5 EMPTY-COLOUR))
(define C6 (make-cell V6 EMPTY-COLOUR))
(define C7 (make-cell V7 EMPTY-COLOUR))
(define C8 (make-cell V8 EMPTY-COLOUR))
(define C9 (make-cell V9 EMPTY-COLOUR))

;; Board is (listof Cell)
;; interp. a list of all 81 cells in the board
(define B4 (list C2 C7 C4 C0 C9 C1 C0 C0 C5
                 C1 C0 C0 C5 C0 C0 C0 C9 C0
                 C6 C0 C0 C0 C0 C3 C2 C8 C0
                 C0 C0 C1 C9 C0 C0 C0 C0 C8
                 C0 C0 C5 C1 C0 C0 C6 C0 C0
                 C7 C0 C0 C0 C8 C0 C0 C0 C3
                 C4 C0 C2 C0 C0 C0 C0 C0 C9
                 C0 C0 C0 C0 C0 C0 C0 C7 C0
                 C8 C0 C0 C3 C4 C9 C0 C0 C0))

;; Position is Natural[0, 81)
;; interp. the position of a cell on the board

;; Unit is (listof Position)
;; interp. a list of the positions of every cell in a unit, which is
;;         a row, column, or box, which cannot have duplicate numbers
(define ALL-POS (build-list 81 identity))
(define ROWS  '(( 0  1  2  3  4  5  6  7  8)
                ( 9 10 11 12 13 14 15 16 17)
                (18 19 20 21 22 23 24 25 26)
                (27 28 29 30 31 32 33 34 35)
                (36 37 38 39 40 41 42 43 44)
                (45 46 47 48 49 50 51 52 53)
                (54 55 56 57 58 59 60 61 62)
                (63 64 65 66 67 68 69 70 71)
                (72 73 74 75 76 77 78 79 80)))
(define COLS  '(( 0  9 18 27 36 45 54 63 72)
                ( 1 10 19 28 37 46 55 64 73)
                ( 2 11 20 29 38 47 56 65 74)
                ( 3 12 21 30 39 48 57 66 75)
                ( 4 13 22 31 40 49 58 67 76)
                ( 5 14 23 32 41 50 59 68 77)
                ( 6 15 24 33 42 51 60 69 78)
                ( 7 16 25 34 43 52 61 70 79)
                ( 8 17 26 35 44 53 62 71 80)))
(define BOXES '(( 0  1  2  9 10 11 18 19 20)
                ( 3  4  5 12 13 14 21 22 23)
                ( 6  7  8 15 16 17 24 25 26)
                (27 28 29 36 37 38 45 46 47)
                (30 31 32 39 40 41 48 49 50)
                (33 34 35 42 43 44 51 52 53)
                (54 55 56 63 64 65 72 73 74)
                (57 58 59 66 67 68 75 76 77)
                (60 61 62 69 70 71 78 79 80)))
(define UNITS (append ROWS COLS BOXES))



;; =================
;; Data conversions:

;; Value -> String
;; convert Value to String
(define (val->str v)
  (if (number? v)
      (number->string v)
      ""))

;; Position -> Natural[0, 9)
;; convert Position to zero-indexed row and column
(define (pos->row p)
  (quotient p 9))
(define (pos->col p)
  (remainder p 9))

;; Natural[0, 9) Natural[0, 9) -> Position
;; convert zero-indexed row and column to Position
(define (rc->pos r c)
  (+ (* r 9) c))

;; Position -> Unit
;; produce the unit which contains the given position
(define (pos->rowu p)
  (list-ref ROWS (pos->row p)))
(define (pos->colu p)
  (list-ref COLS (pos->col p)))
(define (pos->boxu p)
  (local [(define (pos->boxu lou)
            (if (member p (first lou))
                (first lou)
                (pos->boxu (rest lou))))]
    (pos->boxu BOXES)))

;; Board Position -> Cell
;; produce the value at the given position on the board
(define (read-cell b p)
  (list-ref b p))

;; Board Position Cell -> Board
;; produce a new board with the given cell at the given position
(define (fill-cell b p c)
  (append (take b p)
          (list c)
          (drop b (add1 p))))



;; =================
;; Global variables:

;; world is the list of all the intermediate boards
;;          generated in the process of solving the
;;          given Sudoku board, which is (last world)
;; space is the index of the current board in world
(define world (list B4))
(define space 0)

;; start with a new board
(define (main b)
  (begin (set! world (list b))
         (set! space 0)
         (set-index)
         (send next-btn enable true)
         (send prev-btn enable false)
         (render (first world))))

;; set the label with the reversed index of the current board
(define (set-index)
  (send current set-label
        (number->string (- (length world) space))))

;; move to the next board, which is more in front in the list
(define (next)
  (if (zero? space)
      (local [(define try (solve-next (first world)))]
        (if (false? try)
            (begin (set-index)
                   (send next-btn enable false)
                   (send prev-btn enable true)
                   (render (clear (first world))))
            (begin (set! world (cons try world))
                   (set-index)
                   (send prev-btn enable true)
                   (render (first world)))))
      (begin (set! space (sub1 space))
             (set-index)
             (send prev-btn enable true)
             (render (list-ref world space)))))

;; move to the previous board, which is more behind in the list
(define (prev)
  (if (zero? (- (length world) space 2))
      (begin (set! space (add1 space))
             (set-index)
             (send prev-btn enable false)
             (send next-btn enable true)
             (render (last world)))
      (begin (set! space (add1 space))
             (set-index)
             (send next-btn enable true)
             (render (list-ref world space)))))



;; =================
;; Functions:

;; Board -> Image
;; render an image of the given board
(define (render b)
  (local [(define (render-cell p c)
            (local [(define x (+ (* CELL-WIDTH (pos->col p))
                                 (* LINE-WIDTH (quotient (pos->col p) 3))))
                    (define y (+ (* CELL-HEIGHT (pos->row p))
                                 (* LINE-HEIGHT (quotient (pos->row p) 3))))
                    (define-values (w h d a) (send DC get-text-extent (val->str (cell-value c))))]
              (send DC set-brush (new brush% [color (cell-colour c)]))
              (send DC draw-rectangle x y CELL-WIDTH CELL-HEIGHT)
              (send DC draw-text (val->str (cell-value c))
                    (+ x (/ (- CELL-WIDTH w) 2)) (+ y (/ (- CELL-HEIGHT h) 2 )))))]
    (for ([i ALL-POS] [j b])
      (render-cell i j))))

;; Board Unit String -> Board
;; produce the given board with all cells in
;; the given unit coloured by the given colour
(define (colour b lop c)
  (if (empty? lop)
      b
      (fill-cell (colour b (rest lop) c)
                 (first lop)
                 (make-cell (cell-value (read-cell b (first lop))) c))))

;; Board -> Board
;; produce the given board with no colouring
(define (clear b)
  (colour b ALL-POS EMPTY-COLOUR))

;; Board -> Board or false
;; given a board, produce the next board, or
;; false if the given board is the last board
(define (solve-next b)
  (local [(define try-space (solve-step solve-space b))
          (define try-slice (solve-step solve-slice b))]
    (cond [(and (false? try-space) (false? try-slice)) false]
          [(false? try-space) try-slice]
          [else try-space])))

;; (Board Natural[1, 9] Unit -> Board) Board -> Board or false
;; given a board and a next-step function, produce the board with
;; one more step, or false if no more new steps can be found
(define (solve-step c b)
  (local [(define (solve-step b n u)
            (cond [(zero? (- 27 u)) false]
                  [(zero? (- 10 n)) (solve-step b 1 (add1 u))]
                  [else
                   (local [(define try (c b n (list-ref UNITS u)))]
                     (if (equal? b try)
                         (solve-step b (add1 n) u)
                         try))]))]
    (solve-step b 1 0)))

;; Board Natural[1, 9] Unit -> Board
;; given a board, a number, and a unit, produce the board with
;; the unit filled in if the number is the last one in the unit
(define (solve-space b n u)
  (local [(define (get-pos lop)
            (if (false? (cell-value (read-cell b (first lop))))
                (first lop)
                (get-pos (rest lop))))
          (define numbers
            (filter number? (map (lambda (p) (cell-value (read-cell b p))) u)))]
    (if (and (= 8 (length numbers))
             (= 8 (length (remove n numbers))))
        (fill-cell (colour (clear b) u SPACE-COLOUR)
                   (get-pos u)
                   (make-cell n PLACE-COLOUR))
        b)))

;; Board Natural[1, 9] Unit -> Board
;; given a board, a number, and a unit, produce the board with
;; the number inside the unit if it can only fit in one place
(define (solve-slice b n u)
  ;; acc is (listof Position); the list of all positions the number can be in
  (local [(define (get-pos b n lop acc)
            (cond [(empty? lop) acc]
                  [(valid? (first lop))
                   (get-pos b n (rest lop) (cons (first lop) acc))]
                  [else
                   (get-pos b n (rest lop) acc)]))
          (define (valid? p)
            (and (false? (cell-value (read-cell b p)))
                 (false? (in-unit? b n (all-units p)))))
          (define (all-units p)
            (append (pos->rowu p) (pos->colu p) (pos->boxu p)))
          (define positions
            (get-pos b n u empty))]
    (if (= 1 (length positions))
        (fill-cell (colour (colour (clear b)
                                   (all-units (first positions)) SLICE-COLOUR)
                           u FOCUS-COLOUR)
                   (first positions)
                   (make-cell n PLACE-COLOUR))
        b)))

;; Board Natural[1, 9] Unit -> Boolean
;; given a board, a number, and a unit, produce true if
;; the number is in the unit on the board, otherwise false
(define (in-unit? b n lop)
  (cond [(empty? lop) false]
        [(and (number? (cell-value (read-cell b (first lop))))
              (= n (cell-value (read-cell b (first lop))))) true]
        [else (in-unit? b n (rest lop))]))
