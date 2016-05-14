#lang racket
(require pict)
(provide (all-defined-out))

;; Brute force Sudoku solver.
;;
;; In Sudoku, the board is a 9x9 grid of squares.
;; There are 9 rows and 9 columns, there are also 9
;; 3x3 boxes. Rows, columns and boxes are all units.
;; So there are 27 units.
;;
;; The overall goal of the game is to fill each square with a
;; Natural[1, 9] such that no unit contains a duplicate number.

;; =================
;; Data definitions:

;; Val is Natural[1, 9] or false
;; interp. value of a square, or false if blank

;; Board is (listof Val) that is 81 elements long
;; interp. Visually a board is a 9x9 array of squares, where
;;         each square has a row and column (r, c). But we
;;         represent it as a single flat list, in which the
;;         rows are layed out one after another in a linear
;;         fashion. (See interpretation of Pos below for how
;;         we convert between (r, c) and position in a board.)

;; Pos is Natural[0, 80]
;; interp. the position of a square on the board
;;         for a given Pos p, then
;;          - the zero-indexed row is (quotient p 9)
;;          - the zero-indexed column is (remainder p 9)

;; Unit is (listof Pos) of length 9
;; interp. The position of every square in a unit. There are
;;         27 of these for the 9 rows, 9 columns and 9 boxes.



;; =================
;; Constants:

(define FONT-SIZE 16)
(define CELL-WIDTH 20)
(define CELL-HEIGHT 20)
(define LINE-WIDTH 2)
(define LINE-HEIGHT 2)
(define BOX-WIDTH (* 3 CELL-WIDTH))
(define BOX-HEIGHT (* 3 CELL-HEIGHT))
(define BOARD-WIDTH (+ (* 3 BOX-WIDTH) (* 2 LINE-WIDTH)))
(define BOARD-HEIGHT (+ (* 3 BOX-HEIGHT) (* 2 LINE-HEIGHT)))
(define ROWS
  (list (list  0  1  2  3  4  5  6  7  8)
        (list  9 10 11 12 13 14 15 16 17)
        (list 18 19 20 21 22 23 24 25 26)
        (list 27 28 29 30 31 32 33 34 35)
        (list 36 37 38 39 40 41 42 43 44)
        (list 45 46 47 48 49 50 51 52 53)
        (list 54 55 56 57 58 59 60 61 62)
        (list 63 64 65 66 67 68 69 70 71)
        (list 72 73 74 75 76 77 78 79 80)))
(define COLS
  (list (list 0  9 18 27 36 45 54 63 72)
        (list 1 10 19 28 37 46 55 64 73)
        (list 2 11 20 29 38 47 56 65 74)
        (list 3 12 21 30 39 48 57 66 75)
        (list 4 13 22 31 40 49 58 67 76)
        (list 5 14 23 32 41 50 59 68 77)
        (list 6 15 24 33 42 51 60 69 78)
        (list 7 16 25 34 43 52 61 70 79)
        (list 8 17 26 35 44 53 62 71 80)))
(define BOXES
  (list (list  0  1  2  9 10 11 18 19 20)
        (list  3  4  5 12 13 14 21 22 23)
        (list  6  7  8 15 16 17 24 25 26)
        (list 27 28 29 36 37 38 45 46 47)
        (list 30 31 32 39 40 41 48 49 50)
        (list 33 34 35 42 43 44 51 52 53)
        (list 54 55 56 63 64 65 72 73 74)
        (list 57 58 59 66 67 68 75 76 77)
        (list 60 61 62 69 70 71 78 79 80)))
(define UNITS (append ROWS COLS BOXES))



;; =================
;; Functions:

;; Natural[0, 8] Natural[0, 8] -> Pos
;; convert zero-indexed row and column to Pos
(define (r-c->pos r c)
  (+ (* r 9) c))

;; Board Pos -> Val
;; produce value at given position on board
(define (read-square bd p)
  (list-ref bd p))

;; Board Pos Val -> Board
;; produce new board with nv at given position
(define (fill-square bd p nv)
  (append (take bd p)
          (list nv)
          (drop bd (add1 p))))

;; Board -> Board or false
;; produce a solution for bd, or false if bd is unsolvable
;; ASSUME: bd is valid
(define (solve bd)
  (local [(define (fn-for-bd bd)
            (if (solved? bd)
                bd
                (fn-for-lobd (next-boards bd))))
          (define (fn-for-lobd lobd)
            (cond [(empty? lobd) false]
                  [else
                   (local [(define try (fn-for-bd (first lobd)))]
                     (if (not (false? try))
                         try
                         (fn-for-lobd (rest lobd))))]))]
    (fn-for-bd bd)))

;; Board -> Boolean
;; produce true if board is solved
;; ASSUME: board is valid, so it is solved if it is full
(define (solved? bd)
  (andmap number? bd))

;; Board -> (listof Board)
;; produce list of valid next boards from board
;; find first empty square, fill it with Natural[1, 9], keep only valid boards
(define (next-boards bd)
  (keep-only-valid (fill-with-1-9 (find-blank bd) bd)))

;; Board -> Pos
;; produce the position of the first blank square
;; ASSUME: the board has at least one blank square
(define (find-blank bd)
  (cond [(empty? bd) (error "The board didn't have a blank space.")]
        [else
         (if (false? (first bd))
             0
             (+ 1 (find-blank (rest bd))))]))

;; Pos Board -> (listof Board)
;; produce 9 boards, with blank filled with Natural[1, 9]
(define (fill-with-1-9 p bd)
  (local [(define (build-one n)
            (fill-square bd p (+ n 1)))]
    (build-list 9 build-one)))

;; (listof Board) -> (listof Board)
;; produce list containing only valid boards
(define (keep-only-valid lobd)
  (filter valid-board? lobd))

;; Board -> Boolean
;; produce true if no unit on the board has the same natural twice, false otherwise
(define (valid-board? bd)
  (local [(define (valid-units? lou)        ;(listOf Unit) -> Boolean
            (andmap valid-unit? lou))       ; - produce true if all units are valid
          (define (valid-unit? lop)         ;(listof Pos) -> Boolean
            (no-duplicates?                 ; - produce true if a single unit is valid
             (keep-only-values
              (read-unit lop))))
          (define (no-duplicates? lon)      ;(listof Natural[1, 9]) -> Boolean
            (cond [(empty? lon) true]       ; - produce true if no natural appears twice
                  [else
                   (if (member (first lon) (rest lon))
                       false
                       (no-duplicates? (rest lon)))]))
          (define (keep-only-values lov)    ;(listof Val) -> (listof Natural[1, 9])
            (filter number? lov))           ; - produce a list of only naturals
          (define (read-unit lop)           ;(listof Pos) -> (listof Val)
            (map read-pos lop))             ; - produce values of bd in the unit
          (define (read-pos p)              ;Pos -> Val
            (read-square bd p))]            ; - produce contents of bd at p
    (valid-units? UNITS)))

;; Board -> Image
;; render an image of the current board
(define (render bd)
  (local [(define VLINE (rectangle LINE-WIDTH BOX-HEIGHT))
          (define HLINE (rectangle BOARD-WIDTH LINE-HEIGHT))
          (define (render-bd bd)
            (vc-append (hc-append (first bd)   VLINE (second bd) VLINE (third bd))
                       HLINE
                       (hc-append (fourth bd)  VLINE (fifth bd)  VLINE (sixth bd))
                       HLINE
                       (hc-append (seventh bd) VLINE (eighth bd) VLINE (ninth bd))))
          (define (list-bd bd acc)
            ;; acc is (listof Unit); the boxes to be rendered
            (cond [(empty? acc) empty]
                  [else
                   (cons (render-box (list-box bd (first acc)))
                         (list-bd bd (rest acc)))]))
          (define (render-box box)
            (table 3 box cc-superimpose cc-superimpose 0 0))
          (define (list-box bd box)
            (cond [(empty? box) empty]
                  [else
                   (cons (render-cell (read-square bd (first box)))
                         (list-box bd (rest box)))]))
          (define (render-cell val)
            (if (number? val)
                (cc-superimpose (rectangle CELL-WIDTH CELL-HEIGHT)
                                (text (number->string val) null FONT-SIZE 0))
                (rectangle CELL-WIDTH CELL-HEIGHT)))]
    (render-bd (list-bd bd BOXES))))