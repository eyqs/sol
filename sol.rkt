#lang racket/gui
(require pict)
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
(define SLICE-COLOUR "Moccasin")
(define FOCUS-COLOUR "LightSalmon")
(define SPACE-COLOUR "LightCyan")
(define NEW-COLOUR "LightGreen")



;; =================
;; Windowing:

(define frame (new frame% [label "sol"]))
(define canvas (new canvas% [parent frame] [style '(transparent)]
                    [min-width BOARD-WIDTH] [min-height BOARD-HEIGHT]))
(send frame show true)
(define DC (send canvas get-dc))



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
(define C0 (make-cell V0 "White"))
(define C1 (make-cell V1 "White"))
(define C2 (make-cell V2 "White"))
(define C3 (make-cell V3 "White"))
(define C4 (make-cell V4 "White"))
(define C5 (make-cell V5 "White"))
(define C6 (make-cell V6 "Moccasin"))
(define C7 (make-cell V7 "LightSalmon"))
(define C8 (make-cell V8 "LightCyan"))
(define C9 (make-cell V9 "LightGreen"))

;; Board is (listof Cell)
;; interp. a list of all 81 cells in the board
(define B1 (list C1 C3 C0 C2 C0 C0 C7 C4 C0
                 C0 C2 C5 C0 C1 C0 C0 C0 C0
                 C4 C8 C0 C0 C6 C0 C0 C5 C0
                 C0 C0 C0 C7 C8 C0 C2 C1 C0
                 C5 C0 C0 C0 C9 C0 C3 C7 C0
                 C9 C0 C0 C0 C3 C0 C0 C0 C5
                 C0 C4 C0 C0 C0 C6 C8 C9 C0
                 C0 C5 C3 C0 C0 C1 C4 C0 C0
                 C6 C0 C0 C0 C0 C0 C0 C0 C0))

;; Position is Natural[0, 81)
;; interp. the position of a cell on the board

;; Unit is (listof Position)
;; interp. a list of the positions of every cell in a unit, which is
;;         a row, column, or box, which cannot have duplicate numbers
(define ROWS  '(( 0  1  2  3  4  5  6  7  8)
                ( 9 10 11 12 13 14 15 16 17)
                (18 19 20 21 22 23 24 25 26)
                (27 28 29 30 31 32 33 34 35)
                (36 37 38 39 40 41 42 43 44)
                (45 46 47 48 49 50 51 52 53)
                (54 55 56 57 58 59 60 61 62)
                (63 64 65 66 67 68 69 70 71)
                (72 73 74 75 76 77 78 79 80)))
(define COLS  '((0   9 18 27 36 45 54 63 72)
                (1  10 19 28 37 46 55 64 73)
                (2  11 20 29 38 47 56 65 74)
                (3  12 21 30 39 48 57 66 75)
                (4  13 22 31 40 49 58 67 76)
                (5  14 23 32 41 50 59 68 77)
                (6  15 24 33 42 51 60 69 78)
                (7  16 25 34 43 52 61 70 79)
                (8  17 26 35 44 53 62 71 80)))
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

;; Board Position -> Value
;; produce the value at the given position on the board
(define (read-square b p)
  (list-ref b p))

;; Board Position Value -> Board
;; produce a new board with the given value at the given position
(define (fill-square b p v)
  (append (take b p)
          (list v)
          (drop b (add1 p))))



;; =================
;; Functions:

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
  (local [(define (render-cell pos val)
            (local [(define x (+ (* CELL-WIDTH (pos->col pos))
                                 (* LINE-WIDTH (quotient (pos->col pos) 3))))
                    (define y (+ (* CELL-HEIGHT (pos->row pos))
                                 (* LINE-HEIGHT (quotient (pos->row pos) 3))))
                    (define (val->string val)
                      (if (number? val)
                          (number->string val)
                          ""))
                    (define-values (w h d a) (send DC get-text-extent (val->string val)))]
              (send DC draw-rectangle x y CELL-WIDTH CELL-HEIGHT)
              (send DC draw-text (val->string val)
                    (+ x (/ (- CELL-WIDTH w) 2)) (+ y (/ (- CELL-HEIGHT h) 2 )))))]
    (for ([i bd] [j (build-list 81 identity)])
      (render-cell j i))))

;; Board String (listof Pos) Image -> Image
;; highlight a list of positions on the board with the given colour
(define (highlight bd col lop img)
  (local [(define (highlight-all bd col lop img)
            (cond [(empty? lop) img]
                  [else
                   (highlight-one bd col (first lop) (highlight-all bd col (rest lop) img))]))
          (define (highlight-one bd col p img)
            (pin-over img
                      (x-coordinate (pos->col p))
                      (y-coordinate (pos->row p))
                      (cc-superimpose
                       (filled-rectangle CELL-WIDTH CELL-HEIGHT #:color col)
                       (number-image bd p))))
          (define (x-coordinate n)
            (+ (* n CELL-WIDTH)
               (* (quotient n 3) LINE-WIDTH)))
          (define (y-coordinate n)
            (+ (* n CELL-HEIGHT)
               (* (quotient n 3) LINE-HEIGHT)))
          (define (number-image bd p)
            (if (number? (read-square bd p))
                (text (number->string (read-square bd p)) null FONT-SIZE 0)
                (blank)))]
    (highlight-all bd col lop img)))

;; Temporary testing function before I make this into a world program
(define-struct testboard (bd box n))
(define (test-spaces bd)
  (local [(define (test-spaces bd unit)
            (cond [(<= 27 unit) (test-slices bd)]
                  [(equal? bd (solve-spaces bd (list-ref UNITS unit)))
                   (test-spaces bd (add1 unit))]
                  [else
                   (make-testboard (solve-spaces bd (list-ref UNITS unit)) unit false)]))]
    (test-spaces bd 0)))
(define (test-slices bd)
  (local [(define (test-slices bd box n)
            (cond [(< 9 n) (make-testboard bd false false)]
                  [(= 9 box) (test-slices bd 0 (add1 n))]
                  [(equal? bd (solve-slices bd (list-ref BOXES box) n))
                   (test-slices bd (add1 box) n)]
                  [else
                   (make-testboard (solve-slices bd (list-ref BOXES box) n) box n)]))]
    (test-slices bd 0 1)))
(define B false)
(define test-bd
  (list 1 3 B 2 B B 7 4 B
        B 2 5 B 1 B B B B
        4 8 B B 6 B B 5 B
        B B B 7 8 B 2 1 B
        5 B B B 9 B 3 7 B
        9 B B B 3 B B B 5
        B 4 B B B 6 8 9 B
        B 5 3 B B 1 4 B B
        6 B B B B B B B B))
(define tb (make-testboard test-bd false false))
(define (next)
  (begin
    (set! test-bd (testboard-bd tb))
    (set! tb (test-spaces test-bd))
    (if (not (false? (testboard-box tb)))
        (if (not (false? (testboard-n tb)))
            (highlight-slices test-bd (list-ref BOXES (testboard-box tb)) (testboard-n tb) (render test-bd))
            (highlight test-bd SPACE-COLOUR (list-ref UNITS (testboard-box tb)) (render test-bd)))
        (render test-bd))))

;; Board Unit Natural[1, 9] -> Board
;; given a board, a box unit, and a number, produce the board with the number
;; filled in inside the box unit if there is only one space for it to be
(define (solve-slices bd box n)
  (if (in-unit? bd box n)
      bd
      (local [(define (solve-slices bd box n acc)
                (if (empty? box)
                    (if (empty? acc)
                        bd
                        (if (empty? (rest acc))
                            (fill-square bd (first acc) n)
                            bd))
                    (local [(define column (list-ref COLS (pos->col (first box))))
                            (define row (list-ref ROWS (pos->row (first box))))
                            (define colpos (in-unit? bd column n))
                            (define rowpos (in-unit? bd row n))]
                      (cond [(and (false? colpos) (false? rowpos) (false? (read-square bd (first box))))
                             (solve-slices bd (rest box) n (cons (first box) acc))]
                            [else
                             (solve-slices bd (rest box) n acc)]))))]
        (solve-slices bd box n empty))))

;; Board Unit Natural[1, 9] Image -> Image
;; given a board, a box unit, a number, and an image, produce a board image that highlights
;; every row and column that intersects the box, if the number is in that row or column
(define (highlight-slices bd box n img)
  (if (empty? box)
      img
      (local [(define column (list-ref COLS (pos->col (first box))))
              (define row (list-ref ROWS (pos->row (first box))))
              (define colpos (in-unit? bd column n))
              (define rowpos (in-unit? bd row n))]
        (cond [(not (false? colpos))
               (if (not (false? rowpos))
                   (highlight bd FOCUS-COLOUR (list colpos)
                              (highlight bd SLICE-COLOUR column
                                         (highlight bd FOCUS-COLOUR (list rowpos)
                                                    (highlight bd SLICE-COLOUR row
                                                               (highlight-slices bd (rest box) n img)))))
                   (highlight bd FOCUS-COLOUR (list colpos)
                              (highlight bd SLICE-COLOUR column
                                         (highlight-slices bd (rest box) n img))))]
              [(not (false? rowpos))
               (highlight bd FOCUS-COLOUR (list rowpos)
                          (highlight bd SLICE-COLOUR row
                                     (highlight-slices bd (rest box) n img)))]
              [else
               (highlight-slices bd (rest box) n img)]))))

;; Board Unit Natural[1, 9] -> Pos or false
;; given a board, a unit and a number, return the position
;; of the number in the unit or false if it is not there
(define (in-unit? bd lop n)
  (cond [(empty? lop) false]
        [(and (number? (read-square bd (first lop)))
              (= n (read-square bd (first lop))))
         (first lop)]
        [else
         (in-unit? bd (rest lop) n)]))

;; Board Unit -> Board
;; given a board and a unit, produce the board with the unit
;; filled in if there is only one number left to fill
(define (solve-spaces bd lop)
  (local [(define (solve-spaces lon)
            (if (= 8 (length values))
                (fill-square bd
                             (get-pos lop)
                             (get-value lon))
                bd))
          (define (get-value lon)
            (first (remove* values lon)))
          (define (get-pos lop)
            (if (false? (read-pos (first lop)))
                (first lop)
                (get-pos (rest lop))))
          (define (keep-only-values lov)
            (filter number? lov))
          (define (read-unit lop)
            (map read-pos lop))
          (define (read-pos p)
            (read-square bd p))
          (define values (keep-only-values (read-unit lop)))]
    (solve-spaces '(1 2 3 4 5 6 7 8 9))))