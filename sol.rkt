#lang racket
(require pict)
(provide (all-defined-out))

;; Dumb force Sudoku solver.

;; =================
;; Data definitions:

;; Val is Natural[1, 9] or false
;; interp. value of a square, or false if blank

;; Board is (listof Val) that is 81 elements long
;; interp. a list of values of every square on the board

;; Pos is Natural[0, 80]
;; interp. the position of a square on the board
;;         for a given Pos p, then
;;          - the zero-indexed row is (quotient p 9)
;;          - the zero-indexed column is (remainder p 9)

;; Unit is (listof Pos) of length 9
;; interp. the position of every square in a unit
;;         a unit is a row, column, or box
;;         no duplicate numbers can be in a unit



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
(define SLICE-COLOUR "Moccasin")
(define FOCUS-COLOUR "LightSalmon")
(define SPACE-COLOUR "LightCyan")
(define NEW-COLOUR "LightGreen")
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

;; Board String (listof Pos) Image -> Image
;; highlight a list of positions on the board with the given colour
(define (highlight bd col lop img)
  (local [(define (highlight-all bd col lop img)
            (cond [(empty? lop) img]
                  [else
                   (highlight-one bd col (first lop) (highlight-all bd col (rest lop) img))]))
          (define (highlight-one bd col p img)
            (pin-over img
                      (x-coordinate (remainder p 9))
                      (y-coordinate (quotient p 9))
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
                    (local [(define column (list-ref COLS (remainder (first box) 9)))
                            (define row (list-ref ROWS (quotient (first box) 9)))
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
      (local [(define column (list-ref COLS (remainder (first box) 9)))
              (define row (list-ref ROWS (quotient (first box) 9)))
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
    (solve-spaces (list 1 2 3 4 5 6 7 8 9))))