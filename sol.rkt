;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 160508-Module9B.2Sudoku) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require racket/list)

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

(define ALL-VALS (list 1 2 3 4 5 6 7 8 9))
(define B false)
(define BD1
  (list B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B))
(define BD2
  (list 1 2 3 4 5 6 7 8 9 
        B B B B B B B B B 
        B B B B B B B B B 
        B B B B B B B B B 
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B))
(define BD3
  (list 1 B B B B B B B B
        2 B B B B B B B B
        3 B B B B B B B B
        4 B B B B B B B B
        5 B B B B B B B B
        6 B B B B B B B B
        7 B B B B B B B B
        8 B B B B B B B B
        9 B B B B B B B B))
(define BD4
  (list 2 7 4 B 9 1 B B 5
        1 B B 5 B B B 9 B
        6 B B B B 3 2 8 B
        B B 1 9 B B B B 8
        B B 5 1 B B 6 B B
        7 B B B 8 B B B 3
        4 B 2 B B B B B 9
        B B B B B B B 7 B
        8 B B 3 4 9 B B B))
(define BD4s
  (list 2 7 4 8 9 1 3 6 5
        1 3 8 5 2 6 4 9 7
        6 5 9 4 7 3 2 8 1
        3 2 1 9 6 4 7 5 8
        9 8 5 1 3 7 6 4 2
        7 4 6 2 8 5 9 1 3
        4 6 2 7 5 8 1 3 9
        5 9 3 6 1 2 8 7 4
        8 1 7 3 4 9 5 2 6))
(define BD5
  (list 5 B B B B 4 B 7 B
        B 1 B B 5 B 6 B B
        B B 4 9 B B B B B
        B 9 B B B 7 5 B B
        1 8 B 2 B B B B B 
        B B B B B 6 B B B 
        B B 3 B B B B B 8
        B 6 B B 8 B B B 9
        B B 8 B 7 B B 3 1))
(define BD5s
  (list 5 3 9 1 6 4 8 7 2
        8 1 2 7 5 3 6 9 4
        6 7 4 9 2 8 3 1 5
        2 9 6 4 1 7 5 8 3
        1 8 7 2 3 5 9 4 6
        3 4 5 8 9 6 1 2 7
        9 2 3 5 4 1 7 6 8
        7 6 1 3 8 2 4 5 9
        4 5 8 6 7 9 2 3 1))
(define BD6
  (list B B 5 3 B B B B B 
        8 B B B B B B 2 B
        B 7 B B 1 B 5 B B 
        4 B B B B 5 3 B B
        B 1 B B 7 B B B 6
        B B 3 2 B B B 8 B
        B 6 B 5 B B B B 9
        B B 4 B B B B 3 B
        B B B B B 9 7 B B))
(define BD7
  (list 1 2 3 4 5 6 7 8 B 
        B B B B B B B B 2 
        B B B B B B B B 3 
        B B B B B B B B 4 
        B B B B B B B B 5
        B B B B B B B B 6
        B B B B B B B B 7
        B B B B B B B B 8
        B B B B B B B B 9))
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
(check-expect (read-square BD2 (r-c->pos 0 5)) 6)
(check-expect (read-square BD3 (r-c->pos 7 0)) 8)
(define (read-square bd p)
  (list-ref bd p))

;; Board Pos Val -> Board
;; produce new board with nv at given position
(check-expect (fill-square BD1 (r-c->pos 0 0) 1)
              (cons 1 (rest BD1)))
(define (fill-square bd p nv)
  (append (take bd p)
          (list nv)
          (drop bd (add1 p))))

;; Board -> Board or false
;; produce a solution for bd, or false if bd is unsolvable
;; ASSUME: bd is valid
(check-expect (solve BD4) BD4s)
(check-expect (solve BD5) BD5s)
(check-expect (solve BD7) false)
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
(check-expect (solved? BD1) false)
(check-expect (solved? BD2) false)
(check-expect (solved? BD4s) true)
(define (solved? bd)
  (andmap number? bd))

;; Board -> (listof Board)
;; produce list of valid next boards from board
;; find first empty square, fill it with Natural[1, 9], keep only valid boards
(check-expect (next-boards (cons 1 (rest BD1)))
              (list (cons 1 (cons 2 (rest (rest BD1))))
                    (cons 1 (cons 3 (rest (rest BD1))))
                    (cons 1 (cons 4 (rest (rest BD1))))
                    (cons 1 (cons 5 (rest (rest BD1))))
                    (cons 1 (cons 6 (rest (rest BD1))))
                    (cons 1 (cons 7 (rest (rest BD1))))
                    (cons 1 (cons 8 (rest (rest BD1))))
                    (cons 1 (cons 9 (rest (rest BD1))))))
(define (next-boards bd)
  (keep-only-valid (fill-with-1-9 (find-blank bd) bd)))

;; Board -> Pos
;; produce the position of the first blank square
;; ASSUME: the board has at least one blank square
(check-expect (find-blank BD1) 0)
(check-expect (find-blank (cons 2 (rest BD1))) 1)
(check-expect (find-blank (cons 2 (cons 4 (rest (rest BD1))))) 2)
(define (find-blank bd)
  (cond [(empty? bd) (error "The board didn't have a blank space.")]
        [else
         (if (false? (first bd))
             0
             (+ 1 (find-blank (rest bd))))]))

;; Pos Board -> (listof Board)
;; produce 9 boards, with blank filled with Natural[1, 9]
(check-expect (fill-with-1-9 0 BD1)
              (list (cons 1 (rest BD1))
                    (cons 2 (rest BD1))
                    (cons 3 (rest BD1))
                    (cons 4 (rest BD1))
                    (cons 5 (rest BD1))
                    (cons 6 (rest BD1))
                    (cons 7 (rest BD1))
                    (cons 8 (rest BD1))
                    (cons 9 (rest BD1))))
(define (fill-with-1-9 p bd)
  (local [(define (build-one n)
            (fill-square bd p (+ n 1)))]
    (build-list 9 build-one)))

;; (listof Board) -> (listof Board)
;; produce list containing only valid boards
(check-expect (keep-only-valid (list (cons 1 (cons 1 (rest (rest BD1)))))) empty)
(define (keep-only-valid lobd)
  (filter valid-board? lobd))

;; Board -> Boolean
;; produce true if no unit on the board has the same natural twice, false otherwise
(check-expect (valid-board? BD1) true)
(check-expect (valid-board? BD2) true)
(check-expect (valid-board? BD3) true)
(check-expect (valid-board? BD4) true)
(check-expect (valid-board? BD5) true)
(check-expect (valid-board? (cons 2 (rest BD2))) false)
(check-expect (valid-board? (cons 2 (rest BD3))) false)
(check-expect (valid-board? (cons 2 (cons 6 (rest (rest BD4))))) false)
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