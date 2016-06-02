;;;; Sokoban solver

;; =================
;; Constants:

(defconstant NUMROWS 10)
(defconstant NUMCOLS NUMROWS)
(defconstant NUMCELL (* NUMROWS NUMCOLS))



;; =================
;; Data definitions:

;; Cell is Natural[0, 4]
;; interp. 0 means the cell has a box in a finished position
;;         1 means the cell has nothing in a finished position
;;         2 means the cell has a box
;;         3 means the cell has nothing
;;         4 means the cell is a wall
(defun cellp (c)
  (and (integerp c) (<= 0 c 4)))

;; Board is (listof Cell)
;; interp. a list of every cell in the board
(defconstant B0 (list 0 0 0 0 0 0 0 0 0 0
                      0 0 0 0 0 0 0 0 0 0
                      0 0 0 0 0 0 0 0 0 0
                      0 0 0 0 0 0 0 0 0 0
                      0 0 0 0 0 0 0 0 0 0
                      0 0 0 0 0 0 0 0 0 0
                      0 0 0 0 0 0 0 0 0 0
                      0 0 0 0 0 0 0 0 0 0
                      0 0 0 0 0 0 0 0 0 0
                      0 0 0 0 0 0 0 0 0 0))
(defconstant B1 (list 4 4 4 4 4 4 4 4 4 4
                      4 4 4 4 4 4 4 4 4 4
                      4 4 3 3 3 4 4 4 4 4
                      4 4 3 3 3 4 3 3 4 4
                      4 4 4 3 3 3 3 1 4 4
                      4 4 4 3 4 4 4 1 4 4
                      4 3 2 3 4 4 4 1 4 4
                      4 3 2 2 4 4 4 4 4 4
                      4 3 3 3 4 4 4 4 4 4
                      4 4 4 4 4 4 4 4 4 4))
(defun boardp (b)
  (defun locp (loc acc)
    (cond ((null loc) (= NUMCELL acc))
          (t (and (cellp (car loc))
                  (locp (cdr loc) (+ acc 1))))))
  (locp b 0))

;; Position is Natural[0, NUMCELL)
;; interp. the position of a cell on the board
(defun positionp (p)
  (and (integerp p) (<= 0 p (- NUMCELL 1))))



;; =================
;; Data conversions:

;; Cell -> Boolean
;; produce t if the given cell is a wall, otherwise nil
(defun wallp (c)
  (= 4 c))

;; Cell -> Boolean
;; produce t if the given cell is a box, otherwise nil
(defun boxp (c)
  (or (= 0 c) (= 2 c)))

;; Cell -> Boolean
;; produce t if the given cell is a cell in a finished position, otherwise nil
(defun donep (c)
  (<= 0 c 1))

;; Cell -> String
;; convert Cell to String
(defun cell-to-string (c)
  (cond ((= 0 c) " @")
        ((= 1 c) " O")
        ((= 2 c) " a")
        ((= 3 c) " _")
        ((= 4 c) " X")))

;; Position -> Natural[0, NUMROWS)
;; Position -> Natural[0, NUMCOLS)
;; convert Position to zero-indexed row and column
(defun pos-to-row (p)
  (floor p NUMCOLS))
(defun pos-to-col (p)
  (mod p NUMCOLS))

;; Number Number -> Position or nil
;; convert two numbers to Position only if they are
;; a valid zero-indexed row column pair, otherwise nil
(defun rc-to-pos (r c)
  (cond ((and (<= 0 r) (<= 0 c) (> NUMROWS r) (> NUMCOLS c))
         (+ (* r NUMCOLS) c))
        (t nil)))

;; Board Position -> Cell
;; produce the cell at the given position on the board
(defun read-cell (b p)
  (nth p b))

;; Board Position Cell -> Board
;; produce a new board with the given cell at the given position
(defun fill-cell (b p c)
  (append (subseq b 0 p)
          (list c)
          (nthcdr (+ p 1) b)))



;; =================
;; Functions:

;; Board Position -> Display
;; display a text representation of the given board
;; with the player at the given position
(defun render (b player)
  ;; Position Number[0, NUMCELL] -> Display
  ;; display a text representation of the given number of cells
  ;; on the given board starting from the given position
  (defun render-cells (p i)
    (if (not (zerop i))
        (progn (princ (if (= player p)
                          " U"
                          (cell-to-string (read-cell b p))))
               (render-cells (+ p 1) (- i 1)))))
  ;; Number[0, NUMROWS) -> Display
  ;; display a text representation of the given row on the given board
  (defun render-rows (i)
    (if (not (= i NUMROWS))
        (progn (render-cells (* NUMCOLS i) NUMCOLS)
               (terpri)
               (render-rows (+ i 1)))))
  (render-rows 0))

;; Board -> Boolean
;; produce t if the board is solved, otherwise nil
(defun solvedp (b)
  ;; Position -> Boolean
  ;; produce nil if the position is not solved
  (defun solved-posp (p)
    (cond ((= NUMCELL p) t)
          ((and (not (boxp (read-cell b p)))
                (donep (read-cell b p))) nil)
          ((and (not (donep (read-cell b p)))
                (boxp (read-cell b p))) nil)
          (t (solved-posp (+ p 1)))))
  (solved-posp 0))
