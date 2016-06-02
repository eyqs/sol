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
(defconstant C0 0)
(defconstant C1 1)
(defconstant C2 2)
(defconstant C3 3)
(defconstant C4 4)
(defun cellp (c)
  (and (integerp c) (<= 0 c 4)))

;; Board is (listof Cell)
;; interp. a list of every cell in the board
(defconstant B0 (list C0 C0 C0 C0 C0 C0 C0 C0 C0 C0
                      C0 C0 C0 C0 C0 C0 C0 C0 C0 C0
                      C0 C0 C0 C0 C0 C0 C0 C0 C0 C0
                      C0 C0 C0 C0 C0 C0 C0 C0 C0 C0
                      C0 C0 C0 C0 C0 C0 C0 C0 C0 C0
                      C0 C0 C0 C0 C0 C0 C0 C0 C0 C0
                      C0 C0 C0 C0 C0 C0 C0 C0 C0 C0
                      C0 C0 C0 C0 C0 C0 C0 C0 C0 C0
                      C0 C0 C0 C0 C0 C0 C0 C0 C0 C0
                      C0 C0 C0 C0 C0 C0 C0 C0 C0 C0))
(defconstant B1 (list C4 C4 C4 C4 C4 C4 C4 C4 C4 C4
                      C4 C4 C4 C4 C4 C4 C4 C4 C4 C4
                      C4 C4 C3 C3 C3 C4 C4 C4 C4 C4
                      C4 C4 C3 C3 C3 C4 C3 C3 C4 C4
                      C4 C4 C4 C3 C3 C3 C3 C1 C4 C4
                      C4 C4 C4 C3 C4 C4 C4 C1 C4 C4
                      C4 C3 C2 C3 C4 C4 C4 C1 C4 C4
                      C4 C3 C2 C2 C4 C4 C4 C4 C4 C4
                      C4 C3 C3 C3 C4 C4 C4 C4 C4 C4
                      C4 C4 C4 C4 C4 C4 C4 C4 C4 C4))
(defun boardp (b)
  (defun locp (loc acc)
    (cond ((null loc) (= NUMCELL acc))
          (t (and (cellp (car loc))
                  (locp (cdr loc) (+ acc 1))))))
  (locp b 0))

;; Position is Natural[0, NUMCELL)
;; interp. the position of a cell on the board
(defconstant P1 1)
(defconstant P2 (- NUMCELL 1))
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
  (cond ((= 0 c) "@")
        ((= 1 c) "O")
        ((= 2 c) "a")
        ((= 3 c) "_")
        ((= 4 c) "X")))

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

;; Board -> Display
;; display a text representation of the given board
(defun render (b)
  ;; Natural[0, NUMCOLS) String -> String
  ;; produce a string containing the naturals from 0 inclusive to
  ;; NUMCOLS exclusive, to help support aid players to find cells
  (defun render-nums (i acc)
    (cond ((zerop i) (concatenate 'string "   0" acc))
          ((> 10 i) (render-nums (- i 1) (concatenate 'string " " (write-to-string i) acc)))
          (t (render-nums (- i 1) (concatenate 'string (write-to-string i) acc)))))
  ;; Position Number[0, NUMCELL] -> Display
  ;; display a text representation of the given number of cells
  ;; on the given board starting from the given position
  (defun render-cells (p i)
    (cond ((not (zerop i))
           (progn (princ (concatenate 'string " " (cell-to-string (read-cell b p))))
                  (render-cells (+ p 1) (- i 1))))))
  ;; Number[0, NUMROWS) -> Display
  ;; display a text representation of the given row on the given board
  (defun render-rows (i)
    (cond ((not (= i NUMROWS))
           (progn (cond ((> 10 i) (princ (concatenate 'string " " (write-to-string i))))
                        (t (princ (write-to-string i))))
                  (render-cells (* NUMCOLS i) NUMCOLS)
                  (terpri)
                  (render-rows (+ i 1))))))
  (progn (princ (render-nums (- NUMCOLS 1) ""))
         (terpri)
         (render-rows 0)))

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
