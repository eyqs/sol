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
