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
