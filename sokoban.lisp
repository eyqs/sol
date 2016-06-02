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
    (if (null loc)
        (= NUMCELL acc)
        (and (cellp (car loc))
             (locp (cdr loc) (+ acc 1)))))
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
  (if (and (<= 0 r) (<= 0 c) (> NUMROWS r) (> NUMCOLS c))
      (+ (* r NUMCOLS) c)
      nil))

;; String Position -> Position or nil
;; produce the position after moving in the
;; given direction from the given position,
;; or nil if the given position is at an edge
(defun dir-to-pos (dir p)
  (let ((row (pos-to-row p))
        (col (pos-to-col p)))
    (cond ((equal "w" dir)
           (rc-to-pos (- row 1) col))
          ((equal "a" dir)
           (rc-to-pos row (- col 1)))
          ((equal "s" dir)
           (rc-to-pos (+ row 1) col))
          ((equal "d" dir)
           (rc-to-pos row (+ col 1)))
          (t nil))))

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
;; Global variables:

;; *current-world* is the list of all the intermediate boards
;;                 generated in the process of solving the given
;;                 Sokoban board, which is (last *current-world*)
;; *current-cells* is the list of the positions the player is in
;;                 in all the intermediate boards in *current-world*
;; *current-space* is the index of the current board in *current-world*
(defvar *current-world* nil)
(defvar *current-cells* nil)
(defvar *current-space* 0)

;; start with a new board
(defun main (b p)
  (progn (setf *current-world* (list b))
         (setf *current-cells* (list p))
         (setf *current-space* 0)
         (render b p)
         (prompt)))

;; move to the next board, which is more in front in the list
(defun next ()
  (if (not (zerop *current-space*))
      (setf *current-space* (- *current-space* 1)))
  (render (nth *current-space* *current-world*)
          (nth *current-space* *current-cells*)))

;; move to the previous board, which is more behind in the list
(defun prev ()
  (if (not (zerop (- (length *current-world*) *current-space* 1)))
      (setf *current-space* (+ *current-space* 1)))
  (render (nth *current-space* *current-world*)
          (nth *current-space* *current-cells*)))

;; add the new board to the current world and cells
(defun add-board (b p)
  (setf *current-world* (cons b (nthcdr *current-space* *current-world*)))
  (setf *current-cells* (cons p (nthcdr *current-space* *current-cells*)))
  (setf *current-space* 0)
  (render b p)
  (if (solvedp b)
      (princ "Congratulations, you win!")))

;; prompt the user for a direction
(defun prompt ()
  (loop
     (terpri)
     (princ "Enter n for the next state, p for the previous state, or")
     (terpri)
     (princ "enter q to quit, or enter a direction to move [w/a/s/d]: ")
     (let ((str (read-line *query-io*)))
       (cond ((equal "q" str) (return))
             ((equal "n" str) (next))
             ((equal "p" str) (prev))
             (t (move str))))))

;; try to move in the given direction
(defun move (dir)
  (if (null (dir-to-pos dir (nth *current-space* *current-cells*)))
      (princ "Invalid direction!")
      (let* ((b (nth *current-space* *current-world*))
             (p0 (nth *current-space* *current-cells*))
             (p1 (dir-to-pos dir p0))
             (p2 (dir-to-pos dir p1))
             (c1 (read-cell b p1)))
        (cond ((wallp c1)
               (princ "Invalid direction!"))
              ((boxp c1)
               (let ((c2 (read-cell b p2)))
                 (cond ((or (null p2) (wallp c2) (boxp c2))
                        (princ "The box is stuck!"))
                       ((and (donep c1) (donep c2))
                        (add-board (fill-cell (fill-cell b p2 0) p1 1) p1))
                       ((donep c1)
                        (add-board (fill-cell (fill-cell b p2 2) p1 1) p1))
                       ((donep c2)
                        (add-board (fill-cell (fill-cell b p2 0) p1 3) p1))
                       (t (add-board (fill-cell (fill-cell b p2 2) p1 3) p1)))))
              (t (add-board b p1))))))



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
