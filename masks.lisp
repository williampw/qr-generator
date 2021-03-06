;;;; Copyright 2016 William Woelffel
;;;; 
;;;; This file is part of qr-generator.
;;;; 
;;;; qr-generator is free software: you can redistribute it and/or modify it
;;;; under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;; 
;;;; qr-generator is distributed in the hope that it will be useful, but
;;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU General Public License
;;;; along with qr-generator.  If not, see <http://www.gnu.org/licenses/>.
;;;;
;;;; You may contact William Woelffel via email: william.woelffel@gmail.com

(in-package #:qr-generator)

(defparameter *masks*
  (list (lambda (point) (evenp (+ (x point) (y point))))
	(lambda (point) (evenp (x point)))
	(lambda (point) (zerop (mod (y point) 3)))
	(lambda (point) (zerop (mod (+ (x point) (y point)) 3)))
	(lambda (point) (evenp (+ (floor (x point) 2) (floor (y point) 3))))
	(lambda (point) (zerop (+ (mod (* (x point) (y point)) 2)
				  (mod (* (x point) (y point)) 3))))
	(lambda (point) (evenp (+ (mod (* (x point) (y point)) 2)
				  (mod (* (x point) (y point)) 3))))
	(lambda (point) (evenp (+ (mod (+ (x point) (y point)) 2)
				  (mod (* (x point) (y point)) 3)))))
  "The eight masks defined by the QR spec. Each mask is a predicate that decides whether a module
  should be flipped (white becomes black, and vice versa).")

(defun flip-color (module)
  "Flips the color of MODULE, a `dot' in the modules list of a `qr-code' object."
  (setf (color module) (opposite-color (color module))))

(defun opposite-color (color)
  (if (eql color 'white) 'black 'white))

(defun apply-mask (qr-code mask)
  (loop with grid-copy = (alexandria:copy-array (grid qr-code))
     for module in (modules qr-code)
     for module-pos = (pos module)
     for module-color = (color module)
     do (when (funcall mask module-pos)
	  (setf (aref grid-copy (x module-pos) (y module-pos))
		(color-value (opposite-color module-color))))
     finally (return grid-copy)))

(defun penalty (array)
  "Total penalty score of a `qr-code' represented by ARRAY."
  (+ (penalty-1 array) (penalty-2 array) (penalty-3 array) (penalty-4 array)))

(defmacro within-grid ((nrows ncols row col &optional (max-row nrows) (max-col ncols)) array
		       &body body)
  "Executes BODY while iterating over the ROW and COL of ARRAY"
  `(destructuring-bind (,nrows ,ncols) (array-dimensions ,array)
     (dotimes (,row ,max-row)
       (dotimes (,col ,max-col)
	 ,@body))))

(defmacro match-pattern (list-pattern &key (reverse nil) (transpose nil))
  (declare (ignorable reverse transpose))
  (multiple-value-bind (black-index white-index pat-length)
      (loop for index upfrom 0
	 for color in list-pattern
	 when (= color 0) collect index into black-index
	 when (= color 255) collect index into white-index
	 finally (return (values black-index white-index index)))
    (flet ((row-match (black-index white-index array row col)
	     `(and (= 255
		      ,@(loop for idx in white-index collect
			     `(aref ,array ,row (+ ,col ,idx))))
		   (= 0
		      ,@(loop for idx in black-index collect
			     `(aref ,array ,row (+ ,col ,idx))))))
	   (col-match (black-index white-index array row col)
	     `(and (= 255
		      ,@(loop for idx in white-index collect
			     `(aref ,array (+ ,row ,idx) ,col)))
		   (= 0
		      ,@(loop for idx in black-index collect
			     `(aref ,array (+ ,row ,idx) ,col)))))
	   (reverse-index (index)
	     (mapcar (lambda (x) (- pat-length x 1)) index)))
      (alexandria:with-gensyms (array nrows ncols row col result)
	`(lambda (,array)
	   (let ((,result 0))
	     (within-grid (,nrows ,ncols ,row ,col ,nrows (- ,ncols ,pat-length)) ,array
	       (when
		   ,(if reverse
			(let ((reverse-black (reverse-index black-index))
			      (reverse-white (reverse-index white-index)))
			  `(or ,(row-match black-index white-index array row col)
			       ,(row-match reverse-black reverse-white array row col)))
			(row-match black-index white-index array row col))
		 (incf ,result)))

	     (within-grid (,nrows ,ncols ,row ,col (- ,nrows ,pat-length)) ,array
	       ,(when transpose
		  `(when
		       ,(if reverse
			    (let ((reverse-black (reverse-index black-index))
				  (reverse-white (reverse-index white-index)))
			      `(or ,(col-match black-index white-index array row col)
				   ,(col-match reverse-black reverse-white array row col)))
			    (col-match black-index white-index array row col))
		     (incf ,result))))
	     ,result))))))

(defun penalty-1 (array)
  "Enforces the penalty rule that five or more contiguous cells, row-wise or column-wise,  with the
same color should be avoided, on ARRAY."
  (+ (count-contiguous array) (count-contiguous (transpose array))))

(defun penalty-2 (array)
  "Enforces the penalty rule that a square of side two of cells with the same color should be
avoided, on ARRAY."
  (* 3 (match-p2 array)))

(defun penalty-3 (array)
  "Enforces the penalty rule that the pattern [black white black black black white black white white
white], and the reverse, should be avoided row-wise and column-wise, on ARRAY."
  (let ((matcher (match-pattern (0 255 0 0 0 255 0 255 255 255 255) :reverse t :transpose t)))
    (* 40 (funcall matcher array))))

(defun penalty-4 (array)
  "Enforces a penalty rule based on the balance between white and black modules on ARRAY."
  (let* ((dark-modules (count-array array 0))
	 (total-modules (apply #'* (array-dimensions array)))
	 (dark-percentage (* 100 (/ dark-modules total-modules)))
	 (sub-multiple (* 5 (floor dark-percentage 5)))
	 (sup-multiple (+ 5 sub-multiple))
	 (final-values (list (/ (abs (- sub-multiple 50)) 5)
			     (/ (abs (- sup-multiple 50)) 5))))
    (reduce #'min final-values :key (lambda (x) (* 10 x)))))

(defun find-best-mask (qr-code)
  "Returns the index of the mask generating the lowest penalty score on QR-CODE."
  (let ((mask-scores (mapcar (lambda (mask) (penalty (apply-mask qr-code mask))) *masks*)))
    (position (apply #'min mask-scores) mask-scores)))

(defun enforce-mask (qr-code mask)
  "Enforces MASK on QR-CODE: binds the `mask' slot of QR-CODE to the MASK index for bookkeeping,
and actually flips the colors of the modules inside QR-CODE."
  (setf (mask qr-code) mask)
  (dolist (module (modules qr-code))
    (when (funcall (nth mask *masks*) (pos module))
      (flip-color module)))
  (setf (grid qr-code) (pixelize qr-code 1)))

(defun match-p2 (array)
  (let ((result 0))
    (within-grid (nrows ncols row col (1- nrows) (1- ncols)) array
      (when (= (aref array row col)
	       (aref array (1+ row) col)
	       (aref array row (1+ col))
	       (aref array (1+ row) (1+ col)))
	(incf result)))
    result))

(defun greedy-contiguous (array start-row start-col)
  "Progresses in ARRAY from starting coordinates START-ROW & START-COL as long as the values read
are the same as in the first cell. Returns the number of contiguous modules of the same color."
  (destructuring-bind (nrows ncols) (array-dimensions array)
    (declare (ignore nrows))
    (loop for col upfrom start-col below ncols
       for value = (aref array start-row col)
       with seeked-value = (aref array start-row start-col)
       while (= seeked-value value)
       count 1)))

(defun score-contiguous (n-contig-cells)
  "Returns the penalty score of N-CONTIG-CELLS contiguous cells with same value."
  (if (< n-contig-cells 5)
      0
      (+ 3 (- n-contig-cells 5))))

(defun count-contiguous (array)
  "Returns the penalty score for conitguous (row-wise) cells with the same colors in ARRAY."
  (destructuring-bind (nrows ncols) (array-dimensions array)
    (loop for row upfrom 0 below nrows sum
	 (do ((col 0 (+ col (greedy-contiguous array row col)))
	      (contig-score 0))
	     ((>= col ncols) contig-score)
	   (incf contig-score
		 (score-contiguous (greedy-contiguous array row col)))))))

(defun transpose (array)
  "Returns a transposed (rows become columns and vice versa) copy of ARRAY."
  (array-operations:permute '(1 0) array))

(defun count-array (array item &key (test #'=))
  "Count the occurences of ITEM in the 2-D ARRAY."
  (let ((result 0))
    (within-grid (nrows ncols row col) array
      (when (funcall test (aref array row col) item)
  	(incf result)))
    result))
