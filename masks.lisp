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

(defun penalty-1 (array)
  "Enforces the penalty rule that five or more contiguous cells, row-wise or column-wise,  with the
same color should be avoided, on ARRAY."
  (+ (count-contiguous array) (count-contiguous (transpose array))))

(defun penalty-2 (array)
  "Enforces the penalty rule that a square of side two of cells with the same color should be
avoided, on ARRAY."
  (* 3
     (+ (match-pattern array '((0 0) (0 0)))
	(match-pattern array '((255 255) (255 255))))))

(defun penalty-3 (array)
  "Enforces the penalty rule that the pattern [black white black black black white black white white
white], and the reverse, should be avoided row-wise and column-wise, on ARRAY."
  (let* ((base-pattern '(0 255 0 0 0 255 0 255 255 255 255))
	 (pattern (list base-pattern))
	 (anti-pattern (list (reverse base-pattern))))
    (* 40
       (+ (match-pattern array pattern)
	  (match-pattern array anti-pattern)
	  (match-pattern (transpose array) pattern)
	  (match-pattern (transpose array) anti-pattern)))))

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

(defun match-pattern (array pattern)
  "Counts the matches of PATTERN in ARRAY. PATTERN must be a 2-dimensionnal list."
  (let* ((height (length pattern))
	 (width (length (first pattern)))
	 (pattern-array (make-array (list height width)
				    :initial-contents pattern)))
    (destructuring-bind (nrows ncols) (array-dimensions array)
      (loop for row upto(- nrows height) sum
	   (loop for col upto (- ncols width)
		for row-end = (if (< (+ row height) nrows) (+ row height))
		for col-end = (if (< (+ col width) ncols) (+ col width))
	      count (equalp pattern-array
			    (cl-slice:slice array
					    (cons row row-end)
					    (cons col col-end))))))))

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

(defun count-array (array item)
  "Count the occurences of ITEM in the 2-D ARRAY."
  (count item (array-operations:flatten array)))
