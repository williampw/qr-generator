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


(in-package :qr-generator)

(defclass polynomial ()
  ((coefs :initarg :coefs
	  :reader coefs
	  :type (vector (mod 256)))
   (degree :accessor degree
	   :type integer
	   :documentation "This is updated automatically and should not be manually set.")))

(defmethod initialize-instance :after ((object polynomial) &key)
  (with-slots (coefs degree) object
    (multiple-value-bind (trimmed-coefs n-coefs) (remove-trailing-zeros coefs)
      (declare ((vector (mod 256)) trimmed-coefs)
      	       (integer n-coefs))
      (setf coefs (make-array n-coefs :element-type '(mod 256) :initial-contents trimmed-coefs))
      (setf degree (1- n-coefs)))))

(defgeneric remove-trailing-zeros (sequence)
  (:documentation "Returns SEQUENCE without the trailing zeros."))

(defmethod remove-trailing-zeros ((sequence list))
  (let ((result nil)
	(seen-nonzero nil))
    (dolist (number (reverse sequence))
      (when (or seen-nonzero (not (zerop number)))
	(setf seen-nonzero t)
	(push number result)))
    (if (null result)
	(values (make-array 1 :element-type '(mod 256) :initial-element 0) 1)
	(values (make-array (length result) :element-type '(mod 256) :initial-contents result)
		(length result)))))

(defmethod remove-trailing-zeros ((sequence vector))
  (loop 
     with apparent-length = (length sequence)
     for i downfrom (1- apparent-length) to 0
     for coef = (aref sequence i)
     when (zerop coef)
     count it into trailing-zeros
     until (not (zerop coef))
     finally (return (if (= trailing-zeros apparent-length)
			 (values (make-array 1 :element-type '(mod 256) :initial-element 0) 1)
			 (values (coerce (subseq sequence 0 (- apparent-length trailing-zeros))
					 '(vector (mod 256)))
				 (- apparent-length trailing-zeros))))))

(defgeneric (setf coefs) (value poly))

(defmethod (setf coefs) (value (poly polynomial))
  (multiple-value-bind (trimmed-coefs n-coefs) (remove-trailing-zeros value)
    (declare ((vector (mod 256)) trimmed-coefs)
	     (integer n-coefs))
    (setf (slot-value poly 'coefs) trimmed-coefs)
    (setf (degree poly) (1- n-coefs)))
  poly)

(defun x-power-n (n)
  "Returns the `polynomial' object representing 1*x^n."
  (declare (integer n))
  (make-instance 'polynomial :coefs (append (loop repeat n collect 0) '(1))))

(defmethod print-object ((poly polynomial) stream)
  (flet ((merge-lists (l1 l2)
	   (loop for a across l1
	      for b in l2
	      nconc (list b a)))
	 (exponents (degree)
	   (loop for i upto degree collect i)))
   (print-unreadable-object (poly stream :type t)
     (with-slots (coefs degree) poly
       (format stream "~{~dx^~d~^ + ~}"
	       (nreverse (merge-lists coefs (exponents degree))))))))

(defun nth-galois (n)
  (declare ((mod 256) n))
  "Return the number in the Galois field corresponding to 2 to the N"
  (if (null n)
      0
      (aref *log-antilog* (abs n))))

(defun galois-exponent (integer)
  (declare ((mod 256) integer))
  "Return the power of two representing INTEGER in the Galois field."
  (position (abs integer) *log-antilog*))

(defun galois-add (&rest numbers)
  (reduce #'logxor (remove-if #'null numbers)))

(defun galois-product (a b)
  (declare ((mod 256) a b))
  (if (or (zerop a) (zerop b))
      0
      (nth-galois (mod (+ (galois-exponent a) (galois-exponent b)) 255))))

;; (defmethod add (poly-1 poly-2)
;;   (flet ((pad-polynomial (short-poly to-degree)
;; 	   (append (coefs short-poly)
;; 		   (loop repeat (- to-degree (degree short-poly))
;; 		      collect 0))))
;;     (destructuring-bind (short-poly long-poly) (sort (list poly-1 poly-2) #'< :key #'degree)
;;       (make-instance 'polynomial
;; 		     :coefs (mapcar #'galois-add (coefs long-poly)
;; 				    (pad-polynomial short-poly (degree long-poly)))))))

(defmethod add (poly-1 poly-2)
  (destructuring-bind (short-poly long-poly) (sort (list poly-1 poly-2) #'< :key #'degree)
    (let ((short-coefs (coefs short-poly))
	  (long-coefs (coefs long-poly)))
      (declare ((vector (mod 256)) short-coefs long-coefs))
     (make-instance 'polynomial
		    :coefs (concatenate '(vector (mod 256))
					(map '(vector (mod 256)) #'galois-add short-coefs long-coefs)
					(subseq long-coefs (1+ (degree short-poly))))))))

(defun shift-degree (poly degree-shift)
  "DESCTRUCTIVELY shift the polynomial's degree"
  (unless (zerop degree-shift)
    (setf (coefs poly)
	  (concatenate '(vector (mod 256))
		       (loop repeat degree-shift collect 0) (coefs poly))))
  poly)

(defgeneric multiply (poly-1 poly-2)
  (:documentation "Multiplication of two polynomials in the Galois field. The second one may be an integer."))

(defmethod multiply ((poly polynomial) (constant integer))
  (with-slots (coefs) poly
    (make-instance
     'polynomial
     :coefs (map '(vector (mod 256)) (lambda (x) (galois-product x constant)) coefs))))


(defmethod multiply ((poly-1 polynomial) (poly-2 polynomial))
  (destructuring-bind (short-poly long-poly) (sort (list poly-1 poly-2) #'< :key #'degree)
   (loop for coef integer across (coefs short-poly)
      for degree-shift upfrom 0
      collect (shift-degree (multiply long-poly coef) degree-shift) into intermediate-products
      finally (return (reduce #'add intermediate-products)))))

;; (defmethod multiply ((poly-1 polynomial) (poly-2 polynomial))
;;   (let ((degree-shifts (alexandria:iota (1+ (degree poly-1)))))
;;     (reduce #'add (mapcar (lambda (coef degree-shift)
;; 			    (shift-degree (multiply poly-2 coef) degree-shift))
;; 			  (coefs poly-1)
;; 			  degree-shifts))))

(defgeneric divide (poly-1 poly-2))

(defmethod divide ((poly-1 polynomial) (poly-2 polynomial))
  (destructuring-bind (divisor dividend) (sort (list poly-1 poly-2) #'<= :key #'degree)
    (loop 
       for loop-dividend = dividend then (add loop-dividend intermediate-product)
       for loop-divisor = (multiply divisor (x-power-n (- (degree dividend) (degree divisor))))
       then (setf (coefs loop-divisor) (subseq (coefs loop-divisor)
					       (- (degree loop-divisor)
						  (degree loop-dividend))))
       for intermediate-product = (multiply loop-divisor (alexandria:last-elt (coefs loop-dividend)))
       while (>= (degree loop-dividend) (degree divisor))
       finally (return loop-dividend))))

(defvar *generator-galois*
  (loop with result = (make-array 36)
     for counter upfrom 0
     for i across (subseq *log-antilog* 0 36)
     for pol = (make-instance 'polynomial :coefs '(1 1))
     then (multiply pol (make-instance 'polynomial :coefs (list i 1)))
     do (setf (aref result counter) pol)
     finally (return result)) 
  "Polynomials, in the Galois field, used as generators in the Reed-Solomon process.")

(defun format-divide (format-polynomial)
  "Generate the bits for the format pattern, based on an odd division."
  (loop with base-generator = (make-instance 'polynomial :coefs '(1 1 1 0 1 1 0 0 1 0 1))
     for format-poly = (multiply (x-power-n 10) format-polynomial) then result
     for generator-poly = (multiply (x-power-n (- (degree format-poly)
						  (degree base-generator)))
				    base-generator )
     for result = (add format-poly generator-poly)
     while (> (degree format-poly) 10)
     finally (return result)))
