(in-package :qr-generator)

(defclass polynomial ()
  ((coefs :initarg :coefs
	       :reader coefs)
   (degree :accessor degree
	   :documentation "This is updated automatically and should not be manually set.")))

(defmethod initialize-instance :after ((object polynomial) &key)
  (with-slots (coefs degree) object
    (setf coefs (remove-trailing-zeros coefs))
    (setf degree (1- (length coefs)))))

(defun remove-trailing-zeros (list)
  (let ((result nil)
	(seen-nonzero nil))
    (dolist (number (reverse list))
      (when (or seen-nonzero (not (zerop number)))
	(setf seen-nonzero t)
	(push number result)))
    result))

(defgeneric (setf coefs) (value poly))

(defmethod (setf coefs) (value (poly polynomial))
  (setf (slot-value poly 'coefs) (remove-trailing-zeros value))
  (setf (degree poly) (1- (length value)))
  poly)

(defun x-power-n (n)
  "Returns the `polynomial' object representing 1*x^n."
  (make-instance 'polynomial :coefs (append (loop repeat n collect 0) '(1))))

(defmethod print-object ((poly polynomial) stream)
  (flet ((merge-lists (l1 l2)
	   (loop for a in l1
	      for b in l2
	      append (list b a)))
	 (exponents (degree)
	   (loop for i upto degree collect i)))
   (print-unreadable-object (poly stream :type t)
     (with-slots (coefs degree) poly
       (format stream "~{~dx^~d~^ + ~}"
	       (nreverse (merge-lists coefs (exponents degree))))))))

(defun nth-galois (n)
  "Return the number in the Galois field corresponding to 2 to the N"
  (if (null n)
      0
      (aref *log-antilog* (abs n))))

(defun galois-exponent (integer)
  "Return the power of two representing INTEGER in the Galois field."
  (position (abs integer) *log-antilog*))

(defun galois-add (&rest numbers)
  (reduce #'logxor (remove-if #'null numbers)))

(defun galois-product (a b)
  (if (or (zerop a) (zerop b))
      0
      (nth-galois (mod (+ (galois-exponent a) (galois-exponent b)) 255))))

(defmethod add (poly-1 poly-2)
  (flet ((pad-polynomial (short-poly to-degree)
	   (append (coefs short-poly)
		   (loop repeat (- to-degree (degree short-poly))
			collect 0))))
    (let ((short-poly (if (<= (degree poly-1) (degree poly-2))
			  poly-1
			  poly-2))
	  (long-poly (if (> (degree poly-1) (degree poly-2))
			 poly-1
			 poly-2)))
      (make-instance 'polynomial
		     :coefs (mapcar #'galois-add (coefs long-poly)
					 (pad-polynomial short-poly (degree long-poly)))))))

(defgeneric multiply (poly-1 poly-2)
  (:documentation "Multiplication of two polynomials in the Galois field. The second one may be an integer."))

(defmethod multiply ((poly polynomial) (constant integer))
  (with-slots (coefs) poly
    (make-instance
     'polynomial
     :coefs (mapcar (lambda (x) (galois-product x constant)) coefs))))


(defmethod multiply ((poly-1 polynomial) (poly-2 polynomial))
  (flet ((shift-degree (poly degree-shift)
	   (unless (zerop degree-shift)
	     (setf (coefs poly)
		   (append (loop repeat degree-shift collect 0)
			   (coefs poly))))
	   poly))
    (loop for coef in (coefs poly-1)
       for degree-shift upfrom 0
       collect (shift-degree (multiply poly-2 coef) degree-shift) into intermediate-products
       finally (return (reduce #'add intermediate-products)))))

(defgeneric divide (poly-1 poly-2))

(defmethod divide ((poly-1 polynomial) (poly-2 polynomial))
  (let ((dividend (if (> (degree poly-1) (degree poly-2))
		      poly-1
		      poly-2))
	(divisor (if (> (degree poly-1) (degree poly-2))
		     poly-2
		     poly-1)))
    (loop 
       for loop-dividend = dividend then (add loop-dividend intermediate-product)
       for loop-divisor = (multiply divisor (x-power-n (- (degree dividend) (degree divisor))))
       then (setf (coefs loop-divisor) (rest (coefs loop-divisor)))
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

