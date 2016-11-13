(in-package #:qr-generator)

(defun qr-code-size (version)
  (+ (* 4 (1- version)) 21))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Classes to represent geometrical objects

(defclass point ()
  ((x :initarg :x
      :initform (error "Must provide the x coordinate.")
      :accessor x
      :documentation "The x coordinate of the point.")
   
   (y :initarg :y
      :initform (error "Must provide the y coordinate.")
      :accessor y
      :documentation "The y coordinate of the point.")))

(defclass square ()
  ((side :initarg :side
	 :initform (error "Must provide the side of the square")
	 :accessor side)
   
   (position :initarg :pos
	     :initform (error "Must provide either a point or a 2-element list.")
	     :accessor pos)
   
   (color :initarg :color
	  :initform 'white
	  :accessor color)))

(defclass finder-pattern ()
  ((content :initform (list (make-instance 'square :side 7 :pos '(0 0) :color 'black)
			    (make-instance 'square :side 5 :pos '(1 1) :color 'white)
			    (make-instance 'square :side 3 :pos '(2 2) :color 'black)))
   (position :reader pos)   
   (location :reader loc)))

(defclass alignment-pattern ()
  ((content :initform (list (make-instance 'square :side 5 :pos '(-2 -2) :color 'black)
			    (make-instance 'square :side 3 :pos '(-1 -1) :color 'white)
			    (make-instance 'square :side 1 :pos '(0 0) :color 'black)))
   (position :reader pos)))

(defclass qr-code ()
  ((version :initarg :version
	    :initform (error "Must supply the version of the QR code."))
   (content :initarg :content)
   (size)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Printing functions, to make object easier to inspect

(defmethod print-object ((object point) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (x y) object
      (format stream "located at (~d; ~d)" x y))))

(defmethod print-object ((object square) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (side position) object
      (format stream " of width ~d positionned from ~s" side position))))

(defmethod print-object ((object finder-pattern) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (location) object
      (if (slot-boundp object 'position)
	  (format stream "located in the ~a corner" location)
	  (format stream "floating without a location")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defgeneric equality (object-a object-b)
  (:documentation "Tests whether OBJECT-A and OBJECT-B have the same contents."))

(defmethod equality ((point-a point) (point-b point))
  (and (= (x point-a) (x point-b))
       (= (y point-a) (y point-b))))

(defgeneric add (point-a point-b)
  (:documentation "Vectorial sum of points."))

(defmethod add ((point-a point) (point-b point))
  (make-instance 'point
		 :x (+ (x point-a) (x point-b))
		 :y (+ (y point-a) (y point-b))))

(defgeneric subtract (point-a point-b)
  (:documentation "Vectorial difference of points."))

(defmethod subtract ((point-a point) (point-b point))
  (add point-a (make-instance 'point
			      :x (- (x point-b))
			      :y (- (y point-b)))))

(defgeneric multi (object scalar))

(defmethod multi ((object point) (scalar integer))
  (make-instance 'point
		 :x (* (x object) scalar)
		 :y (* (y object) scalar)))

(defmethod initialize-instance :after ((this-square square) &key)
  (let ((position (slot-value this-square 'position)))
    (when (listp position)
	    (destructuring-bind (x y) position
	      (setf (slot-value this-square 'position)
		    (make-instance 'point :x x :y y))))))

(defgeneric move-to (object point)
  (:documentation "Sets the position of OBJECT to POINT"))

(defmethod move-to ((object square) (point point))
  "Moves SQUARE to POINT."
  (with-slots (position) object
    (setf position point)))

(defmethod move-to ((object square) (point list))
  "Moves SQUARE to the coordinates designated by POINT."
  (destructuring-bind (x y) point
    (with-slots (position) object
      (setf position (make-instance 'point :x x :y y)))))

(defgeneric move-by (object point)
  (:documentation "Shift OBJECT by an offset given by POINT"))

(defmethod move-by ((object square) (point point))
  (with-slots (position) object
    (move-to object (add position point))))

(defmethod move-by ((object square) (point list))
  (destructuring-bind (x y) point
    (move-by object (make-instance 'point :x x :y y))))

(defgeneric set-position (object point))

(defgeneric set-location (location-designator object parent-version)
  (:documentation "Generic schmuck"))

(defmethod set-position ((object finder-pattern) (point point))
  (with-slots (content position) object
    (setf position point)
    (loop for shape in content do
	 (move-by shape point))))

(defmethod set-location ((location-designator symbol) (object finder-pattern)
			 parent-version)
  "Mian block"
  (declare (ignore parent-version))
  (with-slots (location) object
    (alexandria:eswitch (location-designator)
      ('top-left (setf location location-designator))
      ('bottom-left (setf location location-designator))
      ('top-right (setf location location-designator)))))

(defmethod set-location :after ((location-designator symbol) (object finder-pattern)
				parent-version)
  "After block"
  (with-slots (location) object
    (set-position object
	  (ecase location
	    (top-left (make-instance 'point :x 0 :y 0))
	    (bottom-left (make-instance 'point :y 0
					:x (+ (* 4 (1- parent-version)) 14)))
	    (top-right (make-instance 'point
				      :y (+ (* 4 (1- parent-version)) 14)
				      :x 0))))))

(defgeneric pixelize (object module-size &key canvas)
  (:documentation "Render OBJECT in pixels knowing that a module measures MODULE-SIZE pixels."))

(defmethod pixelize ((object square) module-size &key canvas)
  (with-slots (position side color) object
    (let ((pixel-side (* side module-size))
	  (pixel-pos (multi position module-size)))
      (loop for i from (x pixel-pos) below (+ pixel-side (x pixel-pos)) do
	   (loop for j from (y pixel-pos) below (+ pixel-side (y pixel-pos)) do
		(setf (aref canvas i j) (color-value color))))
      canvas)))

(defmethod pixelize ((object finder-pattern) module-size &key canvas)
  (with-slots (content) object
    (loop for shape in content
       for cv = (pixelize shape module-size :canvas canvas)
       then (pixelize shape module-size :canvas cv))
    canvas))

(defun color-value (color)
  (ecase color
    (white 1)
    (black 0)
    (t 2)))


(defmethod move-by ((object alignment-pattern) (point point))
  (with-slots (content) object
    (loop for shape in content do
	 (move-by shape point))))

(defmethod move-by ((object alignment-pattern) (point list))
  (with-slots (content) object
    (loop for shape in content do
	 (move-by shape point))))

(defmethod initialize-instance :after ((object qr-code) &key)
  (with-slots (size version content) object
    (setf size (qr-code-size version))
    (setf content
	  (loop for location in '(top-left top-right bottom-left)
	     for fp = (make-instance 'finder-pattern)
	     do (set-location location fp version)
	     collect fp))))

(defmethod pixelize ((object qr-code) module-size &key canvas)
  (with-slots (size content) object
    (let* ((pixel-side (* module-size size))
	   (canvas (or canvas (make-array (list pixel-side pixel-side)
					  :initial-element (color-value t)))))
      (loop for shape in content
	 for cv = (pixelize shape module-size :canvas canvas)
	 then (pixelize shape module-size :canvas cv))
    canvas)))
