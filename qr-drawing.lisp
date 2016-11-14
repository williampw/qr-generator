(in-package #:qr-generator)

(defun qr-code-size (version)
  (+ (* 4 (1- version)) 21))

(defun alignment-pattern-positions (version)
  (let ((grid (aref *alignment-pattern-centers* (1- version))))
    (alexandria:map-product (lambda (x y) (make-instance 'point :x x :y y)) grid grid)))

(defun between-values (x min max &key (test-min #'<=) (test-max #'<))
  (and (funcall test-min min x) (funcall test-max x max)))

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
	     :accessor pos
	     :documentation "Position can be supplied either as a POINT or a list of 2 coordinates")
   
   (color :initarg :color
	  :initform 'white
	  :accessor color
	  :documentation "COLOR can be any symbol, but only BLACK and WHITE have a meaning.")))

(defmethod initialize-instance :after ((object square) &key)
  "When OBJECT is instantiated, if :POS is provided as a list make it a POINT before storing."
  (with-slots (position) object
    (when (listp position)
      (destructuring-bind (x y) position
	(setf position (make-instance 'point :x x :y y))))))

(defclass pattern ()
  ((content :initarg :content
	    :accessor content
	    :documentation "List of shapes that make up the pattern.")
   (position :initform (make-instance 'point :x 0 :y 0)
	     :reader pos)))

(defclass finder-pattern (pattern)
  ((content :initform (list (make-instance 'square :side 7 :pos '(0 0) :color 'black)
			    (make-instance 'square :side 5 :pos '(1 1) :color 'white)
			    (make-instance 'square :side 3 :pos '(2 2) :color 'black)))
   (position :documentation "Real coordinates in the geometrical space. Since it depends on
  the containing object's size, it should not be set manually. Instead, set the abstract `location'
  and let it set the position for you.")   
   (location :reader loc
	     :documentation "Location on the QR code. Should be set by providing either of
  ('top-left 'top-right 'bottom-left) to `set-location', which will take care of updating
  `position' to the appropriate coordinates.")))

(defclass alignment-pattern (pattern)
  ((content :initform (list (make-instance 'square :side 5 :pos '(-2 -2) :color 'black)
			    (make-instance 'square :side 3 :pos '(-1 -1) :color 'white)
			    (make-instance 'square :side 1 :pos '(0 0) :color 'black)))))

(defclass separator (pattern)
  ((location :initarg :location)))

(defclass timing-pattern (pattern)
  ((generator :reader row-generator
	      :initform (lambda (parent-size)
			  (append
			   (loop with x = 6 for y below parent-size
			      for color in (alexandria:circular-list 'black 'white)
			      collect (make-instance 'square :pos (list x y)
						     :color color :side 1))
			   (loop with y = 6 for x below parent-size			      
			      for color in (alexandria:circular-list 'black 'white)
			      collect (make-instance 'square :pos (list x y)
						     :color color :side 1)))))))

(defclass dot (pattern)
  ((content :initform (list (make-instance 'square :side 1 :pos '(0 0) :color 'unknown)))
   (color :initarg :color)))

(defmethod initialize-instance :after ((object dot) &key)
  (with-slots (content color position) object
    (setf (color (first content)) color)))

(defclass dark-module (dot)
  ((color :initform 'black)))

(defclass qr-code ()
  ((version :initarg :version
	    :initform (error "Must supply the version of the QR code."))
   (content :initarg :content
	    :reader content)
   (size :reader size)))

(defmethod initialize-instance :after ((object qr-code) &key)
  (with-slots (size version content) object
    (setf size (qr-code-size version))
    (setf content (add-finder-patterns version))
    (setf content (append content
			  (add-alignment-patterns content version)
			  (add-separators version)
			  (add-timing-patterns content size)
			  (add-dark-module version)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Printing functions, to make object easier to inspect

(defmethod print-object ((object point) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (x y) object
      (format stream "located at (~d; ~d)" x y))))

(defmethod print-object ((object square) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (side position color) object
      (format stream " ~a of width ~d positionned from ~s" color side position))))

(defmethod print-object ((object finder-pattern) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (location) object
      (if (slot-boundp object 'position)
	  (format stream "located in the ~a corner" location)
	  (format stream "floating without a location")))))

(defmethod print-object ((object dot) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (color position) object
      (format stream "~a positionned from ~s" color position))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Arithemtic operations

(defgeneric equality (object-a object-b)
  (:documentation "Tests whether OBJECT-A and OBJECT-B have the same contents."))

(defmethod equality ((point-a point) (point-b point))
  (and (= (x point-a) (x point-b))
       (= (y point-a) (y point-b))))

(defmethod equality ((sqa square) (sqb square))
  (and (equality (pos sqa) (pos sqb))
       (eql (color sqa) (color sqb))
       (= (side sqa) (side sqb))))

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Objects in the geometrical space

(defgeneric move-to (shape point)
  (:documentation "Sets the position of SHAPE to POINT"))

(defgeneric move-by (shape point)
  (:documentation "Shift SHAPE by an offset given by POINT"))

(defgeneric extent (object))

(defgeneric overlap-p (object-1 object-2)
  (:documentation "Compare the extents of OBJECT-1 and OBJECT-2 and return t if there is any
  overlap."))

(defmethod move-to ((shape point) (destination point))
  (with-slots (x y) destination
    (setf (x shape) x
	  (y shape) y)))

(defmethod move-to ((shape point) (destination list))
  (destructuring-bind (x y) destination
    (setf (x shape) x
	  (y shape) y)))

(defmethod move-to ((shape square) (destination point))
  "Moves SQUARE to POINT."
  (with-slots (position) shape
    (move-to position destination)
    shape))

(defmethod move-to ((shape square) (destination list))
  "Moves SQUARE to the coordinates designated by POINT."
  (with-slots (position) shape
    (move-to position destination)))

(defmethod move-by ((shape point) (offset point))
  (move-to shape (add shape offset)))

(defmethod move-by ((shape square) (offset point))
  "If a POINT is supplied as offset to SHAPE, it is only a matter of setting the new position to
the sum of POINT and the SHAPE's current position."
  (with-slots (position) shape
    (move-to shape (add position offset))))

(defmethod move-by ((shape square) (offset list))
  "If a `list' is supplied as offset to SHAPE, turn it into a `point' and refer to the method
specialized on `point'."
  (with-slots (position) shape
    (move-by position offset)))

(defmethod extent ((shape point))
  (with-slots (x y) shape
    (list x x y y)))

(defmethod extent ((shape square))
  (with-slots (side position) shape
    (with-slots (x y) position
      (list x (+ x side) y (+ y side)))))

(defmethod extent ((shape pattern))
  (with-slots (content) shape
    (loop for sub-shape in content
       for (xmin xmax ymin ymax) = (extent sub-shape)
       minimizing xmin into x-low
       maximizing xmax into x-high
       minimizing ymin into y-low
       maximizing ymax into y-high
       finally (return (list x-low x-high y-low y-high)))))

(defmethod overlap-p ((object-1 point) (object-2 point))
  "The only way two points can overlap is if they are the same point."
  (equality object-1 object-2))

(defmethod overlap-p ((object-1 square) (object-2 square))
  (destructuring-bind (xmin-1 xmax-1 ymin-1 ymax-1) (extent object-1)
    (destructuring-bind (xmin-2 xmax-2 ymin-2 ymax-2) (extent object-2)
      (and (or (between-values xmin-1 xmin-2 xmax-2)
	       (between-values xmin-2 xmin-1 xmax-1))
	   (or (between-values ymin-1 ymin-2 ymax-2)
	       (between-values ymin-2 ymin-1 ymax-1))))))

(defmethod overlap-p ((object-1 pattern) (object-2 square))
  (some (lambda (shape) (overlap-p shape object-2))
	  (content object-1)))

(defmethod overlap-p ((object-1 pattern) (object-2 pattern))
  (some (lambda (shape) (overlap-p object-2 shape))
	  (content object-1)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setting positions of patterns

(defgeneric set-position (object point &key relative)
  (:documentation "Set the `position' of OBJECT to POINT. Since this often requires knowledge about
  the containing object, it is not done manually but after the abstract `location' of OBJECT has
  been set."))

(defmethod set-position ((object pattern) point &key (relative t))
  (let ((moving-func (if relative #'move-by #'move-to)))
   (with-slots (content position) object
     (funcall moving-func position point)
     (loop for shape in content do
	  (funcall moving-func shape point))
     object)))

;; (defmethod set-position ((object pattern) (point list) &key (relative t))
;;   (assert (= (length point) 2))
;;   (destructuring-bind (x y) point
;;     (set-position object (make-instance 'point :x x :y y) :relative relative)))

(defgeneric set-location (location-designator object parent-version)
  (:documentation "Generic schmuck"))

(defmethod set-location ((location-designator symbol) (object finder-pattern)
			 parent-version)
  "Mian block"
  (declare (ignore parent-version))
  (with-slots (location) object
    (ecase location-designator
      ((top-left) (setf location location-designator))
      ((bottom-left) (setf location location-designator))
      ((top-right) (setf location location-designator)))))

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

(defmethod set-location ((location-designator symbol) (object separator) parent-version)
  (declare (ignore parent-version))
  (format t "i'm here!")
  (with-slots (content location) object
    (ecase location-designator
      ((top-left)
       (setf location location-designator
	     content
	     (append (loop for x below 8 with y = 7 collect
			  (make-instance 'square :side 1 :pos (list x y) :color 'white))
		     (loop for y below 7 with x = 7 collect
			  (make-instance 'square :side 1 :pos (list x y) :color 'white)))))
      ((top-right)
       (setf location location-designator
	     content
	     (append (loop for x below 8 with y = -1 collect
			  (make-instance 'square :side 1 :pos (list x y) :color 'white))
		     (loop for y below 7 with x = 7 collect
			  (make-instance 'square :side 1 :pos (list x y) :color 'white)))))
      ((bottom-left)
       (setf location location-designator
	     content
	     (append (loop for x from -1 below 7 with y = 7 collect
			  (make-instance 'square :side 1 :pos (list x y) :color 'white))
		     (loop for y below 7 with x = -1 collect
			  (make-instance 'square :side 1 :pos (list x y) :color 'white))))))))

(defmethod set-location :after ((location-designator symbol) (object separator) parent-version)
  (with-slots (location) object
    (set-position object
	  (ecase location
	    (top-left (make-instance 'point :x 0 :y 0))
	    (bottom-left (make-instance 'point :y 0
					:x (+ (* 4 (1- parent-version)) 14)))
	    (top-right (make-instance 'point
				      :y (+ (* 4 (1- parent-version)) 14)
				      :x 0))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; QR code buiding happens here

(defun add-finder-patterns (version)
  (loop for location in '(top-left top-right bottom-left)
     for fp = (make-instance 'finder-pattern)
     do (set-location location fp version)
     collect fp))

(defun add-alignment-patterns (content version)
  (flet ((overlaps-with-finders (pattern)
	   (loop for finder in (subseq content 0 3)
	      thereis (overlap-p finder pattern))))
    (loop for ali = (make-instance 'alignment-pattern)
       for possible-pos in (alignment-pattern-positions version)
       do (set-position ali possible-pos)
       unless (overlaps-with-finders ali)
       collect ali)))

(defun add-separators (version)
  (loop for location in '(top-left top-right bottom-left)
     for sp = (make-instance 'separator)
     do (set-location location sp version)
     collect sp))

(defun add-timing-patterns (content size)
  (flet ((overlaps-with-finders (shape)
	   (loop for pattern in (subseq content 0 3)
		thereis (overlap-p pattern shape))))
   (let ((timing-pattern (make-instance 'timing-pattern)))
     (with-slots (generator) timing-pattern
       (setf (content timing-pattern)
	     (remove-if #'overlaps-with-finders (funcall generator size))))
     (list timing-pattern))))

(defun add-dark-module (version)
  (let ((dark-module (make-instance 'dark-module)))
    (set-position dark-module (make-instance 'point :x (+ (* 4 version) 9) :y 8))
    (list dark-module)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rendering functions : to turn things into beautiful pixels

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

(defmethod pixelize ((object pattern) module-size &key canvas)
  (with-slots (content) object
    (loop for shape in content
       for cv = (pixelize shape module-size :canvas canvas)
       then (pixelize shape module-size :canvas cv))
    canvas))

(defun color-value (color)
  (ecase color
    (white 255)
    (black 0)
    (t 127)))


(defmethod pixelize ((object qr-code) module-size &key canvas)
  (with-slots (size content) object
    (let* ((pixel-side (* module-size size))
	   (canvas (or canvas (make-array (list pixel-side pixel-side)
					  :initial-element (color-value t)
					  :element-type '(unsigned-byte 8)))))
      (loop for shape in content
	 for cv = (pixelize shape module-size :canvas canvas)
	 then (pixelize shape module-size :canvas cv))
    canvas)))


(defun print-to-file (qr-code module-size file)
  (with-open-file (out file :direction :output :if-exists :supersede)
    (let* ((px (pixelize qr-code module-size))
	   (rows-cols (array-dimensions px)))
      (loop for row below (first rows-cols)
		do (loop for col below (second rows-cols) do
			(format out "~d" (aref px row col)))
		  (format out "~%")))))

(defun print-to-png (qr-code module-size file)
  (let* ((png-size (* (size qr-code) module-size))
	 (px (make-array (* png-size png-size)
			 :element-type '(unsigned-byte 8)
			 :initial-contents (array-operations:flatten
					    (pixelize qr-code module-size))))
	 (png (make-instance 'zpng:png
                             :color-type :grayscale
                             :width png-size
                             :height png-size
			     :image-data px)))    
    (zpng:write-png png file)))

(defun step-side (dot &key direction)
  (declare (ignorable direction))
  (let ((offset (make-instance 'point :x 0 :y -1)))
    (set-position dot offset)))

(defun step-forward (dot &key direction)
  (let ((offset (make-instance 'point :x (ecase  direction ((:up) -1) (:down 1)) :y 0)))
    (set-position dot offset)))

(defun progress (dot steps &key direction)
  (dotimes (i steps dot)
    (if (evenp i)
	(step-side dot :direction direction)
	(step-forward dot :direction direction))))
