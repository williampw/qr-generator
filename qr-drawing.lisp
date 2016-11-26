(in-package #:qr-generator)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 					   QR code class

;;; A `qr-code' object contains two lists of `pattern' objects: the `patterns' slot is a list of
;;; patterns defined by the specification (finder pattern, alignement pattern, etc.), the `modules'
;;; slot is a list of `dot' patterns representing the actual data modules.
;;; The position of all `pattern' objects is set in modules coordinates, not in pixels: the pixel
;;; coordinates are only determined at rendering time.

(defclass qr-code ()
  ((version :initarg :version
	    :initform (error "Must supply the version of the QR code."))
   (error-correction-mode :initarg :ec-mode
			  :accessor ec-mode
			  :documentation "Error corresction mode must be one of :Q :L :M :H.")
   (patterns :reader patterns
	     :documentation "Fixed patterns defined by the QR code specifications: finder, etc.")
   (modules :accessor modules
	    :initform nil
	    :documentation "Data modules that carry the actual data bits.")
   (size :reader size
	 :documentation "Number of modules on a side of the QR code.")
   (step-move :accessor step-move
	      :initform :side
	      :documentation "When moving in the QR code to add data modules, the next move will be
  this one. It can be either :side or :forward.")
   (progress-direction :accessor progress-direction
		       :initform :up
		       :documentation "When moving in the QR code to add data modules, this is the
  direction in which we progress. It can be either :up or :down.")
   (filling-point :accessor filling-point
		  :documentation "Point at which the next data module will be added.")
   (grid :accessor grid
	 :documentation "A representation of the QR code as an array. Bound by `write-data' once
  the QR code has been filled, grid is used to compute penalties for masks.")
   (mask :accessor mask
	 :documentation "The mask applied on the QR code. This information is kept to fill the
  format pattern.")))

(defmethod initialize-instance :after ((object qr-code) &key)
  (with-slots (size version filling-point patterns) object
    (setf size (qr-code-size version))
    (setf filling-point (make-instance 'point :x (1- size) :y (1- size)))
    (setf patterns (add-located-patterns 'finder-pattern '(top-left top-right bottom-left) version))
    (setf patterns
	  (append patterns
		  (add-alignment-patterns patterns version)
		  (add-located-patterns 'separator '(top-left top-right bottom-left) version)
		  (add-timing-patterns patterns size)
		  (add-located-patterns 'dark-module '(bottom-left) version)
		  (add-located-patterns 'format-pattern '(top-left top-right bottom-left) version)
		  (when (>= version 7)
		    (add-located-patterns 'version-pattern '(top-right bottom-left) version))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 			     Classes to represent geometrical objects

;;; Points represent positions, they have no dimensions

(defclass point ()
  ((x :initarg :x
      :initform (error "Must provide the x coordinate.")
      :accessor x
      :documentation "The x coordinate of the point.")   
   (y :initarg :y
      :initform (error "Must provide the y coordinate.")
      :accessor y
      :documentation "The y coordinate of the point.")))

;;; Squares are the basic shape making up patterns. Their position is designated by a `point'. They
;;; have a color and a side, so that they may be rendered.

(defclass square ()
  ((side :initarg :side
	 :initform (error "Must provide the side of the square")
	 :accessor side)   
   (position :initarg :pos
	     :initform (error "Must provide either a point or a 2-element list.")
	     :accessor pos
	     :documentation "Top left corner of the square.")
   (color :initarg :color
	  :initform 'white
	  :accessor color
	  :documentation "COLOR can be any symbol, but only BLACK and WHITE have a meaning.")))

;;; For convienience, this enables intialization from a list position instead of a `point'.
(defmethod initialize-instance :after ((object square) &key)
  "When OBJECT is instantiated, if :POS is provided as a list make it a POINT before storing."
  (with-slots (position) object
    (when (listp position)
      (destructuring-bind (x y) position
	(setf position (make-instance 'point :x x :y y))))))

;;; Many shapes can be described by concentric squares. This macro makes it easier to generate a
;;; list of `square' objects representing a concentric shape. 
(defmacro concentric-shape (&body specs)
  (let ((content)
	(parent-pos 0)
	(parent-side 0))
    (labels ((position-offset (side)
	       (+ parent-pos (/ (- parent-side side) 2)))
	     (spec->square (spec)
	       (destructuring-bind (&key pos side color) spec
		 (let ((new-pos (if (eql pos :deduce) (position-offset side) pos)))
		   (setf parent-side side
			 parent-pos new-pos)
		   `(make-instance 'square :side ,side :pos ',(list new-pos new-pos)
				   :color ',color)))))
      `(list ,@(dolist (spec specs (nreverse content))
		 (push (spec->square spec) content))))))

;;; Patterns contain shapes, but more importantly, they have a `position' and a `location'
;;; slots. Location is abstract and refers to a position determined by the QR code specification: a
;;; location may be bottom left, or top right. The position cannot be set when a `pattern' is
;;; instantiated most of the time: it depends on the version of the `qr-code' containing the
;;; `pattern'. Therefore it is the responsibility of the `qr-code' to call `set-location' when it
;;; instantiates its patterns.
;;; Some patterns'content may depend on their position: yhis is the case for separators for
;;; instance. The `define-located-pattern' macro is responsible for defining the class of located
;;; patterns, and generating the `set-location' methods.

(defclass pattern ()
  ((content :initarg :content
	    :accessor content
	    :documentation "List of shapes that make up the pattern.")
   (position :initform (make-instance 'point :x 0 :y 0)
	     :reader pos
	     :documentation "Real coordinates in the module space. Since it depends on
  the containing object's size, it should not be set manually. Instead, set the abstract `location'
  and let it set the position for you.")))

(defgeneric set-location (location-designator pattern parent-version)
  (:documentation "Set the location slot of PATTERN to LOCATION-DESIGNATOR and automatically move
  PATTERN to the position corresponding to LOCATION-DESIGNATOR."))

(defmacro define-located-pattern (class (&key (unique-content nil)) &body location-forms)
  "Define a pattern called CLASS whit a series of LOCATION-FORMS. Each location form should be
  (location-designator (x-position y-position) &optional content-at-location-designator)."
  (flet ((extract-locations (location-forms)
	   (loop for (location (x-loc y-loc) &optional content-form) in location-forms
	      collect location))
	 (form->point (location-form)
	   (destructuring-bind (location (x-loc y-loc) &optional content-form) location-form
	     (declare (ignore content-form))
	     `((,location) (make-instance 'point :x ,x-loc :y ,y-loc))))
	 (form->position (location-form)
	   (destructuring-bind (location (x-loc y-loc) &optional content-form) location-form
	     (declare (ignore x-loc y-loc))
	     (let ((content (or content-form unique-content)))
	       (if content
		   `((,location) (setf location location-designator
				       content ,content))
		   `((,location) (setf location location-designator)))))))
    `(progn

       (defclass ,class (pattern)
	 ((location :reader location
		    :documentation ,(format nil "Location on the QR code. Should be set by
  providing either of ~a to `set-location', which will take care of updating `position' to the
  appropriate coordinates." (extract-locations location-forms)))))
       
       (defmethod set-location ((location-designator symbol) (object ,class) parent-version)
	 (declare (ignorable parent-version))
	 (with-slots (content location) object
	   (ecase location-designator
	     ,@(loop for location-form in location-forms collect
		    (form->position location-form)))))
       
       (defmethod set-location :after ((location-designator symbol) (object ,class) parent-version)
		  (with-slots (location) object
		    (set-position object
				  (ecase location
				    ,@(loop for location-form in location-forms collect
					   (form->point location-form)))))))))

(define-located-pattern separator ()
  (top-left (0 0)
	    (append (loop for x below 8 with y = 7 collect
			 (make-instance 'square :side 1 :pos (list x y) :color 'white))
		    (loop for y below 7 with x = 7 collect
			 (make-instance 'square :side 1 :pos (list x y) :color 'white))))
  (top-right (0 (+ (* 4 (1- parent-version)) 14))
	     (append (loop for x below 8 with y = -1 collect
			  (make-instance 'square :side 1 :pos (list x y) :color 'white))
		     (loop for y below 7 with x = 7 collect
			  (make-instance 'square :side 1 :pos (list x y) :color 'white))))
  (bottom-left ((+ (* 4 (1- parent-version)) 14) 0)
	       (append (loop for x from -1 below 7 with y = 7 collect
			    (make-instance 'square :side 1 :pos (list x y) :color 'white))
		       (loop for y below 7 with x = -1 collect
			    (make-instance 'square :side 1 :pos (list x y) :color 'white)))))

(define-located-pattern format-pattern ()
  (top-left (0 0)
	    (append (loop for x below 9 with y = 8 unless (= x 6)
		       collect (make-instance 'square :side 1 :pos (list x y)))
		    (loop for y downfrom 7 to 0 with x = 8 unless (= y 6)
		       collect (make-instance 'square :side 1 :pos (list x y)))))
  (top-right (8 (+ (* 4 (1- parent-version)) 13))
	     (loop with x = 0 for y downfrom 7 to 0
		collect (make-instance 'square :side 1 :pos (list x y))))
  (bottom-left ((+ (* 4 (1- parent-version)) 14) 8)
	       (loop for x upfrom 0 to 6 with y = 0
		  collect (make-instance 'square :side 1 :pos (list x y)))))

(define-located-pattern version-pattern ()
  (bottom-left ((+ (* 4 (1- parent-version)) 10) 0)
	       (loop for y below 6 append
			  (loop for x below 3
			     collect (make-instance 'square :side 1 :pos (list x y)))))
  (top-right (0 (+ (* 4 (1- parent-version)) 10))
	     (loop for x below 6 append
			  (loop for y below 3
			     collect (make-instance 'square :side 1 :pos (list x y))))))

(define-located-pattern finder-pattern
    (:unique-content
     (concentric-shape
       (:side 7 :color black :pos 0)
       (:side 5 :color white :pos :deduce)
       (:side 3 :color black :pos :deduce)))
  (top-left (0 0))
  (top-right (0 (+ (* 4 (1- parent-version)) 14)))
  (bottom-left ((+ (* 4 (1- parent-version)) 14) 0)))

(defclass alignment-pattern (pattern)
  ((content :initform 
	    (concentric-shape
	     (:side 5 :color black :pos -2)
	     (:side 3 :color white :pos :deduce)
	     (:side 1 :color black :pos :deduce)))))

;;; Timing patterns are defined by the alternance of black and white. The vertical one is treated
;;; differently from the horizontal one, therefore we have two very similar classes.

(defclass horizontal-timing-pattern (pattern)
  ((generator :reader generator
	      :initform (lambda (parent-size)
			  (loop with x = 6 for y below parent-size
			     for color in (alexandria:circular-list 'black 'white)
			     collect (make-instance 'square :pos (list x y)
						    :color color :side 1))))))

(defclass vertical-timing-pattern (pattern)
  ((generator :reader generator
	      :initform (lambda (parent-size)
			  (loop with y = 6 for x below parent-size			      
			      for color in (alexandria:circular-list 'black 'white)
			      collect (make-instance 'square :pos (list x y)
						     :color color :side 1))))))

(define-located-pattern dark-module
    (:unique-content (list (make-instance 'square :pos '(0 0) :side 1 :color 'black)))
  (bottom-left ((+ (* 4 parent-version) 9) 8)))

;;; The dot is a basic module used to represent the data held by the QR code. Since its color
;;; varies, unlike the other patterns, it has a color clot. A specialized setter takes care of
;;; updating the square in the dot, whenever the dot's color changes

(defclass dot (pattern)
  ((content :initform (list (make-instance 'square :side 1 :pos '(0 0) :color 'unknown)))
   (color :initarg :color
	  :reader color)))

(defmethod initialize-instance :after ((object dot) &key)
  (with-slots (content color position) object
    (setf (color (first content)) color)))

(defgeneric (setf color) (value object))

(defmethod (setf color) (value (object dot))
  (with-slots (content color) object
    (setf color value
	  (color (first content)) value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 					  Moving patterns

;;; Whenever a pattern's location is set, its position can be determined and the pattern is
;;; reponsible for moving the shapes in its content slot.

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 		       Printing functions, to make object easier to inspect

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
;;; 				  Arithemtic operations on points

(defgeneric add (point-a point-b)
  (:documentation "Vectorial sum of points."))

(defmethod add ((point-a point) (point-b point))
  (make-instance 'point
		 :x (+ (x point-a) (x point-b))
		 :y (+ (y point-a) (y point-b))))

(defmethod add ((point-a point) (point-b list))
  (destructuring-bind (x-b y-b) point-b
    (make-instance 'point
		   :x (+ (x point-a) x-b)
		   :y (+ (y point-a) y-b))))

(defgeneric multi (object scalar))

(defmethod multi ((object point) (scalar integer))
  (make-instance 'point
		 :x (* (x object) scalar)
		 :y (* (y object) scalar)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 				    Moving and comparing shapes

(defgeneric move-to (shape point)
  (:documentation "Sets the position of SHAPE to POINT"))

(defmethod move-to ((shape point) (destination point))
  (with-slots (x y) destination
    (setf (x shape) x
	  (y shape) y)
    shape))

(defmethod move-to ((shape point) (destination list))
  (destructuring-bind (x y) destination
    (setf (x shape) x
	  (y shape) y)
    shape))

(defmethod move-to ((shape square) (destination point))
  "Moves SQUARE to POINT."
  (with-slots (position) shape
    (move-to position destination)
    shape))

(defmethod move-to ((shape square) (destination list))
  "Moves SQUARE to the coordinates designated by POINT."
  (with-slots (position) shape
    (move-to position destination)))

(defgeneric move-by (shape point)
  (:documentation "Shift SHAPE by an offset given by POINT"))

(defmethod move-by ((shape point) (offset point))
  (move-to shape (add shape offset)))

(defmethod move-by ((shape point) (offset list))
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

(defun extent (square)
  (with-slots (side position) square
    (with-slots (x y) position
      (list x (+ x side) y (+ y side)))))

(defun between-values (x min max &key (test-min #'<=) (test-max #'<))
  (and (funcall test-min min x) (funcall test-max x max)))

(defgeneric overlap-p (object-1 object-2)
  (:documentation "Compare the extents of OBJECT-1 and OBJECT-2 and return t if there is any
  overlap."))

(defmethod overlap-p ((shape square) (point point))
  (destructuring-bind (xmin xmax ymin ymax) (extent shape)
    (and (between-values (x point) xmin xmax)
	 (between-values (y point) ymin ymax))))

(defmethod overlap-p ((shape-1 square) (shape-2 square))
  (destructuring-bind (xmin-1 xmax-1 ymin-1 ymax-1) (extent shape-1)
    (destructuring-bind (xmin-2 xmax-2 ymin-2 ymax-2) (extent shape-2)
      (and (or (between-values xmin-1 xmin-2 xmax-2)
	       (between-values xmin-2 xmin-1 xmax-1))
	   (or (between-values ymin-1 ymin-2 ymax-2)
	       (between-values ymin-2 ymin-1 ymax-1))))))

(defmethod overlap-p ((object pattern) (point point))
  (some (lambda (shape) (overlap-p shape point))
	(content object)))

(defmethod overlap-p ((object-1 pattern) (object-2 square))
  (some (lambda (shape) (overlap-p shape object-2))
	(content object-1)))

(defmethod overlap-p ((object-1 pattern) (object-2 pattern))
  (some (lambda (shape) (overlap-p object-2 shape))
	(content object-1)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 				   QR code buiding happens here

(defun qr-code-size (version)
  "Size, ie number of module on a side, of a QR code of version VERSION."
  (+ (* 4 (1- version)) 21))

(defun alignment-pattern-positions (version)
  "All the possible positions for the centers of the alignment patterns, for a certain Qr code
VERSION"
  (let ((grid (aref *alignment-pattern-centers* (1- version))))
    (alexandria:map-product #'list  grid grid)))


;;; All located patterns, defined thanks to the `define-located-pattern' macro, can be instantiated
;;; and placed correctly by the same function. All one needs to know is the list of the
;;; locations. I'd rather put that in a class-acllocated slot, but it wouldn't be reachable before
;;; instantiating. 

(defun add-located-patterns (pattern-type locations version)
  (loop for location in locations
     for object = (make-instance pattern-type)
     do (set-location location object version)
     collect object))

;;; Alignment patterns and timing patterns need their own function, since they are not located
;;; patterns.

(defun add-alignment-patterns (content version)
  (flet ((overlaps-with-finders (pattern)
	   (loop for finder in (subseq content 0 3)
	      thereis (overlap-p finder pattern))))
    (loop for ali = (make-instance 'alignment-pattern)
       for possible-pos in (alignment-pattern-positions version)
       do (set-position ali possible-pos)
       unless (overlaps-with-finders ali)
       collect ali)))

(defun add-timing-patterns (patterns size)
  (flet ((overlaps-with-finders (shape)
	   (loop for pattern in (subseq patterns 0 3)
	      thereis (overlap-p pattern shape))))
    (let ((horizontal-timing-pattern (make-instance 'horizontal-timing-pattern))
	  (vertical-timing-pattern (make-instance 'vertical-timing-pattern)))
      (with-slots (generator) horizontal-timing-pattern
	(setf (content horizontal-timing-pattern)
	      (remove-if #'overlaps-with-finders (funcall generator size))))
      (with-slots (generator) vertical-timing-pattern
	(setf (content vertical-timing-pattern)
	      (remove-if #'overlaps-with-finders (funcall generator size))))
      (list horizontal-timing-pattern vertical-timing-pattern))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 		    Rendering functions : to turn things into beautiful pixels

(defgeneric pixelize (object module-size &key canvas)
  (:documentation "Render OBJECT in pixels knowing that a module measures MODULE-SIZE pixels."))

(defmethod pixelize ((object square) module-size &key canvas)
  (with-slots (position side color) object
    (let ((pixel-side (* side module-size))
	  (pixel-pos (multi position module-size)))
      (loop for i from (x pixel-pos) below (+ pixel-side (x pixel-pos)) do
	   (loop for j from (y pixel-pos) below (+ pixel-side (y pixel-pos)) do
		(setf (aref canvas i j) (color-value color)))))))

(defmethod pixelize ((object pattern) module-size &key canvas)
  (with-slots (content) object
    (dolist (shape content)
      (pixelize shape module-size :canvas canvas))))

(defmethod pixelize ((object qr-code) module-size &key canvas)
  (with-slots (size patterns modules) object
    (let* ((pixel-side (* module-size size))
	   (canvas (or canvas
		       (make-array (list pixel-side pixel-side)
				   :initial-element (color-value t)
				   :element-type '(unsigned-byte 8)))))
      (dolist (pattern patterns)
	(pixelize pattern module-size :canvas canvas))
      (dolist (module modules)
	(pixelize module module-size :canvas canvas))
      canvas)))

(defun color-value (color)
  (ecase color
    ((white) 255)
    ((black) 0)
    (t 127)))

(defun print-to-file (qr-code module-size file)
  (with-open-file (out file :direction :output :if-exists :supersede)
    (let* ((px (pixelize qr-code module-size))
	   (rows-cols (array-dimensions px)))
      (loop for row below (first rows-cols)
	 do (loop for col below (second rows-cols) do
		 (format out "~d" (aref px row col)))
	   (format out "~%")))))

(defun print-to-png (qr-code module-size file)
  "Saves QR-CODE to a png FILE after rendering, knowing that a module measures MODULE-SIZE pixels."
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-step (point move direction)
  (declare (ignorable direction))
  (let ((offset (ecase move
		  ((:side) '(0 -1))
		  ((:forward)
		   (ecase direction
		     ((:up) '(-1 1))
		     ((:down) '(1 1)))))))
    (add point offset)))

(defun ensure-step (qr-code point move direction)
  (let ((new-point (make-step point move direction)))
    (restart-case (check-inside qr-code new-point)
      (step-again (new-move new-direction) (ensure-step qr-code new-point new-move new-direction))
      (double-side-step () (make-step point :side direction)))))

(defun check-pattern-overlap (qr-code point)
  (find t (patterns qr-code) :key (lambda (pattern) (overlap-p pattern point))))

(defun flip-direction (direction)
  (if (eql direction :up) :down :up))

(defun next-step-move (step-move)
  (if (eql step-move :forward) :side :forward))

(defun progress (qr-code)
  (with-slots (filling-point step-move progress-direction) qr-code
    (handler-bind
	((out-of-bounds-error
	  (lambda (c)
	    (declare (ignore c))
	    (setf progress-direction (flip-direction progress-direction))
	    (setf filling-point
		  (invoke-restart 'double-side-step)))))
      (let* ((next-point (ensure-step qr-code filling-point step-move progress-direction))
	     (overlapping-pattern (check-pattern-overlap qr-code next-point)))
	(setf filling-point next-point)
	(setf step-move (next-step-move step-move))
	(cond
	  ((and overlapping-pattern (eql (type-of overlapping-pattern) 'vertical-timing-pattern))
	   (prog1 (progress qr-code)
	     (setf step-move :side)))
	  (overlapping-pattern (progress qr-code))
	  (t filling-point))))))

(defgeneric check-inside (qr-code point)
  (:documentation "Is POINT inside the QR-CODE? If so, return POINT."))

(defmethod check-inside ((qr-code qr-code) (point point))
  (cond
    ((or (minusp (x point))
	 (>= (x point) (size qr-code)))
     (error 'out-of-bounds-error))
    ((minusp (y point)) (error "Beyond QR code capacity!"))
    (t point)))

(defmethod check-inside ((qr-code qr-code) (point list))
  (destructuring-bind (x y) point
    (when (minusp y)
      (error "Too much data."))
    (if (or (minusp x) (>= x (size qr-code)))
	(error 'out-of-bounds-error)      
	point)))

(define-condition out-of-bounds-error (error)
  ((text :initform "Below the lower limit or above the upper limit of the QR code")))

(defun write-module (qr-code bit)
  (let ((data-module (make-instance 'dot :color (if (zerop bit) 'white 'black))))
    (set-position data-module (filling-point qr-code) :relative nil)
    (push data-module (modules qr-code)))
  (progress qr-code))

(defun write-data (qr-code bit-string)
  (dolist (bit (mapcar #'parse-integer (chunk bit-string 1)))
    (write-module qr-code bit))
  (setf (grid qq) (pixelize qq 1)))

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
				  (mod (* (x point) (y point)) 3))))))

(defun flip-color (module)
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

(defun find-right-mask (qr-code)
  (let ((mask-scores (mapcar (lambda (mask) (penalty (apply-mask qr-code mask))) *masks*)))
    (position (apply #'min mask-scores) mask-scores)))

(defun enforce-mask (qr-code mask)
  (setf (mask qr-code) mask)
  (dolist (module (modules qr-code))
    (when (funcall (nth mask *masks*) (pos module))
      (flip-color module)))
  (setf (grid qq) (pixelize qq 1)))

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
are the  same as in the first cell."
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
  (destructuring-bind (nrows ncols) (array-dimensions array)
    (loop for row upfrom 0 below nrows sum
	 (do ((col 0 (+ col (greedy-contiguous array row col)))
	      (contig-score 0))
	     ((>= col ncols) contig-score)
	   (incf contig-score
		 (score-contiguous (greedy-contiguous array row col)))))))

(defun penalty-1 (array)
  (+ (count-contiguous array) (count-contiguous (transpose array))))

(defun penalty-2 (array)
  (* 3
     (+ (match-pattern array '((0 0) (0 0)))
	(match-pattern array '((255 255) (255 255))))))

(defun transpose (array)
  (array-operations:permute '(1 0) array))

(defun penalty-3 (array)
  (let* ((base-pattern '(255 0 255 255 255 0 255 0 0 0 0))
	 (pattern (list base-pattern))
	 (anti-pattern (list (reverse base-pattern))))
    (* 40
       (+ (match-pattern array pattern)
	  (match-pattern array anti-pattern)
	  (match-pattern (transpose array) pattern)
	  (match-pattern (transpose array) anti-pattern)))))

(defun count-array (array item)
  (count item (array-operations:flatten array)))

(defun penalty-4 (array)
  (let* ((dark-modules (count-array array 0))
	 (total-modules (apply #'* (array-dimensions array)))
	 (dark-percentage (* 100 (/ dark-modules total-modules)))
	 (sub-multiple (* 5 (floor dark-percentage 5)))
	 (sup-multiple (+ 5 sub-multiple))
	 (final-values (list (/ (abs (- sub-multiple 50)) 5)
			     (/ (abs (- sup-multiple 50)) 5))))
    (reduce #'min final-values :key (lambda (x) (* 10 x)))))

(defun penalty (array)
  (+ (penalty-1 array) (penalty-2 array) (penalty-3 array) (penalty-4 array)))

(defun fill-format-pattern (format-pattern format-string)
  (let ((bits (mapcar #'parse-integer (chunk format-string 1))))
    (ecase (location format-pattern)
      ((top-left top-right)
       (loop for module in (content format-pattern)
	  for bit in bits
	  do (setf (color module) (if (zerop bit) 'white 'black))))
      ((bottom-left)
       (loop for module in (content format-pattern)
	  for bit in (subseq bits 8)
	    do (setf (color module) (if (zerop bit) 'white 'black)))))))

(defun fill-format-patterns (qr-code)
  (with-slots (mask error-correction-mode) qr-code
    (let ((format-string "010111011011010";; (format-string error-correction-mode mask)
			 )
	  (format-patterns (remove-if-not (lambda (obj) (eql (type-of obj) 'format-pattern))
					  (patterns qr-code))))
      (dolist (fp format-patterns)
	(fill-format-pattern fp (reverse format-string))))))
