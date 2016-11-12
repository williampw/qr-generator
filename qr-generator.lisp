;;;; qr-generator.lisp

(in-package #:qr-generator)

(defun assocval (item alist)
  (cdr (assoc item alist))) 

(defun numeric-mode-p (char)
  (digit-char-p char))

(defun alphanumeric-mode-p (char)
  (let ((alphabet (map 'list #'character "ABCDEFGHIJKLMNOPQRSTUVWXYZ $%*+-./")))
    (or (numeric-mode-p char)
	(member char alphabet))))

(defun acceptable-encoding-modes (string &optional (auto-upcase-string nil))
  "Checks the string agains all encoding modes and returns an alist with the encoding modes as keys and their suitability as values."
  (let* ((char-sequence (map 'list #'character (if auto-upcase-string
						   (string-upcase string)
						   string)))
	 (numeric-mode (every #'numeric-mode-p char-sequence))
	 (alphanumeric-mode (every #'alphanumeric-mode-p char-sequence))
	 (byte-mode (not alphanumeric-mode)))
    (list (cons :numeric-mode numeric-mode)
	  (cons :alphanumeric-mode alphanumeric-mode)
	  (cons :byte-mode byte-mode))))

(defun find-encoding-mode (string)
  "Determines the appropriate encoding mode for string."
  (loop with modes = (acceptable-encoding-modes string)
     for try-mode in *encoding-modes*
     for (this-mode . possible) = (assoc try-mode modes) do
       (if possible
	   (return this-mode))))

(defun choose-error-correction (level)
  (assert (= 1 (length level)))
  (cond ((string-equal level "L") :L)
	((string-equal level "M") :M)
	((string-equal level "Q") :Q)
	((string-equal level "H") :H)
	(t (error "Unrecognized error correction level. Must be one of (L M Q H)"))))

(defun encoding-mode-indicator (encoding-mode)
  (let ((encoding-mode-indicator (assocval encoding-mode *encoding-mode-indicators*)))
    (padded-binary encoding-mode-indicator 4)))

(defun get-qr-version (string-length error-correction-mode encoding-mode)
  (loop for version from 1 upto 40
     for version-level = (assocval version *character-capacities*)
     for correction-level = (assocval error-correction-mode version-level)
     for max-chars = (assocval encoding-mode correction-level) do
       (when (> max-chars string-length)
	   (return version))))

(defun get-qr-version-from-string (string error-correction-mode)
  (let ((length (length string))
	(encoding-mode (find-encoding-mode string)))
    (get-qr-version length error-correction-mode encoding-mode)))

(defun padded-binary (decimal-number width)
  (format nil "~v,'0b" width decimal-number))

(defun character-count-indicator-length (version encoding-mode)
  (let ((version-table (assocval version *count-indicator-length*)))
    (assocval encoding-mode version-table)))

(defun character-count-indicator (string-length version encoding-mode)
  (padded-binary string-length
		 (character-count-indicator-length version encoding-mode)))

(defun split-string-into-blocks (string block-size)
  (loop with string-length = (length string)
     for i = 0 then (+ i block-size)
       while (< i (- string-length block-size))
       for substring = (subseq string i (+ i block-size))
       collect substring into split
       finally
       (return (nconc split (list (subseq string i))))))

(defun split-integer-to-blocks (integer &key (block-size 3))
  (let* ((string-data (format nil "~d" integer))
	 (split-string (split-string-into-blocks string-data block-size)))
    (mapcar #'parse-integer split-string)))

(defun represent-substring (substring)
  (ecase (length substring)
    (1 (padded-binary (assocval (char substring 0) *alphanumeric-encoding*)
		      6))
    (2 (padded-binary (+ (* 45 (assocval (char substring 0) *alphanumeric-encoding*))
			 (assocval (char substring 1) *alphanumeric-encoding*))
		      11))))

(defun string-to-message (string correction-mode)
  (let* ((length (length string))
	 (encoding-mode (find-encoding-mode string))
	 (error-correction-mode (choose-error-correction correction-mode))
	 (version (get-qr-version-from-string string error-correction-mode))
	 (capacity (qr-capacity version error-correction-mode))
	 (mode-indicator (encoding-mode-indicator encoding-mode))
	 (character-count-indic (character-count-indicator length version encoding-mode))
	 (raw-data (concatenate 'string mode-indicator character-count-indic
				(encode-data string encoding-mode)))
	 (terminated-data (concatenate 'string raw-data (terminator (length raw-data) capacity)))
	 (padded-data (concatenate 'string terminated-data
				   (padding-to-multiple-of-eight (length terminated-data)))))
    (values
     (format nil "~a~a" padded-data (filling-to-capacity (length padded-data) capacity))
     (list :version version :error-correction-mode error-correction-mode))))

(defgeneric encode-data (data encoding-mode)
  (:documentation "Represent the data as a string of binary numbers."))

(defmethod encode-data ((data integer) (encoding-mode (eql :numeric-mode)))
  (format nil "~{~b~}" (split-integer-to-blocks data)))

(defmethod encode-data ((data string) (encoding-mode (eql :numeric-mode)))
  (format nil "~{~b~}" (mapcar #'parse-integer
			       (split-string-into-blocks data 3))))

(defmethod encode-data ((data string) (encoding-mode (eql :alphanumeric-mode)))
  (let ((substrings (split-string-into-blocks data 2)))
    (format nil "~{~b~}" (map 'list #'represent-substring substrings))))

(defmethod encode-data ((data string) (encoding-mode (eql :byte-mode)))
  (format nil "~{~8,'0b~}" (map 'list #'char-code data)))

(defun qr-capacity (version error-correction-mode)
  (* 8 (assocval error-correction-mode
		 (assocval version *capacity*))))

(defun terminator (string-length capacity)
  (if (< string-length capacity)
      (padded-binary 0 (min 4 (- capacity string-length)))
      ""))

(defun padding-to-multiple-of-eight (string-length)
  (unless (zerop (rem string-length 8))
    (padded-binary 0 (- 8 (rem string-length 8)))))

(defun filling-to-capacity (string-length capacity)
  (let ((filling-bytes (alexandria:circular-list (padded-binary 236 8)
						 (padded-binary 17 8)))
	(bytes-to-fill (/ (- capacity string-length) 8)))
    (apply #'concatenate 'string (loop repeat bytes-to-fill
				   for i in filling-bytes
				    collect i))))

(defun product (&rest integers)
  (reduce #'logxor integers))

(defun remove-trailing-zeros (list)
  (let ((result nil)
	(seen-nonzero nil))
    (dolist (number (reverse list))
      (when (or seen-nonzero (not (zerop number)))
	(setf seen-nonzero t)
	(push number result)))
    result))

(defun nil-padding (n)
  (loop repeat n collect nil))

(defun degree (polynomial)
  (1- (length (remove-trailing-zeros polynomial))))

(defun multiply (poly-a poly-b)
  (let* ((deg-a (degree poly-a))
	 (deg-b (degree poly-b))
	 (intermediate-products nil))
    (setf intermediate-products
	  (loop for coef upto deg-a collect
	       (append (nil-padding coef)
		       (mapcar (lambda (x) (* x (nth coef poly-a))) poly-b)
		       (nil-padding (- deg-a coef)))))
    (loop for k upto (+ deg-a deg-b) collect
	 (loop for product in intermediate-products
	      sum (or (nth k product) 0)))))

(defun multiply-exponent-list (poly-a poly-b)
  (let ((basis (mapcar (lambda (x) (mod (+ x (first poly-a)) 255)) poly-b))
	(second (mapcar (lambda (x) (mod (+ x (second poly-a)) 255)) poly-b)))
    (loop for a in (append basis (cons nil nil))
       for b in (append (cons nil nil) second) ;; do
       collect (galois-exponent (reduce #'product
					(mapcar #'nth-galois (remove-if #'null (list a b))))))))

(defun galois-exponent (integer)
  (car (rassoc (abs integer) *log-antilog*)))

(defun nth-galois (integer)
  "Return the integer-th element of the Galois field"
  (if (null integer)
      0
      (assocval integer *log-antilog*)))

(defvar *generator-polynomials*
  (loop for i from 0 upto 35
     for pol = (list i 0) then (multiply-exponent-list (list i 0) pol)
     collect (cons (1+ i) pol)))

(defun split-message-string (message)
  (mapcar (lambda (x) (parse-integer x :radix 2))
	  (split-string-into-blocks message 8)))

(defun message-polynomial (message)
  (reverse (split-message-string message)))

(defun shift-polynomial (polynomial n)
  (multiply polynomial
	    (loop for i upto n collect (if (= n i) 1 0))))

(defun prepare-polynomials (message-polynomial generator-galois)
  (let* ((generator-poly (mapcar #'nth-galois generator-galois))
	 (message-shift (degree generator-poly))
	 (generator-shift (degree message-polynomial)))
    (values (shift-polynomial message-polynomial message-shift)
	    (mapcar #'galois-exponent (shift-polynomial generator-poly generator-shift)))))

(defun step1a (message-poly generator-galois)
  (let* ((lead-coef (alexandria:last-elt (mapcar #'galois-exponent message-poly))))
    (mapcar (lambda (x) (multiply-galois-exponents lead-coef x))
	    generator-galois)))

(defun step1b (message-poly generator-galois)
  (let ((generator-poly (mapcar #'nth-galois generator-galois)))
    (remove-trailing-zeros (mapcar #'logxor message-poly generator-poly))))

(defun multiply-galois-exponents (&rest numbers)
  (if (some #'null numbers)
      nil
      (mod (reduce #'+ numbers) 255)))

(defun reed-solomon (message-poly generator-galois)
  (multiple-value-bind (mpo ggo) (prepare-polynomials message-poly generator-galois)
    (loop repeat (count-if-not #'zerop mpo)
       for gg = ggo then (rest gg)
       for mp = mpo then r1b
       for r1a = (step1a mp gg)
       for r1b = (step1b mp r1a)
	 finally (return r1b))))
