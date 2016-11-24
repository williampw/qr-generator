;;;; qr-generator.lisp

(in-package #:qr-generator)

(defun assocval (item alist)
  (cdr (assoc item alist))) 

(defun numeric-mode-p (char)
  (digit-char-p char))

(defun alphanumeric-mode-p (char)
  (let ((alphabet (map 'list #'character "ABCDEFGHIJKLMNOPQRSTUVWXYZ $%*+-./:")))
    (or (numeric-mode-p char)
	(member char alphabet))))

(defun determine-encoding-mode (string &optional (auto-upcase-string nil))
  "Checks STRING against all encoding modes and returns the suitable encoding mode. For now,
:byte-mode is only set to t if alphanumeric is unsuitable."
  (let* ((char-sequence (map 'list #'character (if auto-upcase-string
						   (string-upcase string)
						   string))))
    (cond
      ((every #'numeric-mode-p char-sequence) :numeric-mode)
      ((every #'alphanumeric-mode-p char-sequence) :alphanumeric-mode)
      (t :byte-mode))))

(defun choose-error-correction (level)
  (assert (= 1 (length level)))
  (cond ((string-equal level "L") :L)
	((string-equal level "M") :M)
	((string-equal level "Q") :Q)
	((string-equal level "H") :H)
	(t (error "Unrecognized error correction level. Must be one of (L M Q H)"))))

(defun analyze-text (input-text)
  "Reads an INPUT-TEXT string and returns a plist containg the suitable version and encoding mode.")

(defun encoding-mode-indicator (encoding-mode)
  "Represents the encoding mode indicator on a 4-bit string."
  (let ((encoding-mode-indicator (assocval encoding-mode *encoding-mode-indicators*)))
    (padded-binary encoding-mode-indicator 4)))

(defun get-qr-version (string-length error-correction-mode encoding-mode)
  "Determines the suitable QR code version for a string whose length is STRING-LENGTH, given its
error correction-mode and encoding mode."
  (loop for version from 1 upto 40
     for key-triplet = (list version error-correction-mode encoding-mode)
     for capacity = (gethash key-triplet *character-capacities*)
     do (when (>= capacity string-length)
	  (return version))))

(defun get-qr-version-from-string (string error-correction-mode)
  (let ((length (length string))
	(encoding-mode (determine-encoding-mode string)))
    (get-qr-version length error-correction-mode encoding-mode)))

(defun padded-binary (decimal-number width)
  "Represents DECIMAL-NUMBER in binary, with a 0 padding to the left until WIDTH is reached."
  (format nil "~v,'0b" width decimal-number))

(defun character-count-indicator-length (version encoding-mode)
  (let ((version-table (assocval version *count-indicator-length*)))
    (assocval encoding-mode version-table)))

(defun character-count-indicator (string-length version encoding-mode)
  (padded-binary string-length
		 (character-count-indicator-length version encoding-mode)))

(defun chunk (sequence chunk-size)
  "Split SEQUENCE into chunks of CHUNK-SIZE characters. The final block may be smaller than
CHUNK-SIZE. Returns the list of subsequences."
  ;; Collect the subsequences as long as there is room for a full chunk, ie as long as end is below
  ;; the length. The final chunk is collected without specifying the end, and appended to the list
  ;; before reversing it.
  (do ((start 0 (+ start chunk-size))
       (end chunk-size (+ end chunk-size))
       (end-max (1- (length sequence)))
       (result nil))
      ((> end end-max) (append (nreverse result)
			       (list (subseq sequence start))))
    (push (subseq sequence start end) result)))

(defgeneric encode-data (data encoding-mode)
  (:documentation "Represent DATA as a string of binary numbers."))

(defmethod encode-data ((data string) (encoding-mode (eql :numeric-mode)))
  (format nil "~{~b~}" (mapcar #'parse-integer (chunk data 3))))

(defmethod encode-data ((data string) (encoding-mode (eql :alphanumeric-mode)))
  (flet ((represent-substring (substring)
	   (ecase (length substring)
	     ((1)
	      (padded-binary (gethash (char substring 0) *alphanumeric-encoding*)
			     6))
	     ((2)
	      (padded-binary (+ (* 45 (gethash (char substring 0) *alphanumeric-encoding*))
				(gethash (char substring 1) *alphanumeric-encoding*))
			     11)))))
    (let ((substrings (chunk data 2)))
      (format nil "~{~b~}" (mapcar #'represent-substring substrings)))))

(defmethod encode-data ((data string) (encoding-mode (eql :byte-mode)))
  (format nil "~{~8,'0b~}" (mapcar #'char-code data)))

(defun string-to-message (string correction-level)
  (let* ((length (length string))
	 (encoding-mode (determine-encoding-mode string))
	 (error-correction-mode (choose-error-correction correction-level))
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

(defun qr-capacity (version error-correction-mode)
  "Number of bits required to fill a QR code of given VERSION with given ERROR-CORRECTION-MODE."
  (* 8 (getf (gethash (list version error-correction-mode) *codewords*)
	     :total-codewords)))

(defun terminator (string-length capacity)
  "Terminator string that should be added at the end of an encoded string whose length is 
STRING-LENGTH to try and reach CAPACITY."
  (when (< string-length capacity)
      (padded-binary 0 (min 4 (- capacity string-length)))))

(defun padding-to-multiple-of-eight (string-length)
  "Padding string that should be added at the end of a terminated string whose length is 
STRING-LENGTH to make its length a multiple of 8."
  (unless (zerop (rem string-length 8))
    (padded-binary 0 (- 8 (rem string-length 8)))))

(defun filling-to-capacity (string-length capacity)
  "Filling string that should be added at the end of a padded string whose length is STRING-LENGTH 
to make it reach CAPACITY."
  (let ((filling-bytes (alexandria:circular-list (padded-binary 236 8)
						 (padded-binary 17 8)))
	(bytes-to-fill (/ (- capacity string-length) 8)))
    (format nil "~{~a~}" (subseq filling-bytes 0 bytes-to-fill))))


(defun split-message-string (message)
  (mapcar (lambda (x) (parse-integer x :radix 2))
	  (chunk message 8)))

(defun message-polynomial (message)
  (make-instance 'polynomial :coefs (nreverse (split-message-string message))))

(defun select-generator-galois (version error-correction-mode)
  (let ((generator-length (getf (gethash (list version error-correction-mode) *codewords*)
				:ec-codewords)))
    (aref *generator-galois* (1- generator-length))))

(defun reed-solomon (message-poly generator-poly)
  (divide (multiply message-poly (x-power-n (degree generator-poly)))
	  generator-poly))

(defun submessage-to-int (submessage)
  (split-message-string submessage))

(defun group-message (message property-list)
  (destructuring-bind (&key version error-correction-mode) property-list
    (let* ((block-info (gethash (list version error-correction-mode) *block-information*))
	   (group1 (chunk message (* 8 (getf block-info :words-in-b1)))))
      (append (list group1)
	      (when (getf block-info :blocks-in-g2)
		(list (chunk (subseq message (* 8 (getf block-info :blocks-in-g1)
						(getf block-info :words-in-b1)))
			     (* 8 (getf block-info :words-in-b2)))))))))

(defun chunks-to-polynomials (chunks)
  ;; (mapcar #'message-polynomial (alexandria:flatten chunks))
  (loop for group in chunks collect
       (loop for this-block in group
	    collect (message-polynomial this-block))))

(defun small-qr-code-p (property-list)
  (destructuring-bind (&key version error-correction-mode) property-list
    (destructuring-bind (blocks-in-grp1 words-in-block1 blocks-in-grp2 words-in-block2)
	(assocval error-correction-mode (assocval version *block-information*))
      (declare (ignore blocks-in-grp2 words-in-block1 words-in-block2))
     (= 1 blocks-in-grp1))))

(defun correction-codewords (chunked-polynomials property-list)
  (destructuring-bind (&key version error-correction-mode) property-list
    (loop for group in chunked-polynomials collect
	 (loop for poly in group collect
	      (reed-solomon poly
			    (select-generator-galois version error-correction-mode))))))

(defun interleave-blocks (groups)
  (let ((blocks (loop for group in groups append
		     (loop for this-block in group collect this-block))))
    (loop for i below (reduce #'max blocks :key #'length)
       append (loop for this-block in blocks
		collect (nth i this-block)) into result
       finally
	 (return (remove-if #'null result)))))

(defun structure-message (chunked-polynomials correction-codewords property-list)
  (flet ((reverse-poly ()
	   (loop for group in chunked-polynomials collect
		(loop for poly in group collect
		     (reverse (coefs poly))))))
   (if (small-qr-code-p property-list)
       (alexandria:flatten (append (reverse-poly) correction-codewords))
       (append (interleave-blocks (reverse-poly))
	      (interleave-blocks correction-codewords)))))

;; (defparameter text "HELLO WOLRD")
;; (defparameter level "M")
;; (multiple-value-bind (msg plist) (string-to-message text level)
;;   (let* ((chunked-poly
;; 	  (chunks-to-polynomials (group-message msg plist))))
;;     (format t "~a~%" chunked-poly)
;;     ;; (structure-message chunked-poly
;;     ;; 		       (correction-codewords chunked-poly plist)
;;     ;; 		       plist)
;;     ))
;; (defvar test5 "0100001101010101010001101000011001010111001001100101010111000010011101110011001000000110000100100000011001100111001001101111011011110110010000100000011101110110100001101111001000000111001001100101011000010110110001101100011110010010000001101011011011100110111101110111011100110010000001110111011010000110010101110010011001010010000001101000011010010111001100100000011101000110111101110111011001010110110000100000011010010111001100101110000011101100000100011110110000010001111011000001000111101100")

;; (let* ((msg test5)
;;        (plist (list :version 5 :error-correction-mode :Q))
;;        (chunked-poly
;; 	(chunks-to-polynomials (group-message msg plist)))
;;        (ec-words (correction-codewords chunked-poly plist)))
;;   (format t "~a~%" chunked-poly)
;;   (format t "~a~%" ec-words)
;;   (structure-message chunked-poly
;; 		     ec-words
;; 		     plist))
;; (print (string= "00100000010110110000101101111000110100010111001011011100010011010100001101000000111011000001000111101100" (string-to-message "HELLO WORLD" "Q")))

(defun binarize-integers (interleaved-data)
  (format nil "~{~8,'0b~}" interleaved-data))

(defun complete-with-remainder (binary-data version)
  (concatenate 'string binary-data
	       (padded-binary 0 (assocval version *remainders*))))

(defun text-to-binary (text correction-level)
  (multiple-value-bind (message property-list) (string-to-message text correction-level)
    (let* ((chunked-poly
	    (chunks-to-polynomials (group-message message property-list)))
	   (ec-words (correction-codewords chunked-poly property-list))
	   (structured-data (structure-message chunked-poly
					       ec-words
					       property-list)))
      (complete-with-remainder (binarize-integers structured-data)
			       (getf property-list :version)))))

