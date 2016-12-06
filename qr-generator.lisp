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

(defun assocval (item alist)
  (cdr (assoc item alist))) 

(defun bits (integer &optional (length nil))
  (unless length
    (setf length (if (zerop integer)
		     1
		     (integer-length integer))))
  (let ((result (make-array length :element-type 'bit :initial-element 0))
	(pointer (1- length)))
    (do ((i pointer (1- i))
	 (index 0 (1+ index)))
	((<= length index) result)
      (when (logbitp index integer)
	(setf (aref result i) 1)))))

(defun from-bits (bit-array)
  (loop for index downfrom (1- (length bit-array)) to 0
     for value = 1 then (* 2 value)
     unless (zerop (bit bit-array index))
     sum value))

(defun vec-concatenate (list-vectors)
  (let* ((total-length (reduce #'+ (mapcar #'length list-vectors)))
	 (result (make-array total-length :element-type 'bit :fill-pointer 0)))
    (loop for vec in list-vectors
       when vec do
	 (loop for element across vec do
	      (vector-push element result)))
    result))

(defun numeric-mode-p (char)
  (digit-char-p char))

(defun alphanumeric-mode-p (char)
  (let ((alphabet (map 'list #'character "ABCDEFGHIJKLMNOPQRSTUVWXYZ $%*+-./:")))
    (or (numeric-mode-p char)
	(member char alphabet))))

(defun determine-encoding-mode (string &optional (auto-upcase-string nil))
  "Checks STRING against all encoding modes and returns the suitable encoding mode. For now,
:byte-mode is only set to t if alphanumeric is unsuitable."
  (let* ((char-sequence (if auto-upcase-string (string-upcase string) string)))
    (cond
      ((every #'numeric-mode-p char-sequence) :numeric-mode)
      ((every #'alphanumeric-mode-p char-sequence) :alphanumeric-mode)
      (t :byte-mode))))

(defun choose-error-correction (level)
  (assert (= 1 (length level)))
  (if (member level '("H" "L" "M" "Q") :test #'string=)
      (intern level :keyword)
      (error "Unrecognized error correction level. Must be one of (L M Q H)")))

(defun encoding-mode-indicator (encoding-mode)
  "Represents the encoding mode indicator on a 4-bit string."
  (let ((encoding-mode-indicator (assocval encoding-mode *encoding-mode-indicators*)))
    (bits encoding-mode-indicator 4)))

(defun get-qr-version (string-length error-correction-mode encoding-mode)
  "Determines the suitable QR code version for a string whose length is STRING-LENGTH, given its
error correction-mode and encoding mode."
  (loop for version from 1 upto 40
     for key-triplet = (list version error-correction-mode encoding-mode)
     for capacity = (gethash key-triplet *character-capacities*)
     do (when (>= capacity string-length)
	  (return version))))

(defun character-count-indicator-length (version encoding-mode)
  (let ((version-table (assocval version *count-indicator-length*)))
    (assocval encoding-mode version-table)))

(defun character-count-indicator (string-length version encoding-mode)
  (bits string-length (character-count-indicator-length version encoding-mode)))

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
  (let ((result))
    (vec-concatenate
     (dolist (chk (chunk data 3) (nreverse result))
       (push (case (length chk)
	       (3 (bits (parse-integer chk) 10))
	       (2 (bits (parse-integer chk) 7))
	       (1 (bits (parse-integer chk) 4)))
	     result)))))

(defmethod encode-data ((data string) (encoding-mode (eql :alphanumeric-mode)))
  (flet ((represent-substring (substring)
	   (ecase (length substring)
	     ((1)
	      (bits (gethash (char substring 0) *alphanumeric-encoding*) 6))
	     ((2)
	      (bits (+ (* 45 (gethash (char substring 0) *alphanumeric-encoding*))
		       (gethash (char substring 1) *alphanumeric-encoding*))
		    11)))))
    (let ((substrings (chunk data 2)))
      (vec-concatenate (mapcar #'represent-substring substrings)))))

(defmethod encode-data ((data string) (encoding-mode (eql :byte-mode)))
  (vec-concatenate (mapcar (lambda (int) (bits int 8)) (map 'list #'char-code data))))

(defun string-to-message (string correction-level)
  (let* ((length (length string))
	 (encoding-mode (determine-encoding-mode string))
	 (error-correction-mode (choose-error-correction correction-level))
	 (version (get-qr-version length error-correction-mode encoding-mode))
	 (capacity (qr-capacity version error-correction-mode))
	 (mode-indicator (encoding-mode-indicator encoding-mode))
	 (character-count-indic (character-count-indicator length version encoding-mode))
	 (raw-data (vec-concatenate (list mode-indicator character-count-indic
					  (encode-data string encoding-mode))))
	 (data-length (length raw-data))
	 (terminator (terminator data-length capacity))
	 (data-length (+ data-length (length terminator)))
	 (padding (padding-to-multiple-of-eight data-length))
	 (data-length (+ data-length (length padding)))
	 (final-filling (filling-to-capacity data-length capacity)))
    (values
     (vec-concatenate (list raw-data terminator padding final-filling))
     (list :version version :error-correction-mode error-correction-mode))))

(defun qr-capacity (version error-correction-mode)
  "Number of bits required to fill a QR code of given VERSION with given ERROR-CORRECTION-MODE."
  (* 8 (getf (gethash (list version error-correction-mode) *codewords*)
	     :total-codewords)))

(defun terminator (string-length capacity)
  "Terminator bits that should be added at the end of an encoded string whose length is 
STRING-LENGTH to try and reach CAPACITY."
  (when (< string-length capacity)
      (bits 0 (min 4 (- capacity string-length)))))

(defun padding-to-multiple-of-eight (string-length)
  "Padding bits  that should be added at the end of a terminated string whose length is 
STRING-LENGTH to make its length a multiple of 8."
  (unless (zerop (rem string-length 8))
    (bits 0 (- 8 (rem string-length 8)))))

(defun filling-to-capacity (string-length capacity)
  "Filling bits that should be added at the end of a padded string whose length is STRING-LENGTH 
to make it reach CAPACITY."
  (let ((filling-bytes (alexandria:circular-list (bits 236 8)
						 (bits 17 8)))
	(bytes-to-fill (/ (- capacity string-length) 8)))
    (vec-concatenate (subseq filling-bytes 0 bytes-to-fill))))


(defun split-message-string (message)
  (mapcar #'from-bits (chunk message 8)))

(defun message-polynomial (message)
  (make-instance 'polynomial :coefs (nreverse (split-message-string message))))

(defun select-generator-galois (version error-correction-mode)
  (let ((generator-length (getf (gethash (list version error-correction-mode) *codewords*)
				:ec-codewords)))
    (aref *generator-galois* (1- generator-length))))

(defun reed-solomon (message-poly generator-poly)
  (divide (multiply message-poly (x-power-n (degree generator-poly)))
	  generator-poly))

(defun group-message (message property-list)
  (destructuring-bind (&key version error-correction-mode) property-list
    (let ((block-info (gethash (list version error-correction-mode) *block-information*)))
      (append (list (chunk (subseq message 0 (* 8 (getf block-info :blocks-in-g1)
						(getf block-info :words-in-b1)))
			   (* 8 (getf block-info :words-in-b1))))
	      (when (getf block-info :blocks-in-g2)
		(list (chunk (subseq message (* 8 (getf block-info :blocks-in-g1)
						(getf block-info :words-in-b1)))
			     (* 8 (getf block-info :words-in-b2)))))))))

(defun chunks-to-polynomials (chunks)
  (loop for group in chunks collect
       (loop for this-block in group
	  collect (message-polynomial this-block))))

(defun small-qr-code-p (property-list)
  (destructuring-bind (&key version error-correction-mode) property-list
    (let ((block-info (gethash (list version error-correction-mode) *block-information*)))
      (= 1 (getf block-info :blocks-in-g1)))))

(defun correction-codewords (chunked-polynomials property-list)
  (destructuring-bind (&key version error-correction-mode) property-list
    (loop for group in chunked-polynomials collect
	 (loop for poly in group collect
	      (reed-solomon poly
			    (select-generator-galois version error-correction-mode))))))

(defun interleave-blocks (groups)
  (let ((blocks (loop for group in groups append
		     (loop for this-block in group collect
			  (reverse (coefs this-block))))))
    (loop for i below (reduce #'max blocks :key #'length)
       append (loop for this-block in blocks
		 collect (when (< i (length this-block)) (aref this-block i))) into result
       finally (return (remove-if #'null result)))))

(defun structure-message (chunked-polynomials correction-codewords property-list)
  (flet ((reverse-poly ()
	   (loop for group in chunked-polynomials collect
		(loop for poly in group collect
		     (coerce (reverse (coefs poly)) 'list)))))
   (if (small-qr-code-p property-list)
       (alexandria:flatten (concatenate 'list
					(reverse-poly)
					(reverse (coefs (caar correction-codewords)))))
       (concatenate 'list  (interleave-blocks chunked-polynomials)
		    (interleave-blocks correction-codewords)))))

(defun binarize-integers (interleaved-data)
  (vec-concatenate (mapcar (lambda (int) (bits int 8)) interleaved-data)))

(defun remainder (version)
  (unless (zerop (aref *remainders* version))
    (bits 0 (aref *remainders* version))))

(defun text-to-binary (text correction-level)
  (multiple-value-bind (message property-list) (string-to-message text correction-level)
    (let* ((chunked-poly
	    (chunks-to-polynomials (group-message message property-list)))
	   (ec-words (correction-codewords chunked-poly property-list))
	   (structured-data (structure-message chunked-poly
					       ec-words
					       property-list)))
      (values (vec-concatenate (list (binarize-integers structured-data)
				     (remainder (getf property-list :version))))
	      property-list))))

