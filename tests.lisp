(defpackage #:qr-generator-tests
  (:use #:cl #:5am #:qr-generator))

(in-package :qr-generator-tests)


;;; Helper functions to generate pratical strings

(defun gen-digit-char ()
  (lambda ()  (char "0123456789" (random 10))))

(defun gen-alphanumeric-char ()
  (lambda ()
    (char "ABCDEFGHIJKLMNOPQRSTUVWXYZ$%*+-./: " (random 35))))

(def-suite main)

(in-suite main)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Basics tests to check the data analysis

(def-suite data-analysis :in main
    :description "Textual data analysis: find the encoding-mode, the version, etc.")

(in-suite data-analysis)

(test numeric-mode
  (is-false (qr-generator::numeric-mode-p #\a))
  (is-false (qr-generator::numeric-mode-p #\&))
  (is-false (qr-generator::numeric-mode-p #\G))
  (for-all ((digit-char (gen-digit-char)))
    (is-true (qr-generator::numeric-mode-p digit-char))))

(test alphanumeric-mode
  (for-all ((alphanumeric-char (gen-alphanumeric-char)))
    (is-true (qr-generator::alphanumeric-mode-p alphanumeric-char)))
  (is-false (qr-generator::alphanumeric-mode-p #\u))
  (is-false (qr-generator::alphanumeric-mode-p #\f))
  (is-false (qr-generator::alphanumeric-mode-p #\é))
  (is-false (qr-generator::alphanumeric-mode-p #\À)))

(test determine-encoding
  (for-all ((digit-string (gen-string :length (gen-integer :min 1 :max 100)
			   :elements (gen-digit-char)))
	    (alphanumeric-string (gen-string
				  :length (gen-integer :min 1 :max 100)
				  :elements (gen-alphanumeric-char)))
	    (random-string (gen-string :length (gen-integer :min 1 :max 100))))
    (is (eql :numeric-mode (qr-generator::determine-encoding-mode digit-string)))
    (is (eql :alphanumeric-mode (qr-generator::determine-encoding-mode alphanumeric-string)))
    (is (eql :byte-mode (qr-generator::determine-encoding-mode random-string))))
  (is (eql :alphanumeric-mode (qr-generator::determine-encoding-mode "HELLO WORLD"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Test the data encoding process: find the right encoding mode according to the encoding type and
;;; length of the string, find it from the original string, etc. 

(def-suite data-encoding :in main)

(in-suite data-encoding)

(test qr-version
  (for-all ((numeric-string-length (gen-integer :min 5040 :max 5313))
	    (alphanumeric-string-length (gen-integer :min 2521 :max 2677))
	    (byte-string-length (gen-integer :min 43 :max 62)))
    (is (= 39 (qr-generator::get-qr-version numeric-string-length :M :numeric-mode)))
    (is (= 31 (qr-generator::get-qr-version alphanumeric-string-length :L :alphanumeric-mode)))
    (is (= 4 (qr-generator::get-qr-version byte-string-length :M :byte-mode)))))

(test qr-version-from-string
  (for-all ((numeric-string (gen-string :length (gen-integer :min 747 :max 813)
					:elements (gen-digit-char)))
	    (alphanumeric-string (gen-string
				  :length (gen-integer :min 353 :max 376)
				  :elements (gen-alphanumeric-char)))
	    (random-string (gen-string :length (gen-integer :min 625 :max 666))))
    (is (= 19 (qr-generator::get-qr-version-from-string numeric-string :H)))
    (is (= 14 (qr-generator::get-qr-version-from-string alphanumeric-string :Q)))
    (is (= 20 (qr-generator::get-qr-version-from-string random-string :M)))))

(test encode-data
  (is (equalp #*0110000101101111000110100010111001011011100010011010100001101
	      (qr-generator::encode-data "HELLO WORLD" :alphanumeric-mode))))

(test message
  (is (equalp #*00100000010110110000101101111000110100010111001011011100010011010100001101000000111011000001000111101100
	      (qr-generator::string-to-message "HELLO WORLD" "Q"))))

(test error-codewords-and-interleaving
  (let* ((test5 #*0100001101010101010001101000011001010111001001100101010111000010011101110011001000000110000100100000011001100111001001101111011011110110010000100000011101110110100001101111001000000111001001100101011000010110110001101100011110010010000001101011011011100110111101110111011100110010000001110111011010000110010101110010011001010010000001101000011010010111001100100000011101000110111101110111011001010110110000100000011010010111001100100001000011101100000100011110110000010001111011000001000111101100)
	 (property-list (list :version 5 :error-correction-mode :Q))
	 (message-polynomials (qr-generator::chunks-to-polynomials
			       (qr-generator::group-message test5 property-list)))
	 (ec-polynomials (qr-generator::correction-codewords message-polynomials property-list)))
    (is (equalp (qr-generator::structure-message message-polynomials ec-polynomials property-list)
		'(67 246 182 70 85 246 230 247 70 66 247 118 134 7 119 86 87 118 50 194 38 134 7
		  6 85 242 118 151 194 7 134 50 119 38 87 16 50 86 38 236 6 22 82 17 18 198 6
		  236 6 199 134 17 103 146 151 236 38 6 50 17 7 236 213 87 148 235 199 204 116
		  159 11 96 177 5 45 60 212 173 115 202 76 24 247 182 133 147 241 124 75 59 223
		  157 242 33 229 200 238 106 248 134 76 40 154 27 195 255 117 129 230 172 154
		  209 189 82 111 17 10 2 86 163 108 131 161 163 240 32 111 120 192 178 39 133
		  141 236)))))

(test interleaving-and-finalizing
  (let* ((test5 #*0100001101010101010001101000011001010111001001100101010111000010011101110011001000000110000100100000011001100111001001101111011011110110010000100000011101110110100001101111001000000111001001100101011000010110110001101100011110010010000001101011011011100110111101110111011100110010000001110111011010000110010101110010011001010010000001101000011010010111001100100000011101000110111101110111011001010110110000100000011010010111001100100001000011101100000100011110110000010001111011000001000111101100)
	 (property-list (list :version 5 :error-correction-mode :Q))
	 (message-polynomials (qr-generator::chunks-to-polynomials
			       (qr-generator::group-message test5 property-list)))
	 (ec-polynomials (qr-generator::correction-codewords message-polynomials property-list))
	 (structured-data (qr-generator::structure-message message-polynomials ec-polynomials property-list))
	 (reference #*01000011111101101011011001000110010101011111011011100110111101110100011001000010111101110111011010000110000001110111011101010110010101110111011000110010110000100010011010000110000001110000011001010101111100100111011010010111110000100000011110000110001100100111011100100110010101110001000000110010010101100010011011101100000001100001011001010010000100010001001011000110000001101110110000000110110001111000011000010001011001111001001010010111111011000010011000000110001100100001000100000111111011001101010101010111100101001110101111000111110011000111010010011111000010110110000010110001000001010010110100111100110101001010110101110011110010100100110000011000111101111011011010000101100100111111000101111100010010110011101111011111100111011111001000100001111001011100100011101110011010101111100010000110010011000010100010011010000110111100001111111111011101011000000111100110101011001001101011010001101111010101001001101111000100010000101000000010010101101010001101101100100000111010000110100011111100000010000001101111011110001100000010110010001001111000010110001101111011000000000))
    (is (equalp (qr-generator::vec-concatenate
		 (list (qr-generator::binarize-integers structured-data)
		       (qr-generator::remainder (getf property-list :version))))
		reference))))


(test format-bits
  (is (equalp (qr-generator::coefs (qr-generator::format-string :L 4))
	      '(1 1 1 1 0 1 0 0 0 1 1 0 0 1 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Test empty QR code generation

(def-suite qr-generation :in main)

(in-suite qr-generation)

(defun version-name (version)
  (format nil "empty-codes/version~2,'0d.png" version))

(defun get-reference-data (version)
  (png-read:image-data (png-read:read-png-file (version-name version))))

(test empty
  (dotimes (version 40)
    (is (equalp (qr-generator::transpose (get-reference-data (1+ version)))
		(qr-generator::pixelize
		 (make-instance 'qr-generator::qr-code :version (1+ version))
		 4)))))
