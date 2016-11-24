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
;;; Test the data encoding process: find the right encoding mode according to the encoding type and length of the string, find it from the original string, etc.

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
  (is (string= "0110000101101111000110100010111001011011100010011010100001101"
	       (qr-generator::encode-data "HELLO WORLD" :alphanumeric-mode))))

(test message
  (is (string= "00100000010110110000101101111000110100010111001011011100010011010100001101000000111011000001000111101100"
	       (qr-generator::string-to-message "HELLO WORLD" "Q"))))
