(defpackage #:qr-generator-tests
  (:use #:cl #:5am #:qr-generator))

(in-package :qr-generator-tests)

;;; Basics tests to check the data analysis

(test numeric-mode
  (is-false (numeric-mode-p "1")))

(test beubeu
  (is-true (= 5 (+ 7 -2))))
