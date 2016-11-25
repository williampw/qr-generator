;;;; qr-generator.asd

(asdf:defsystem #:qr-generator
  :description "qr-generator attempts to generate QR codes from strings according to the ISO specification."
  :author "William Woelffel <william.woelffel@gmail.com>"
  :license "GPLv3"
  :long-description
  "QR code is registered trademark of DENSO WAVE INCORPORATED"
  ;; :in-order-to ((test-op (test-op "qr-generator-test"))
  :depends-on (#:alexandria
	       #:array-operations
	       #:zpng
	       #:cl-slice)
  :serial t
  :components ((:file "package")
	       (:file "qr-tables")
	       (:file "polynomials")
               (:file "qr-generator")
	       (:file "qr-drawing")))

(defmethod perform ((o test-op)
                    (c (eql (find-system :qr-generator))))
  (load-system :qr-generator-tests)
    (symbol-call :5am :run! :qr-generator))
