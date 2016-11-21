(asdf:defsystem #:qr-generator-tests
  :description "qr-generator attempts to generate QR codes from strings according to the ISO specification."
  :author "William Woelffel <william.woelffel@gmail.com>"
  :license "GPLv3"
  :depends-on (#:qr-generator #:fiveam)
  :components ((:file "tests"))
  :perform (test-op (o s) (symbol-call :fiveam '#:run!)))

(defmethod perform ((o test-op) (c (eql (find-system :qr-generator-tests))))
  (uiop:symbol-call :fiveam '#:run!
		    ;; (uiop:find-symbol* '#:qr-generator-test-suite
		    ;; 		       :qr-generator-tests)
		    ))
