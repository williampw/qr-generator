;;;; qr-generator.asd

(asdf:defsystem #:qr-generator
  :description "Describe qr-generator here"
  :author "William Woelffel"
  :license "GPLv3"
  :depends-on (#:alexandria #:array-operations #:zpng)
  :serial t
  :components ((:file "package")
	       (:file "qr-tables")
	       (:file "polynomials")
               (:file "qr-generator")
	       (:file "qr-drawing")))

