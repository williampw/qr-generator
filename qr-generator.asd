;;;; qr-generator.asd

(asdf:defsystem #:qr-generator
  :description "Describe qr-generator here"
  :author "William Woelffel"
  :license "GPLv3"
  :depends-on (#:alexandria)
  :serial t
  :components ((:file "package")
               (:file "qr-generator")))

