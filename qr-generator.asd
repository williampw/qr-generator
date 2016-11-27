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
	       (:file "tables")
	       (:file "polynomials")
	       (:file "masks")
               (:file "qr-generator")
	       (:file "qr-drawing")
	       (:file "main")))

(defmethod perform ((o test-op)
                    (c (eql (find-system :qr-generator))))
  (load-system :qr-generator-tests)
    (symbol-call :5am :run! :qr-generator))
