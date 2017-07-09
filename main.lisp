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

(defun make (text &optional (error-correction-level "L"))
  "Generate a `qr-code' instance that represents TEXT. ERROR-CORRECTION-LEVEL can optionnally
  be set to L, M, Q or H in ascending error of data redundancy. In other words, the higher the error
  correction level, the more resistance to damage the QR code will be."
  (multiple-value-bind (bits property-list) (text-to-binary text error-correction-level)
    (let ((qr-code (make-instance 'qr-code :version (getf property-list :version)
				  :ec-mode (getf property-list :error-correction-mode))))
      (write-data qr-code bits)
      (finalize qr-code)
      qr-code)))

(defun save (qr-code file-name &optional (scale 5))
  "Saves QR-CODE to a png file names FILE-NAME. SCALE defines the size in pixels of the unit square
  of the png QR-CODE. It can be set to any non-zero integer. The bigger SCALE, the bigger the final
  png file."
  (print-to-png qr-code scale file-name))
