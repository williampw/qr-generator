(in-package #:qr-generator)

(defun make (text &optional (error-correction-level "L"))
  "Generate a `qr-code' instance that represents TEXT. ERROR-CORRECTION-LEVEL can optionnally
  be set to L, M, Q or H in acending error of data redundancy. In other words, the higher the error
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
