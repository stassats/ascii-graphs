;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:ascii-graph)

(defvar *width* 80)

(defun total-count (alist)
  (reduce #'+ alist :key #'second))

(defun max-value (alist)
  (reduce #'max alist :key #'second))

(defun max-length (alist)
  (reduce #'max alist :key
          (lambda (x)
            (length (princ-to-string  (car x))))))

(defun max-value-length (alist)
  (reduce #'max alist :key
          (lambda (x)
            (length (princ-to-string  (cdr x))))))

(defun row-length (value total pad)
  (truncate (* (- *width* pad) value) total))

(defun percent (value total)
  (/ (* 100 value) total))

(defun make-bar (value total pad)
  (make-string (row-length value total pad)
               :initial-element #\-))

(defun bar-graph (alist &key title (key #'identity) end
                             print-total
                             percent
                             ((:width *width*) *width*))
  "Alist (name value)"
  (when alist
   (let* ((total (total-count alist))
          (alist (subseq alist 0 end))
          (max-value (max-value alist))
          (max (funcall key max-value))
          (max-length (max-length alist))
          (max-value-length (max-value-length alist))
          (pad (+ max-length 1
                  max-value-length
                  (if percent
                      8
                      0)))
          (width (+ max-length 3
                    (row-length max max pad))))
     (when title
       (format t "~vt~a~2%" (floor (- *width* (length title)) 2) title))
     (loop for (name value) in alist
           do
           (format t "~v@a ~ao~vt~a~@[~vt ~5,2f%~]~%"
                   max-length
                   name
                   (make-bar (funcall key value) max pad)
                   width
                   value
                   (and percent
                        (+ width max-value-length))
                   (and percent
                        (percent value total))))
     (when print-total
      (format t "----~%Total: ~a~%" total))
     (values))))
