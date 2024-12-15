;;;; parse.lisp

(in-package :aoc)

(defun lines (string)
  (ppcre:split "\\n" string))

(defun sections (string)
  (ppcre:split "\\n\\n" string))

(defun trim-lf (string)
  (string-right-trim '(#\Linefeed) string))

(defun strip-cr (string)
  (remove #\Return string))

(defun read-integers (string)
  (read-as-list (map 'string (lambda (c)
                               (if (or (digit-char-p c) (find c "-"))
                                   c
                                   #\Space))
                     string)))

(defun read-as-list (string)
  (read-from-string (format nil "(~a)" string)))

(defun map-sections (input &rest functions)
  (mapcar #'funcall functions (aoc:sections input)))