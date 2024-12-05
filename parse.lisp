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
  (read-from-string (format nil "(~a)" (map 'string (lambda (c)
                                                      (if (digit-char-p c)
                                                          c
                                                          #\Space))
                                            string))))

(defun map-sections (input &rest functions)
  (mapcar #'funcall functions (aoc:sections input)))