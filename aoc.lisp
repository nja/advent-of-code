;;;; aoc.lisp

(in-package #:aoc)

(defun input-for (year day)
  (let ((pathname (asdf:system-relative-pathname
                   :advent-of-code
                   (format nil "~4,'0d/day~2,'0d.input.txt" year day))))
    (alexandria:read-file-into-string pathname)))

(defun lines (string)
  (with-input-from-string (in string)
    (loop for line = (read-line in nil nil)
          while line
          collect line)))

(defun trim-lf (string)
  (string-right-trim '(#\Linefeed) string))

(defun strip-cr (string)
  (remove #\Return string))

(defun tr (set1 set2 seq)
  (map (type-of seq)
       (lambda (x)
         (let ((pos (position x set1)))
           (if pos
               (elt set2 pos)
               x)))
       seq))

(defun symbols (symbols)
  (lambda (s) (find s symbols :key #'symbol-name :test #'string-equal)))

(defun print-array (array)
  (labels
      ((top-row (rows cols i)
         (format t "~&~a" (getspc rows))
         (loop with fmt = (getfmt cols)
               for n below cols
               for s = (format nil fmt n)
               for c = (aref s i)
               do (princ c)))
       (digits (n)
         (length (format nil "~d" n)))
       (getfmt (n)
         (format nil "~~~d,d" (digits n)))
       (getspc (n)
         (format nil "~{~a~}"
                 (make-list (digits n) :initial-element #\Space))))
    (destructuring-bind (rows cols) (array-dimensions array)
      (loop for i from 0 below (digits cols) do (top-row rows cols i))
      (loop with fmt = (getfmt rows)
            for y below rows
            do (format t "~&")
               (format t fmt y)
               (loop for x below cols do (princ (aref array y x)))))))
