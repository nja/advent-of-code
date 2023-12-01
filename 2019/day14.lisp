;;;; day14.lisp

(in-package :aoc2019.day14)

(defun parse (line)
  (let ((read (read-from-string (format nil "(~a)" (delete #\, line)))))
    (list (reverse (butlast read 3))
          '=>
          (list (reverse (last read 2))))))

(defun reactions (input)
  (mapcar #'parse (aoc:lines input)))

(defun input (reaction) (first reaction))
(defun output (reaction) (third reaction))

(defun keys (list) (remove-if-not #'symbolp list))
(defun mul (n list)
  (mapcar (lambda (x)
            (if (numberp x)
                (* n x)
                x))
          list))
(defun add (key n list)
  (incf (getf list key 0) n))

(defun ore-only? (reaction)
  (every (lambda (x) (or (eq 'ore x) (not (symbolp x))))
         (input reaction)))

(defun costs (parsed-lines)
  (loop with hash-table = (make-hash-table)
        for ((n . key) costs) in parsed-lines
        do (setf (gethash key hash-table)
                 (list (list (cons key n))
                       costs))
        finally (return hash-table)))

(defun xreplace (what reaction)
  )
(defun add (what outputs)
  (loop for (key . n) in what
        
        do (incf (cdr ()))))

(defun times (output input)
  (multiple-value-bind (multiplier remainder) (ceiling input output)
    (values multiplier (- remainder))))

(defun substitute-reaction (key replacement reaction)
  ()
  (let ((in (getf (input reaction) key))
        (out (getf (output reaction key))))
    (multiple-value-bind (n remainder) )))

(defparameter *test* "10 ORE => 10 A
1 ORE => 1 B
7 A, 1 B => 1 C
7 A, 1 C => 1 D
7 A, 1 D => 1 E
7 A, 1 E => 1 FUEL")



(defparameter *testx* "10 ORE => 7 A
1 ORE => 1 B
7 A, 1 B => 1 C
1 A, 1 C => 1 D
10 A, 1 D => 1 E
17 A, 1 E => 1 FUEL")