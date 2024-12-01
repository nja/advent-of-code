;;;; day25.lisp

(in-package #:aoc2016.day25)

(defun parse (input)
  (let ((*package* (symbol-package 'parse)))
    (map 'vector (lambda (line) (read-from-string (format nil "(~a~%)" line)))
         (aoc:lines input))))

(defun interpret (instructions a)
  (let ((registers (list 'a a 'b 0 'c 0 'd 0))
        (out 0))
    (flet ((r (x) (if (symbolp x) (getf registers x 0) x)))
      (loop with ip = 0
            while (array-in-bounds-p instructions ip)
            for (op x y) = (aref instructions ip)
            do (case op
                 (cpy (setf (getf registers y) (r x)))
                 (inc (incf (getf registers x)))
                 (dec (decf (getf registers x)))
                 (jnz (unless (zerop (r x)) (incf ip (1- (r y)))))
                 (out (unless (eql out (r x))
                        (return-from interpret))
                  (setf out (if (zerop out) 1 0))))
               (incf ip)))
    t))

(defun part1 (input)
  (let ((instructions (parse input)))
    (setf (aref instructions (1- (length instructions))) '(jnz 0 0))
    (loop for i from 1
          until (interpret instructions i)
          finally (return i))))
