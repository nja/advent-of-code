;;;; day23.lisp

(in-package #:aoc2016.day23)

(defun parse (input)
  (let ((*package* (symbol-package 'parse)))
    (map 'vector (lambda (line) (read-from-string (format nil "(~a~%)" line)))
         (aoc:lines input))))

(defun interpret (instructions registers)
  (flet ((r (x) (if (symbolp x) (getf registers x 0) x)))
    (loop with ip = 0
          while (array-in-bounds-p instructions ip)
          for (op x y) = (aref instructions ip)
          do (case op
               (cpy (setf (getf registers y) (r x)))
               (inc (incf (getf registers x)))
               (dec (decf (getf registers x)))
               (jnz (unless (zerop (r x)) (incf ip (1- (r y)))))
               (tgl (when (array-in-bounds-p instructions (+ ip (r x)))
                      (toggle (aref instructions (+ ip (r x)))))))
             (incf ip))
    registers))

(defun toggle (instruction)
  (when instruction
    (rplaca instruction
            (if (cddr instruction)
                (if (eq 'jnz (car instruction)) 'cpy 'jnz)
                (if (eq 'inc (car instruction)) 'dec 'inc)))))

(defun part1 (input)
  (getf (interpret (parse input) (list 'a 7)) 'a))

(defun split (instructions)
  (list (subseq instructions 0 (position 'tgl instructions :key #'car))
        (subseq instructions (position 'tgl instructions :key #'car))))

(defun toggles (instructions)
  (loop for i from 0 by 2 below (length instructions)
        do (toggle (aref instructions i))))

(defun part2 (input)
  (destructuring-bind (start finish) (split (parse input))
    (toggles finish)
    (let ((registers (interpret start (list 'a 12))))
      (setf (getf registers 'a) (* (getf registers 'a) (factorial (getf registers 'b)))
            (getf registers 'b) 1
            (getf registers 'c) 0)
      (getf (interpret finish registers) 'a))))

(defun factorial (n)
  (loop for i from 1 to n
        for p = i then (* p i)
        finally (return p)))
