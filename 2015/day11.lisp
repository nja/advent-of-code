;;;; day11.lisp

(in-package :aoc2015.day11)

(defun increment (password &optional (pos 0))
  (let* ((i (- (length password) pos 1))
        (c (aref password i)))
    (if (eql #\a (setf (aref password i) (add c)))
        (increment password (1+ pos))
        password)))

(defun add (char)
  (code-char (+ (mod (1+ (- (char-code char) (char-code #\a)))
                     (1+ (- (char-code #\z) (char-code #\a))))
                (char-code #\a))))

(defun increasing? (password)
  (labels ((rec (i n)
             (cond ((zerop n) t)
                   ((eql i (1- (length password))) nil)
                   ((eql (1+ (char-code (aref password i)))
                         (char-code (aref password (1+ i))))
                    (rec (1+ i) (1- n))))))
    (loop for i below (length password)
            thereis (rec i 2))))

(defun confusing? (password)
  (some (a:rcurry #'find password) "iol"))

(defun pairs? (password)
  (ppcre:scan "(.)\\1.*(.)\\2" password))

(defun valid? (password)
  (and (increasing? password)
       (not (confusing? password))
       (pairs? password)))

(defun next (password)
  (loop for p = (increment password) then (increment p)
        when (valid? p)
          do (return p)))

(defun part1 (input)
  (next (remove #\Newline input)))

(defun part2 (input)
  (next (part1 input)))
