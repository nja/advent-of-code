;;;; day24.lisp

(in-package #:aoc2021.day24)

(defun instructionp (s) (funcall (aoc:symbols '(inp add mul div mod eql)) s))
(defun registerp (s) (funcall (aoc:symbols '(w x y z)) s))
(defun operandp (s) (or (registerp s) (parse-integer s)))

(defun instruction (line)
  (ppcre:register-groups-bind ((#'instructionp instruction)
                               (#'registerp a)
                               (#'operandp b))
      ("(inp|add|mul|div|mod|eql) (w|x|y|z) ?(w|x|y|z|[+-]?\\d+)?" line)
    (assert (or (and (eq instruction 'inp) (null b)) b))
    (case instruction
      (inp (list instruction a))
      ((add mul div mod eql) (list instruction a b)))))

(defun parse (input)
  (mapcar #'instruction (aoc:lines input)))

(defun assemble (instructions)
  `(lambda (inputs &optional (z 0))
     (let ((w 0) (x 0) (y 0) (z z) (i 0))
       ,@(mapcar
          (lambda (instruction)
            (destructuring-bind (op a &optional b) instruction
              (case op
                (inp `(setf ,a (aref inputs (prog1 i (incf i)))))
                (add `(incf ,a ,b))
                (mul `(setf ,a (* ,a ,b)))
                (div `(setf ,a (truncate ,a ,b)))
                (mod `(setf ,a (mod ,a ,b)))
                (eql `(setf ,a (if (eql ,a ,b) 1 0))))))
          instructions)
       z)))

(defun alu (instructions digits)
  (when (zerop (funcall (eval (assemble instructions)) digits))
    digits))

(defun constraints (instructions)
  (loop with result
        with stack and pop
        with x and y and i = -1
        for instruction in instructions
        do (destructuring-bind (op a &optional b) instruction
             (case op
               (inp (incf i))
               (add (cond ((numberp b)
                           (case a
                             (x (setf x b))
                             (y (setf y b))))
                          ((and (not pop) (eq a 'z) (eq b 'y))
                           (push (cons i y) stack))))
               (eql (when (and pop (eq a 'x) (eq b 'w))
                      (destructuring-bind (j . y) (pop stack)
                        (let ((diff (+ x y)))
                          (push (if (plusp diff)
                                    (list j diff i)
                                    (list i (- diff) j))
                                result)))))
               (div (when (eq a 'z)
                      (setf pop (eql b 26))))))
        finally (return result)))

(defun print-constraint (constraint)
  (apply #'format t "[~d]~@d=[~d]~%" constraint))

(defun maximize (constraints)
  (loop with digits = (make-array (* 2 (length constraints)) :initial-element 9)
        for (i diff j) in constraints
        do (decf (aref digits i) diff)
        finally (return digits)))

(defun part1 (input)
  (let ((instructions (parse input)))
    (alu instructions (maximize (constraints instructions)))))

(defun minimize (constraints)
  (loop with digits = (make-array (* 2 (length constraints)) :initial-element 1)
        for (i diff j) in constraints
        do (incf (aref digits j) diff)
        finally (return digits)))

(defun part2 (input)
  (let ((instructions (parse input)))
    (alu instructions (minimize (constraints instructions)))))
