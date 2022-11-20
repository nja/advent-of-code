;;;; day12.lisp

(in-package #:aoc2016.day12)

(defun parse (input)
  (let ((*package* (symbol-package 'parse)))
    (mapcar (lambda (line) (read-from-string (format nil "(~a~%)" line)))
            (aoc:lines input))))

(defun assemble (instructions)
  (let ((tags (loop for i below (length instructions) collect i))
        (disasm (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t)))
    (labels ((disasm (tag instruction op)
               (format disasm "~(~&~32a;~2,'0d ~a~)~%"
                       (format nil "~{~a~^ ~}" instruction)
                       tag
                       op))
             (asm (tag opcode x &optional y)
               (assert (or (integerp x) (member x '(a b c d))))
               (assert (or (null y) (integerp y) (member y '(a b c d))))
               (let ((op (case opcode
                           (cpy `(setf ,y ,x))
                           (inc `(incf ,x))
                           (dec `(decf ,x))
                           (jnz `(unless (zerop ,x) (go ,(+ tag y)))))))
                 (disasm tag (list opcode x y) op)
                 (list tag op))))
      (values
       `(lambda (c)
          (declare (optimize (speed 3) (space 0) (safety 0) (debug 0)))
          (let ((a 0) (b 0) (d 0))
            (declare (fixnum a b c d))
            (tagbody
               ,@(mapcan (a:curry #'apply #'asm) tags instructions))
            a))
       disasm))))

(defun part1 (input)
  (funcall (eval (assemble (parse input))) 0))

(defun part2 (input)
  (funcall (eval (assemble (parse input))) 1))
