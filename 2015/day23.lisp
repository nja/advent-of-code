;;;; day23.lisp

(in-package :aoc2015.day23)

(defun parse (input)
  (let ((*package* (symbol-package 'parse)))
    (mapcar (lambda (line) (read-from-string (format nil "(~a~%)" (remove #\, line))))
            (aoc:lines input))))

(defun assemble (instructions a)
  (let ((tags (loop for i below (length instructions) collect i))
        (disasm (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t)))
    (labels ((disasm (tag instruction op)
               (format disasm "~(~&~32a;~2,'0d ~a~)~%"
                       (format nil "~{~a~^ ~}" instruction)
                       tag
                       op))
             (asm (tag instruction)
               (destructuring-bind (opcode r &optional x) instruction
                 (flet ((dst () (or (find (+ tag (or x r)) tags) 'end)))
                   (let ((op (case opcode
                               (hlf `(setf ,r (truncate ,r 2)))
                               (tpl `(setf ,r (* ,r 3)))
                               (inc `(incf ,r))
                               (jmp `(go ,(dst)))
                               (jie `(when (evenp ,r) (go ,(dst))))
                               (jio `(when (eq 1 ,r) (go ,(dst)))))))
                     (disasm tag instruction op)
                     op)))))
      (let ((ops (mapcar #'asm tags instructions)))
        (values
         `(lambda ()
            (let ((a ,a) (b 0))
              (declare (fixnum a b) (optimize (speed 3) (safety 0) (debug 0) (space 0)))
              (tagbody ,@(mapcan #'list tags ops)
               end)
              b))
         disasm)))))

(defun part1 (input)
  (funcall (eval (assemble (parse input) 0))))

(defun part2 (input)
  (funcall (eval (assemble (parse input) 1))))
