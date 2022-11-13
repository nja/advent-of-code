;;;; day21.lisp

(in-package #:aoc2018.day21)

(defun parse (input)
  (let ((*package* (symbol-package 'parse)))
    (destructuring-bind (ip-declaration . instructions) (aoc:lines input)
      (values (mapcar (lambda (line) (read-from-string (format nil "(~a~%)" line)))
                      instructions)
              (parse-integer ip-declaration :start 4)))))

(defmacro test (p) `(if ,p 1 0))

(defun assemble (instructions ip-reg-ix)
  (let ((registers (list 'r0 'r1 'r2 'r3 'r4 'r5))
        (tags (loop for i below (length instructions) collect i))
        (disasm (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t)))
    (setf (elt registers ip-reg-ix) 'ip)
    (labels ((disasm (tag instruction op)
               (format disasm "~(~&~32a;~2,'0d ~a~)~%"
                       (format nil "~{~a~^ ~}" instruction)
                       tag
                       op))
             (asm (tag opcode a b output)
               (flet ((a () (elt registers a))
                      (b () (elt registers b)))
                 (let ((op `(setf ,(elt registers output)
                                  ,(case opcode
                                     (addr `(+ ,(a) ,(b)))
                                     (addi `(+ ,(a) ,b))
                                     (mulr `(* ,(a) ,(b)))
                                     (muli `(* ,(a) ,b))
                                     (banr `(logand ,(a) ,(b)))
                                     (bani `(logand ,(a) ,b))
                                     (borr `(logior ,(a) ,(b)))
                                     (bori `(logior ,(a) ,b))
                                     (setr (a))
                                     (seti a)
                                     (gtir `(test (> ,a ,(b))))
                                     (gtri `(test (> ,(a) ,b)))
                                     (gtrr `(test (> ,(a) ,(b))))
                                     (eqri `(test (= ,(a) ,b)))
                                     (eqir `(test (= ,a ,(b))))
                                     (eqrr `(test (= ,(a) ,(b))))))))
                   (disasm tag (list opcode a b output) op)
                   (if (and (eq opcode 'eqrr) (zerop (min a b)))
                       (let ((r (elt registers (max a b))))
                         (list tag
                               `(when first
                                  (setf r0 ,r)
                                  (go end))
                               `(when (gethash ,r seen)
                                  (setf r0 last)
                                  (go end))
                               `(setf (gethash ,r seen) t
                                      last ,r)
                               op '(go next)))
                       (list tag op '(go next)))))))
      (format disasm "#ip ~d" ip-reg-ix)
      (values
       `(lambda (r0 &key first)
          (declare (optimize (speed 3) (space 0) (safety 0) (debug 0)))
          (let (,@(rest (mapcar (lambda (x) (list x 0)) registers))
                (seen (make-hash-table)) last)
            (declare (fixnum ,@registers))
            (tagbody
               ,@(mapcan (a:curry #'apply #'asm) tags instructions)
             next
               (incf ip)
               (case ip ,@(mapcar (lambda (tag) `(,tag (go ,tag))) tags))
             end)
            r0))
       disasm))))

(defun part1 (input)
  (funcall (eval (multiple-value-call #'assemble (parse input))) 0 :first t))

(defun part2 (input)
  (funcall (eval (multiple-value-call #'assemble (parse input))) 0))
