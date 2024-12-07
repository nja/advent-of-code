;;;; day19.lisp

(in-package #:aoc2018.day19)

(defun parse (input)
  (destructuring-bind (ip-declaration . instructions) (aoc:lines input)
    (values (mapcar #'aoc:read-as-list instructions)
            (parse-integer ip-declaration :start 4))))

(defun assemble (instructions ip-reg-ix &key escape)
  (let ((registers (list 'r0 'r1 'r2 'r3 'r4 'r5))
        (tags (loop for i below (length instructions) collect i)))
    (setf (elt registers ip-reg-ix) 'ip)
    (flet ((asm (tag opcode a b output)
             (let ((ar (elt registers a))
                   (br (elt registers b)))
               (list tag
                     `(setf ,(elt registers output)
                            ,(case opcode
                               (addr `(+ ,ar ,br))
                               (addi `(+ ,ar ,b))
                               (mulr `(* ,ar ,br))
                               (muli `(* ,ar ,b))
                               (banr `(logand ,ar ,br))
                               (bani `(logand ,ar ,b))
                               (borr `(logior ,ar ,br))
                               (bori `(logior ,ar ,b))
                               (setr ar)
                               (seti a)
                               (gtir `(if (> ,a ,br) 1 0))
                               (gtri `(if (> ,ar ,b) 1 0))
                               (gtrr `(if (> ,ar ,br) 1 0))
                               (eqri `(if (= ,ar ,b) 1 0))
                               (eqir `(if (= ,a ,br) 1 0))
                               (eqrr `(if (= ,ar ,br) 1 0))))
                     '(go next)))))
      `(lambda (r0)
         (declare (optimize (speed 3) (space 0) (safety 0) (debug 0)))
         (let (,@(rest (mapcar (lambda (x) (list x 0)) registers)))
           (declare (fixnum ,@registers))
           (tagbody
              (go dispatch)
              ,@(mapcan (a:curry #'apply #'asm) tags instructions)
            next
              (incf ip)
            dispatch
              ,@(when escape
                  `((when (zerop r0)
                      (setf r0 (max ,@registers) ip -1))))
              (case ip ,@(mapcar (lambda (tag) `(,tag (go ,tag))) tags)))
           r0)))))

(defun part1 (input)
  (multiple-value-bind (instructions ip-reg-ix) (parse input)
    (funcall (eval (assemble instructions ip-reg-ix)) 0)))

(defun sum-of-divisors (n)
  (loop for i from 1 to n when (zerop (mod n i)) sum i))

(defun part2 (input)
  (multiple-value-bind (instructions ip-reg-ix) (parse input)
    (sum-of-divisors (funcall (eval (assemble instructions ip-reg-ix :escape t)) 1))))
