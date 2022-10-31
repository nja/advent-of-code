;;;; day16.lisp

(in-package #:aoc2018.day16)

(defun registers (&rest contents)
  (if (null contents)
      (make-array 4 :initial-element 0)
      (make-array 4 :initial-contents contents)))

(defun operate (registers opcode a b output)
  (let ((registers (a:copy-array registers)))
    (flet ((a () (aref registers a))
           (b () (aref registers b))
           (test (p) (if p 1 0)))
      (setf (aref registers output)
       (case opcode
         (addr (+ (a) (b)))
         (addi (+ (a) b))
         (mulr (* (a) (b)))
         (muli (* (a) b))
         (banr (logand (a) (b)))
         (bani (logand (a) b))
         (borr (logior (a) (b)))
         (bori (logior (a) b))
         (setr (a))
         (seti a)
         (gtir (test (> a (b))))
         (gtri (test (> (a) b)))
         (gtrr (test (> (a) (b))))
         (eqri (test (= (a) b)))
         (eqir (test (= a (b))))
         (eqrr (test (= (a) (b))))))
      registers)))

(defparameter *opcodes*
  (vector 'addr 'addi
          'mulr 'muli
          'banr 'bani
          'borr 'bori
          'setr 'seti
          'gtir 'gtri 'gtrr
          'eqri 'eqir 'eqrr))

(defun clean (line)
  (mapcar #'parse-integer (remove-if (a:curry #'string= "") (ppcre:split "[^0-9]+" line))))

(defun sample-sections (input)
  (remove-if-not (lambda (x) (and (< 0 (length x)) (char= #\B (aref x 0))))
                 (aoc:sections input)))

(defun parse-sample (section)
  (let ((lines (mapcar #'clean (aoc:lines section))))
    (list (apply #'registers (first lines))
          (second lines)
          (apply #'registers (third lines)))))

(defun sample-opcodes (sample)
  (destructuring-bind (before (x a b c) after) sample
    (cons x (loop for opcode across *opcodes*
                  for result = (operate before opcode a b c)
                  when (equalp result after)
                    collect opcode))))

(defun sample (input)
  (mapcar (a:compose #'sample-opcodes #'parse-sample)
          (sample-sections input)))

(defun part1 (input)
  (count-if (lambda (x) (< 3 (length x))) (sample input)))

(defun unify (opcodes sample)
  (destructuring-bind (code &rest s) sample
    (when (= 1 (length (setf (aref opcodes code)
                             (intersection (aref opcodes code) s))))
      (loop with op = (car (aref opcodes code))
            for i below (length opcodes)
            unless (= code i)
              do (setf (aref opcodes i) (delete op (aref opcodes i)))))
    opcodes))

(defun opcodes ()
  (map 'vector (lambda (x) (declare (ignore x)) (coerce *opcodes* 'list))
       *opcodes*))

(defun work-out-opcodes (samples)
  (let ((opcodes (opcodes)))
    (dolist (sample samples)
      (unify opcodes sample))
    (map 'vector #'car opcodes)))

(defun instructions (input)
  (mapcar #'clean (aoc:lines (car (last (aoc:sections input))))))

(defun execute (instructions opcodes)
  (loop for (op a b c) in instructions
        for registers = (operate (or registers (registers)) (aref opcodes op) a b c)
        finally (return registers)))

(defun part2 (input)
  (aref (execute (instructions input) (work-out-opcodes (sample input))) 0))
