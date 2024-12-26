;;;; day17.lisp

(in-package :aoc2024.day17)

(defun parse (input)
  (let ((sections (aoc:sections input)))
    (append (a:flatten (mapcar #'aoc:read-integers (aoc:lines (first sections))))
            (list (apply #'vector (aoc:read-integers (second sections)))))))

(defun instruction (opcode)
  (getf '(0 adv
          1 bxl
          2 bst
          3 jnz
          4 bxc
          5 out
          6 bdv
          7 cdv)
        opcode))

(defun operand (x a b c)
  (case x
    ((0 1 2 3) x)
    (4 a)
    (5 b)
    (6 c)))

(defun run (a b c memory)
  (let ((outputs (make-array (length memory) :fill-pointer 0))
        (ip 0))
    (labels ((ref (adr) (and (array-in-bounds-p memory adr) (aref memory adr)))
             (literal () (ref (1+ ip)))
             (combo () (operand (literal) a b c))
             (dv () (ash a (- (combo)))))
      (loop for instruction = (instruction (ref ip))
            for next = (+ ip 2)
            while instruction
            do (ecase instruction
                 (adv (setf a (dv)))
                 (bxl (setf b (logxor b (literal))))
                 (bst (setf b (logand #o7 (combo))))
                 (jnz (or (zerop a) (setf next (literal))))
                 (bxc (setf b (logxor b c)))
                 (out (vector-push (logand #o7 (combo)) outputs))
                 (bdv (setf b (dv)))
                 (cdv (setf c (dv))))
               (setf ip next))
      (values outputs (list a b c)))))

(defun part1 (input)
  (format nil "~{~o~^,~}" (coerce (apply #'run (parse input)) 'list)))

(defun disasm (memory)
  (loop for (o x) on (coerce memory 'list) by #'cddr
        collect (list (instruction o) (operand x 'a 'b 'c))))

(defun quine (memory a)
  (loop for x from a repeat 8
        for output = (run x 0 0 memory)
        when (and (end-match? memory output)
                  (or (and (eql (length memory) (length output)) x)
                      (quine memory (ash x 3))))
          return it))

(defun end-match? (memory output)
  (loop for m across (reverse memory)
        for o across (reverse output)
        always (eql m o)))

(defun part2 (input)
  (quine (car (last (parse input))) 0))

;; (BST A) 
;; (BXL 1) 
;; (CDV B) 
;; (ADV 3) 
;; (BXL A) 
;; (BXC B) 
;; (OUT B) 
;; (JNZ 0) 
