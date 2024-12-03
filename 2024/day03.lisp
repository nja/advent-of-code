;;;; day03.lisp

(in-package :aoc2024.day03)

(defun parse (input)
  (let ((expressions (ppcre:all-matches-as-strings "mul\\(\\d+,\\d+\\)" input)))
    (mapcar (lambda (expr)
              (read-from-string (aoc:tr "mul," "    " expr)))
            expressions)))

(defun part1 (input)
  (reduce #'+ (mapcar (lambda (x) (apply #'* x)) (parse input))))


(defun instructions (s)
  (setf s (wrap s))
  (loop for thing = (cond ((try-read s "mul(")
                           (or (try-read-pair s) 'junk))
                          ((try-read s "do()")
                           'enable)
                          ((try-read s "don't()")
                           'disable)
                          ((eat s)
                           'junk))
        while thing
        unless (eq thing 'junk) collect thing))

(defun try-read-pair (s)
  (let ((a (read-integer s))
        (comma (try-read s ","))
        (b (read-integer s))
        (paren (try-read s ")")))
    (and a comma b paren (list '* a b))))

(defun read-integer (s)
  (loop with i
        for c = (peek s)
        for d = (and (characterp c) (digit-char-p c))
        while d
        do (eat s)
           (setf i (+ d (* (or i 0) 10)))
        finally (return i)))

(defun try-read (s what)
  (let (read)
    (if (loop for c across what
              always (eql c (peek s))
              do (push (eat s) read))
        what
        (and (unpeek s read) nil))))

(defun wrap (s)
  (typecase s
    (cons s)
    (string (wrap (make-string-input-stream s)))
    (stream (cons nil s))))

(defun peek (s)
  (or (caar s)
      (peek-char nil (cdr s) nil)))

(defun eat (s)
  (or (pop (car s))
      (read-char (cdr s) nil)))

(defun unpeek (s c)
  (if (typep c 'sequence)
      (map nil (a:curry #'unpeek s) c)
      (push c (car s)))
  s)

(defun evaluate (instructions)
  (let ((enabled 1))
    (reduce #'+ (mapcar (lambda (x)
                          (case x
                            (enable (setf enabled 1) 0)
                            (disable (setf enabled 0) 0)
                            (t (reduce #'* (cons enabled (rest x))))))
                        instructions))))

(defun part2 (input)
  (evaluate (instructions input)))
