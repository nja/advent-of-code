;;;; day13.lisp

(in-package :aoc2022.day13)

(defun parse (input)
  (with-input-from-string (in input)
    (labels ((peek () (peek-char t in nil 'eof))
             (eat () (read-char in))
             (digit () (digit-char-p (eat)))
             (integer ()
               (loop for integer = (digit) then (+ (digit) (* integer 10))
                     while (and (characterp (peek)) (digit-char-p (peek)))
                     finally (return integer)))
             (thing ()
               (case (peek)
                 (#\[ (eat) (things))
                 (#\, (eat) (thing))
                 (#\] (eat) 'eol)
                 (eof 'eof)
                 (t (integer))))
             (things ()
               (loop for thing = (thing)
                     until (or (eq 'eol thing) (eq 'eof thing))
                     collect thing)))
      (things))))

(defun parse* (input)
  (read-from-string (format nil "(~a)" (aoc:tr "[,]" "( )" input))))

(defun compare (l r)
  (cond ((and (integerp l) (integerp r))
         (cond ((< l r) 'right)
               ((> l r) 'wrong)))
        ((or (integerp l) (integerp r))
         (compare (a:ensure-list l)
                  (a:ensure-list r)))
        ((and (null l) r) 'right)
        ((and l (null r)) 'wrong)
        ((and (null l) (null r)) nil)
        (t (or (compare (first l) (first r))
               (compare (rest l) (rest r))))))

(defun ordered? (a b)
  (not (eq 'wrong (compare a b))))

(defun sum-ordered-indices (packets)
  (loop for (a b) on packets by #'cddr
        for i from 1
        when (ordered? a b)
          sum i))

(defun part1 (input)
  (sum-ordered-indices (parse input)))

(defun dividers (&rest list)
  (mapcar #'list list))

(defun order (packets)
  (sort (append packets (dividers 2 6)) #'ordered?))

(defun decoder-key (packets)
  (reduce #'* (mapcar (lambda (p)
                        (1+ (position p packets :test #'equal)))
                      (dividers 2 6))))

(defun part2 (input)
  (decoder-key (order (parse input))))
