;;;; day21.lisp

(in-package #:aoc2016.day21)

(defun parse (input)
  (mapcar #'parse-line (aoc:lines input)))

(defun parse-line (line)
  (cond ((str:starts-with? "swap position" line)
         (destructuring-bind (x y) (clean line)
           (list 'swap-pos x y)))
        ((str:starts-with? "swap letter" line)
         (ppcre:register-groups-bind (x y) ("swap letter (.) with letter (.)" line)
           (list 'swap-char (coerce x 'character) (coerce y 'character))))
        ((str:starts-with? "rotate left" line)
         (cons 'rotate (clean line)))
        ((str:starts-with? "rotate right" line)
         (cons 'rotate (mapcar #'- (clean line))))
        ((str:starts-with? "rotate based" line)
         (ppcre:register-groups-bind (x) ("rotate based on position of letter (.)" line)
           (list 'rotate-based-on-char (coerce x 'character))))
        ((str:starts-with? "reverse" line)
         (cons 'reverse-positions (clean line)))
        ((str:starts-with? "move" line)
         (cons 'move-position (clean line)))))

(defun clean (line)
  (mapcar #'parse-integer (remove-if (a:curry #'string= "") (ppcre:split "[^0-9]+" line))))


(defun swap-pos (s x y)
  (rotatef (aref s x) (aref s y))
  s)

(defun swap-char (s x y)
  (swap-pos s (position x s) (position y s))
  s)

(defun rotate (s n)
  (loop with copy = (copy-seq s)
        for i below (length s)
        do (setf (aref s i) (aref copy (mod (+ i n) (length s))))
        finally (return s)))

(defun rotate-based-on-char (s c)
  (let* ((pos (position c s))
         (n (- (+ 1 pos (if (>= pos 4) 1 0)))))
    (rotate s n)))

(defun reverse-positions (s lo hi)
  (loop for i from 0
        repeat (ceiling (- hi lo) 2)
        do (rotatef (aref s (+ lo i)) (aref s (- hi i)))
        finally (return s)))

(defun move-position (s from to)
  (let ((tmp (aref s from)))  
    (loop for i from from below (1- (length s))
          do (setf (aref s i) (aref s (1+ i))))
    (loop for i from (1- (length s)) downto (1+ to)
          do (setf (aref s i) (aref s (1- i))))
    (setf (aref s to) tmp)
    s))

(defun scramble (s op)
  (apply (first op) s (rest op)))

(defun scrambler (rules)
  (lambda (word)
    (let ((s (copy-seq word)))
      (mapc (a:curry #'scramble s) rules)
      s)))

(defun part1 (input)
  (funcall (scrambler (parse input)) "abcdefgh"))

(defun find-password (password rules)
  (let ((scrambler (scrambler rules)))
    (a:map-permutations
     (lambda (p)
       (when (string= password (funcall scrambler p))
         (return-from find-password p)))
     password)))

(defun part2 (input)
  (find-password "fbgdceah" (parse input)))
