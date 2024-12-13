;;;; day13.lisp

(in-package :aoc2024.day13)

(defparameter *test*
"Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279")

(defun parse (input)
  (mapcar (lambda (s) (mapcar #'aoc:read-integers (aoc:lines s))) (aoc:sections input)))

(defun tokens (a b prize)
  )

(defun presses (a b p)
  (let ((max (max a b))
        (min (min a b)))
    (/ (rem p max)
       min)))

(defun minimize-tokens (a b prize)
  (destructuring-bind (ax ay) a
    (destructuring-bind (bx by) b
      (destructuring-bind (px py) prize
        (loop for as from 0
              for x from 0 by ax
              for y from 0 by ay
              for bs = (presses x y bx by px py)
              while (and (<= x px) (<= y py))
;              while (<= as 100)
              when bs
                minimize (+ (* as 3) bs))))))

(defun presses (x y bx by px py)
  (loop for presses from 0
        while (and (<= x px) (<= y py))
;        while (<= presses 100)
        when (and (= x px) (= y py))
          return presses
        do (incf x bx)
           (incf y by)))

(defun part1 (input)
  (reduce #'+ (mapcar (a:curry #'apply #'minimize-tokens) (parse input))))

(defun coefficients (a b)
  (loop with array = (make-array (list (length a) 2))
        for an in a
        for bn in b
        for i from 0
        do (setf (aref array 0 i) an
                 (aref array 1 i) bn)
        finally (return array)))