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

(defun close-in (a b prize)
  (destructuring-bind (ax ay) a
    (destructuring-bind (bx by) b
      (destructuring-bind (px py) prize
        (loop for as from 0 by 1
              for x from 0 by ax
              for y from 0 by ay
              when (and (zerop (rem (- px x) bx))
                        (zerop (rem (- py y) by)))
              for bs = (presses x y bx by px py)
              while (and (<= x px) (<= y py))
              when bs
                minimize (+ (* as 3) bs))))))

(defun factor (ar br)
  (/ (* (numerator ar) (denominator br))
     (* (numerator br) (denominator ar))))

(defun shortcut-presses (a b)
  (destructuring-bind (ax ay) a
    (destructuring-bind (bx by) b
      (let ((f (factor (apply #'/ a) (apply #'/ b))))
        (values (truncate 10000000000000 (numerator f))
                (truncate 10000000000000 (denominator f)))))))

(defun part2 (input)
  (mapcar (lambda (machine)
            (ration (first machine) (second machine)))
          (parse input)))

(defun hmm (machines)
  (some (lambda (machine)
          (destructuring-bind (a b prize) machine
            (and (> (first a) (first b))
                 (> (second a) (second b)))))
        machines))

(defun diagonal-presses (a b)
  (loop for d from 0 by (lcm (first a) (first b) (second a) (second b))
        for p = (minimize-tokens a b (list d d))
        when (plusp p)
          return p))


(defun diagonal-presses (a b prize)
  (destructuring-bind (ax ay) a
    (destructuring-bind (bx by) b
      (destructuring-bind (px py) prize
        (loop for as from 0
              for x from 0 by ax
              for y from 0 by ay
              for bs = (diags x y bx by)
              while (and (<= x px) (<= y py))
;              while (<= as 100)
              when bs
                minimize (+ (* as 3) bs))))))

(defun argh (x1 y1 x2 y2)
  (loop for a from 2
        for num = (* a (- x1 y1))
        when (zerop (rem num (- y2 x2)))
          return (values a (/ num (- y2 x2)))))

(defun slopes (machine)
  (sort (list (apply #'/ (first machine))
              (apply #'/ (second machine))
              1/2)
        #'<))

(defun diagonal-presses (a b)
  (destructuring-bind (ax ay) a
    (destructuring-bind (bx by) b
      (let* ((as (/ ay ax))
             (bs (/ by bx))
             (+ (max as bs))
             (- (min as bs))
             (ap 0)
             (bp 1)
             (x bx)
             (y by))
        (flet ((a ()
                 (incf ap)
                 (incf x ax)
                 (incf y ay))
               (b ()
                 (incf bp)
                 (incf x bx)
                 (incf y by)))
          (cond ((= bs 1)
                 (values 0 1))
                ((= as 1)
                 (values 1 0))
                ((<= - 1 +)
                 (loop for s = (/ y x)
                       when (= s 1)
                         return (values ap bp)
                       when (< s 1)
                         do (if (eq bs +)
                                (b)
                                (a))
                       when (> s 1)
                         do (if (eq bs -)
                                (b)
                                (a))))))))))

