;;;; day13.lisp

(in-package :aoc2024.day13)

(defun parse (input)
  (mapcar (lambda (s) (mapcar #'aoc:read-integers (aoc:lines s))) (aoc:sections input)))

(defun minimize-tokens (a b prize &key (startx 0) (starty 0))
  (destructuring-bind (ax ay) a
    (destructuring-bind (bx by) b
      (destructuring-bind (px py) prize
        (loop for as from 0
              for x from startx by ax
              for y from starty by ay
              for bs = (presses x y bx by px py)
              while (and (<= x px) (<= y py))
              when bs
                minimize (+ (* as 3) bs))))))

(defun presses (x y bx by px py)
  (loop for presses from 0
        while (and (<= x px) (<= y py))
        when (and (= x px) (= y py))
          return presses
        do (incf x bx)
           (incf y by)))

(defun part1 (input)
  (reduce #'+ (mapcar (a:curry #'apply #'minimize-tokens) (parse input))))

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

(defparameter *bignum* (- 10000000000000 20000))

(defun skip (machine)
  (destructuring-bind (a b prize) machine
    (declare (ignore prize))
    (multiple-value-bind (ap bp) (diagonal-presses a b)
      (when (and ap bp)
        (let ((skipx (+ (* ap (first a))
                        (* bp (first b))))
              (skipy (+ (* ap (second a))
                        (* bp (second b)))))
          (assert (equal skipx skipy))
          (multiple-value-bind (skips rest) (truncate *bignum* skipx)
            (values (* skips ap) (* skips bp) (* skipx skips))))))))

(defun part2 (input)
  (reduce #'+(mapcar (lambda (m)
             (multiple-value-bind (ap bp start) (skip m)
               (if (and ap bp start)
                   (destructuring-bind (a b prize) m
                     (let ((mt (minimize-tokens a b (mapcar (a:curry #'+ 10000000000000) prize) :startx start :starty start)))
                       (if (zerop mt)
                           0
                           (+ (* 3 ap) bp mt))))
                   0)))
           (parse input))))
