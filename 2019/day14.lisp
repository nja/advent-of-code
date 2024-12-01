;;;; day14.lisp

(in-package :aoc2019.day14)

(defun reactions (input)
  (let ((reactions (make-hash-table)))
    (flet ((plist (s) (reverse (read-from-string (format nil "(~a)" (aoc:tr "," "  " s))))))
      (mapc (lambda (line)
              (destructuring-bind (inputs outputs)
                  (mapcar #'plist (str:split "=>" line))
                (setf (gethash (car outputs) reactions)
                      (list (apply #'bag inputs) (apply #'bag outputs) (second outputs)))))
            (aoc:lines input))
      reactions)))

(defun bag (&rest plist)
  (fset:convert 'fset:bag (a:plist-alist plist) :from-type 'fset:alist))

(defun mine (reactions stock needs mined)
  (if (fset:empty? needs)
      mined
      (let ((spent (fset:intersection stock needs)))
        (if (fset:nonempty? spent)
            (mine reactions
                  (fset:bag-difference stock spent)
                  (fset:bag-difference needs spent)
                  mined)
            (multiple-value-bind (material n) (fset:arb needs)
              (if (eq material 'ore)
                  (mine reactions
                        stock
                        (fset:bag-difference needs (bag 'ore n))
                        (+ mined n))
                  (destructuring-bind (inputs outputs x) (gethash material reactions)
                    (declare (ignore outputs))
                    (multiple-value-bind (q r) (ceiling n x)
                      (mine reactions
                            (fset:with stock material (- r))
                            (fset:bag-sum (fset:less needs material n) (mul inputs q))
                            mined)))))))))

(defun mul (bag n)
  (gmap:gmap :bag-pairs (lambda (x c) (values x (* n c)))
             (:bag-pairs bag)))

(defun cost (reactions fuel)
  (mine reactions (bag) (bag 'fuel fuel) 0))

(defun part1 (input)
  (cost (reactions input) 1))

(defun predicate (reactions ore)
  (lambda (fuel)
    (<= (cost reactions fuel) ore)))

(defun fork (predicate lo &optional hi)
  (cond ((eq lo hi) lo)
        ((null hi)
         (let ((hi (* lo 2)))
           (if (funcall predicate hi)
               (fork predicate hi)
               (fork predicate lo hi))))
        (t (let ((mid (ceiling (+ lo hi) 2)))
             (if (funcall predicate mid)
                 (fork predicate mid hi)
                 (fork predicate lo (1- mid)))))))

(defun part2 (input)
  (let* ((reactions (reactions input))
         (ore 1000000000000))
    (fork (predicate reactions ore) 1)))
