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
                    (let ((f (ceiling n x)))
                      (mine reactions
                            (fset:bag-sum stock (mul outputs f))
                            (fset:bag-sum needs (mul inputs f))
                            mined)))))))))

(defun mul (bag n)
  (gmap:gmap :bag-pairs (lambda (x c) (values x (* n c)))
             (:bag-pairs bag)))


(defun part1 (input)
  (mine (reactions input) (bag) (bag 'fuel 1) 0))