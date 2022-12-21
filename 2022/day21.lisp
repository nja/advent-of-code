;;;; day21.lisp

(in-package :aoc2022.day21)

(defun parse (input)
  (let ((*package* (symbol-package 'parse)))
    (mapcar (lambda (line)
              (let ((description (read-from-string (format nil "(~a)" (remove #\: line)))))
                (case (length description)
                  (2 (destructuring-bind (id n) description
                       (list id n)))
                  (4 (destructuring-bind (id a op b) description
                       (list id op a b))))))
            (aoc:lines input))))

(defun monkey (op &optional a b)
  (lambda (env)
    (flet ((a () (get-value env a))
           (b () (get-value env b)))
      (case op
        (= (- (a) (b)))
        ((+ - / *) (funcall op (a) (b)))
        (t op)))))

(defun get-value (env id)
  (funcall (gethash id env) env))

(defun make-env (descriptions)
  (let ((env (make-hash-table)))
    (dolist (description descriptions env)
      (setf (gethash (first description) env)
            (apply #'monkey (rest description))))))

(defun part1 (input)
  (get-value (make-env (parse input)) 'root))

(defun patch-root (descriptions)
  (mapcar (lambda (x)
            (if (eq 'root (first x))
                (append '(root =) (cddr x))
                x))
          descriptions))

(defun oracle (env)
  (lambda (x)
    (setf (gethash 'humn env) (monkey x))
    (get-value env 'root)))

(defun improve (answer guess)
  (+ guess (truncate answer 7)))

(defun solve (oracle)
  (labels ((rec (guess)
             (let ((answer (funcall oracle guess)))
               (cond ((zerop answer) guess)
                     (t (rec (improve answer guess)))))))
    (rec 0)))

(defun part2 (input)
  (solve (oracle (make-env (patch-root (parse input))))))
