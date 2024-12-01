;;;; day06.lisp

(in-package :aoc2019.day06)

(defun parse (input)
  (let ((*package* (symbol-package 'parse)))
    (mapcar (lambda (line)
              (mapcar #'intern (str:split ")" line)))
            (aoc:lines input))))

(defun system (orbits)
  (let ((bodies (mapcar #'list (remove-duplicates (a:flatten orbits)))))
    (flet ((body (name) (find name bodies :key #'car)))
      (dolist (orbit orbits (body 'com))
        (destructuring-bind (primary secondary) orbit
          (push (body secondary) (cdr (body primary))))))))

(defun count-orbits (system)
  (labels ((rec (s d acc cont)
             (if (null s)
                 (funcall cont acc)
                 (labels ((next (bodies acc)
                            (if (null bodies)
                                (funcall cont acc)
                                (rec (car bodies) (1+ d) acc
                                     (lambda (acc)
                                       (next (cdr bodies) acc))))))
                   (next (cdr s) (+ d acc))))))
    (rec system 0 0 #'identity)))

(defun part1 (input)
  (count-orbits (system (parse input))))

(defun orbital-transfers-required (system a b)
  (let ((a (orbits system a))
        (b (orbits system b)))
    (+ (length (set-difference a b))
       (length (set-difference b a)))))

(defun orbits (system body)
  (labels ((rec (s path cont)
             (cond ((eq s body)
                    (cdr path))
                   ((atom s)
                    (funcall cont))
                   (t
                    (labels ((next (orbits path)
                               (if (null orbits)
                                   (funcall cont)
                                   (rec (car orbits) path
                                        (lambda ()
                                          (next (cdr orbits) path))))))
                      (next s (cons (car s) path)))))))
    (rec system nil #'identity)))

(defun part2 (input)
  (orbital-transfers-required (system (parse input)) 'you 'san))
