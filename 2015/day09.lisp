;;;; day09.lisp

(in-package :aoc2015.day09)

(defun distances (input)
  (let ((*package* (symbol-package 'distances))
        (distances (make-hash-table :test 'equal))
        nodes)
    (dolist (line (aoc:lines input))
      (destructuring-bind (a to b = c) (aoc:read-as-list line)
        (declare (ignore to =))
        (setf (gethash (key a b) distances) c
              (gethash (key a) distances) 0
              (gethash (key b) distances) 0)
        (pushnew a nodes)
        (pushnew b nodes)))
    (values distances nodes)))

(defun key (a &optional b)
  (cond ((null a) b)
        ((null b) a)
        ((string< (symbol-name a) (symbol-name b))
         (cons a b))
        (t
         (cons b a))))

(defun travel (distances nodes &optional (test #'<))
  (let (best-distance best-path)
    (labels ((distance (a b)
               (gethash (key a b) distances))
             (rec (nodes path distance next)
               (cond ((null nodes)
                      (when (or (null best-distance) (funcall test distance best-distance))
                        (setf best-distance distance
                              best-path path))
                      (funcall next))
                     (t (labels ((next (options)
                                   (if (null options)
                                       (funcall next)
                                       (rec (remove (car options) nodes)
                                            (cons (car options) path)
                                            (+ distance (distance (car options) (car path)))
                                            (lambda () (next (cdr options)))))))
                          (next nodes))))))
      (rec nodes nil 0 (lambda () (values best-distance (nreverse best-path)))))))

(defun part1 (input)
  (multiple-value-call #'travel (distances input)))

(defun part2 (input)
  (multiple-value-call #'travel (distances input) #'>))
