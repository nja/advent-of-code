;;;; day25.lisp

(in-package #:aoc2018.day25)

(defstruct (pos :conc-name (:constructor pos (x y z q))) z y x q)

(defun parse (input)
  (mapcar (a:compose (a:curry #'apply #'pos) #'clean) (aoc:lines input)))

(defun clean (line)
  (mapcar #'parse-integer (remove-if (a:curry #'string= "") (ppcre:split "[^0-9-]+" line))))

(defun union-find (items predicate)
  (labels
      ((find* (node)
         (if (null (cdr node))
             node
             (cdr (rplacd node (find* (cdr node))))))
       (union* (a b)
         (let ((root-a (find* a))
               (root-b (find* b)))
           (unless (eq root-a root-b)
             (rplacd root-a root-b))))
       (union-find* (nodes)
         (when nodes
           (mapc (let ((n (first nodes)))
                   (lambda (x)
                     (when (funcall predicate (car n) (car x))
                       (union* n x))))
                 (union-find* (rest nodes)))
           nodes)))
    (union-find* (mapcar #'list items))))

(defun near? (a b)
  (<= (distance a b) 3))

(defun distance (a b)
  (+ (abs (- (x a) (x b)))
     (abs (- (y a) (y b)))
     (abs (- (z a) (z b)))
     (abs (- (q a) (q b)))))

(defun part1 (input)
  (length (remove-duplicates (union-find (parse input) #'near?) :key #'last)))
