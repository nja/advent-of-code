;;;; day18.lisp

(in-package :aoc2019.day18)

(defparameter *test*
"#########
#b.A.@.a#
#########")

(defun parse (input)
  (aoc:to-array input))

(defun start-position (array)
  (list (loop for i below (array-total-size array)
              when (eql #\@ (row-major-aref array i))
                return (multiple-value-list (truncate i (array-dimension array 1))))))

(defun keys (array)
  (sort (loop for i below (array-total-size array)
              for c = (row-major-aref array i)
              when (lower-case-p c)
                collect c)
        #'char<))

(defun donep (array)
  (let ((keys (keys array)))
   (lambda (node)
     (let ((haves (rest node)))
       (every (lambda (key) (find key haves)) keys)))))

(defun neighbours (array node)
  (loop with ((row col) . keys) = node
        for (dr dc) in '((1 0) (-1 0) (0 1) (0 -1))
        for nr = (+ row dr)
        for nc = (+ col dc)
        for x = (and (array-in-bounds-p array nr nc)
                     (aref array nr nc))
        if (or (find x ".@")
               (and (upper-case-p x) (find (char-downcase x) keys)))
          collect (cons (list nr nc) keys)
        if (lower-case-p x)
          collect (cons (list nr nc) (with-key keys x))))

(defun with-key (keys new)
  (if (find new keys)
      keys
      (sort (cons new (copy-seq keys)) #'char<)))

(defun part1 (input)
  (let ((array (aoc:to-array input)))
    (dijkstra:distance (dijkstra:search* (start-position array)
                                         (a:curry #'neighbours array)
                                         :donep (donep array)))))
