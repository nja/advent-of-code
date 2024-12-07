;;;; day22.lisp

(in-package #:aoc2016.day22)

(defstruct (node :conc-name) x y used avail)

(defun parse (input)
  (let (result)
    (ppcre:do-register-groups ((#'parse-integer x y used avail))
        ("/dev/grid/node-x(\\d+)-y(\\d+) +\\d+T +(\\d+)T +(\\d+)T" input)
      (push (make-node :x x :y y :used used :avail avail) result))
    (nreverse result)))

(defun viable? (src dst)
  (and (not (empty? src))
       (not (eq src dst))
       (<= (used src) (avail dst))))

(defun empty? (node)
  (zerop (used node)))

(defun pairs (nodes)
  (mapcan (lambda (x) (mapcar (a:curry #'list x) nodes)) nodes))

(defun part1 (input)
  (count-if (a:curry #'apply #'viable?) (pairs (parse input))))

(defstruct (state :conc-name) data empty grid)

(defun pos (x y)
  (make-node :x x :y y))

(defun init-state (nodes)
  (let* ((max-x (reduce #'max nodes :key #'x))
         (max-y (reduce #'max nodes :key #'y))
         (grid (make-array (list (1+ max-y) (1+ max-x))))
         (empty (find-if #'empty? nodes))
         (data (find-if (lambda (n) (and (zerop (y n)) (eq max-x (x n)))) nodes)))
    (dolist (n nodes)
      (setf (aref grid (y n) (x n))
            (cond ((eq empty n) #\E)
                  ((eq data n) #\D)
                  ((ok? n data) #\.)
                  (t #\#))))
    (make-state :data (pos (x data) (y data))
                :empty (pos (x empty) (y empty))
                :grid grid)))

(defun ok? (a b)
  (and (<= (used a) (size b))
       (<= (used b) (size a))))

(defun size (node)
  (+ (avail node) (used node)))

(defun distance (a b)
  (+ (abs (- (x a) (x b)))
     (abs (- (y a) (y b)))))

(defun neighbours (state)
  (let (result)
    (flet ((collect (x y)
             (when (and (array-in-bounds-p (grid state) y x)
                        (eql #\. (aref (grid state) y x))
                        (or (not (eql x (x (data state))))
                            (not (eql y (y (data state))))))
               (push (make-state :empty (pos x y)
                                 :data (data state)
                                 :grid (grid state))
                     result))))
      (collect (1+ (x (empty state))) (y (empty state)))
      (collect (1- (x (empty state))) (y (empty state)))
      (collect (x (empty state)) (1+ (y (empty state))))
      (collect (x (empty state)) (1- (y (empty state)))))
    (when (eql 1 (distance (empty state) (data state)))
      (push (make-state :empty (data state)
                        :data (empty state)
                        :grid (grid state))
            result))
    result))

(defun done? (state)
  (and (zerop (x (data state)))
       (zerop (y (data state)))))

(defun part2 (input)
  (dijkstra:distance (dijkstra:search* (init-state (parse input)) #'neighbours :donep #'done?)))
