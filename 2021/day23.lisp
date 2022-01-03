;;;; day23.lisp

(in-package #:aoc2021.day23)

(defun parse-input (input)
  (remove-if-not (a:rcurry #'find "ABCD.") input))

(defun print-state (state)
  (format t "            1 ~%")
  (format t "  01234567890 ~%")
  (format t " #############~%")
  (format t "0#~a#~%" (subseq state 0 11))
  (format t "1###~{~a~^#~}###~%" (coerce (subseq state 11 15) 'list))
  (loop for start = 15 then end and end = 19 then (+ end 4)
        for i from 2
        while (<= end (length state))
        do (format t "~d  #~{~a~^#~}#  ~%" i (coerce (subseq state start end) 'list)))
  (format t "   #########  ~%"))

(defun outside-room? (i)
  (and (evenp i) (<= 2 i 8)))

(defun free? (state i)
  (eql #\. (aref state i)))

(defun swaps (state)
  (let (result)
    (flet ((to-burrows ()
             (loop for i below 11
                   for ch across state
                   for home = (home-col ch)
                   for desti = (and home (home-free? state ch))
                   when (and desti (hallway-free? state i home))
                     do (push (cons desti i) result)))
           (to-hallway (pod)
             (loop for i from (home-index pod) by 4
                     below (length state)
                   for ch = (aref state i)
                   when (not (eql #\. ch))
                     do (loop for desti from (1- (home-col pod)) downto 0
                              always (free? state desti)
                              unless (outside-room? desti)
                                do (push (cons i desti) result))
                        (loop for desti from (1+ (home-col pod)) upto 10
                              always (free? state desti)
                              unless (outside-room? desti)
                                do (push (cons i desti) result))
                        (return))))
      (to-burrows)
      (map nil #'to-hallway "ABCD")
      result)))

(defun home-col (pod) (case pod (#\A 2) (#\B 4) (#\C 6) (#\D 8)))
(defun home-index (pod) (ecase pod (#\A 11) (#\B 12) (#\C 13) (#\D 14)))

(defun do-swap (state swap)
  (let ((copy (a:copy-array state)))
    (rotatef (aref copy (car swap)) (aref copy (cdr swap)))
    copy))

(defun hallway-free? (state a b)
  (loop for i from (1+ (min a b)) upto (1- (max a b))
        always (eql #\. (aref state i))))

(defun home-free? (state pod)
  (loop for i from (ecase pod (#\A 11) (#\B 12) (#\C 13) (#\D 14)) by 4 below (length state)
        for ch = (aref state i)
        when (eql #\. ch)
          maximize i into max-free
        always (or (eql #\. ch) (eql ch pod))
        finally (return (unless (zerop max-free) max-free))))

(defun cost-factor (state i)
  (case (aref state i) (#\B 10) (#\C 100) (#\D 1000) (t 1)))

(defun cost (state swap)
  (destructuring-bind (a . b) swap
    (* (cost-factor state a)
       (cost-factor state b)
       (+ (abs (- (index-row a) (index-row b)))
          (abs (- (index-col a) (index-col b)))))))

(defun index-row (i) (max 0 (ceiling (- i 10) 4)))
(defun index-col (i)
  (if (< i 11)
      i
      (* 2 (1+ (mod (- i 11) 4)))))

(defparameter *target* "...........ABCDABCD")

(defstruct node key distance qindex previous visited)

(defun compare (x y)
  (cond ((null x) nil)
        ((null y) t)
        (t (< x y))))

(defun make-queue ()
  (q:make-empty-queue
   (lambda (a b) (compare (node-distance a) (node-distance b)))
   (lambda (x i) (setf (node-qindex x) i))
   #'node-qindex))

(defun get-node (key nodes &optional distance)
  (or (gethash key nodes)
      (setf (gethash key nodes) (make-node :key key :distance distance))))

(defun neighbours (key nodes)
  (loop for swap in (swaps key)
        for next-node = (get-node (do-swap key swap) nodes)
        unless (node-visited next-node)
          collect (cons (cost key swap) next-node)))

(defun search* (source target)
  (loop with queue = (make-queue)
        with nodes = (make-hash-table :test 'equal)
          initially (q:queue-insert queue (get-node source nodes 0))
        until (q:queue-empty-p queue)
        for current = (q:queue-pop queue)
        when (equalp target (node-key current))
          do (return current)
        do (let ((neighbours (neighbours (node-key current) nodes)))
             (loop for (cost . neighbour) in neighbours
                   for alt = (+ (node-distance current) cost)
                   when (compare alt (node-distance neighbour))
                     do (setf (node-distance neighbour) alt
                              (node-previous neighbour) current)
                        (if (node-qindex neighbour)
                            (q:queue-update queue neighbour)
                            (q:queue-insert queue neighbour))))
           (setf (node-visited current) t)
        finally (return nodes)))

(defun part1 (input)
  (node-distance (search* (parse-input input) *target*)))

(defun patch (state extra pos)
  (concatenate 'string (subseq state 0 pos) extra (subseq state pos)))

(defun part2 (input)
  (node-distance (search* (patch (parse-input input) "DCBADBAC" 15)
                          (patch *target* "ABCDABCD" 15))))
