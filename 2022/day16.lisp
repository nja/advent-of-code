;;;; day16.lisp

(in-package :aoc2022.day16)

(defstruct (valve) id rate tunnels)

(defun parse (input)
  (let ((*package* (symbol-package 'parse))
        (valves (make-hash-table)))
    (ppcre:do-register-groups (id rate tunnels)
        ("Valve (\\w+) has flow rate=(\\d+); tunnels? leads? to valves? (.*)" input)
      (let ((id (intern id)))
        (setf (gethash id valves)
              (make-valve :id id :rate (parse-integer rate)
                          :tunnels (mapcar #'intern (str:split ", " tunnels))))))
    valves))

(defun map-distances (valves)
  (let ((distances (make-hash-table :test 'equal))
        (rates (make-hash-table)))
    (flet ((find-distance (src dst)
             (flet ((neighbours (id) (valve-tunnels (gethash id valves))))
               (d:distance (d:search* src #'neighbours :goal dst)))))
      (dolist (valve (a:hash-table-values valves))
        (when (> (valve-rate valve) 0)
          (setf (gethash (valve-id valve) rates) (valve-rate valve))))
      (a:map-combinations (lambda (pair)
                            (a:map-product (lambda (a b)
                                             (setf (gethash (cons a b) distances)
                                                   (find-distance a b)))
                                           pair pair))
                          (cons 'aa (a:hash-table-keys rates)) :length 2))
    (values distances rates (a:hash-table-keys rates))))

(defun travel (distances rates nodes &key (minutes 30))
  (let ((max-released 0)
        best-path)
    (labels ((rate (id) (gethash id rates))
             (distance (a b) (gethash (cons a b) distances 1))
             (rec (minutes released src unopened path)
               (when (and (plusp minutes) (rate src))
                 (decf minutes)
                 (incf released (* minutes (rate src)))
                 (when (< max-released released)
                   (setf max-released released
                         best-path path)))
               (cond
                 ((null unopened) released)
                 ((< (+ released (* minutes (reduce #'+ unopened
                                                    :key (a:rcurry #'gethash rates))))
                     max-released) released)
                 (t 
                  (reduce
                   #'max
                   (mapcar (lambda (dst)
                             (let ((distance (distance src dst)))
                               (if (< minutes (1+ distance))
                                   released
                                   (rec (- minutes distance)
                                        released
                                        dst
                                        (remove dst unopened)
                                        (cons dst path)))))
                           unopened))))))
      (values (rec minutes 0 'aa nodes nil)
              best-path))))

(defun part1 (input)
  (multiple-value-call #'travel (map-distances (parse input))))

(defun complements (nodes)
  (flet ((key (nodes)
           (sort nodes #'string< :key #'symbol-name)))
    (let ((lefts (make-hash-table :test 'equal)))
      (loop for i from 1 to (length nodes)
            do (a:map-combinations
                (lambda (left)
                  (let ((right (key (set-difference nodes left))))
                    (unless (gethash right lefts)
                      (setf (gethash (key left) lefts) t))))
                nodes :length i))
      (a:hash-table-keys lefts))))

(defun elephant (distances rates nodes)
  (flet ((score (nodes)
           (multiple-value-list
            (travel distances rates nodes :minutes 26))))
    (loop for nodes in (complements nodes)
          for (score path) = (score nodes)
          for bonus = (first (score (set-difference nodes path)))
          maximize (+ score bonus))))

(defun part2 (input)
  (multiple-value-call #'elephant (map-distances (parse input))))
