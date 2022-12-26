;;;; day17.lisp

(in-package :aoc2022.day17)

(defun parse (input)
  (let* ((jets (remove nil (map 'list (a:curry #'getf '(#\< < #\> >)) input)))
         (n (length jets)))
    (values (cdr (rplacd (last jets) jets)) n)))

(defparameter *blocks* (remove #\Return
"####

.#.
###
.#.

..#
..#
###

#
#
#
#

##
##"))

(defun pos (x y) (list x y))
(defun add (a b) (mapcar #'+ a b))
(defun x (pos) (car pos))
(defun y (pos) (cadr pos))

(defun blocks (&optional (string *blocks*))
  (let ((blocks
          (mapcar (lambda (section)
                    (let (result
                          (col 0))
                      (flet ((next-row ()
                               (setf result (mapcar (a:curry #'add '(0 -1)) result)
                                     col 0)))
                        (loop for c across section
                              do (case c
                                   (#\. (incf col))
                                   (#\Newline (next-row))
                                   (#\#
                                    (push (pos col 0) result)
                                    (incf col)))
                              finally (return (sort (nreverse result) #'> :key #'cadr))))))
                  (aoc:sections string))))
    (apply #'a:circular-list blocks)))

(defparameter *chamber* nil)

(defun chamber ()
  (make-array (list 10000 7) :initial-element #\.))

(defun faller (pos block)
  (cons pos block))

(defun fpos (faller)
  (car faller))

(defun fblocks (faller)
  (cdr faller))

(defun faller-positions (faller)
  (mapcar (a:curry #'add (fpos faller)) (fblocks faller)))

(defun min-y (faller)
  (+ (y (fpos faller))
     (loop for b in (fblocks faller)
           minimize (y b))))

(defun intersects? (faller positions)
  (loop with (fx fy) = (fpos faller)
        and blocks = (fblocks faller)
        for (x y) in positions
          thereis (loop for (bx by) in blocks
                          thereis (and (eql x (+ fx bx))
                                       (eql y (+ fy by))))))

(defun fits? (chamber faller)
  (loop with (fx fy) = (fpos faller)
        for (bx by) in (fblocks faller)
        for col = (+ fx bx) and row = (+ fy by)
        never (or (not (array-in-bounds-p chamber row col))
                  (not (eql #\. (aref chamber row col))))))

(defun move (chamber faller d)
  (let ((moved (faller (add (fpos faller) d) (fblocks faller))))
    (when (fits? chamber moved)
      moved)))

(defun fix (chamber faller)
  (loop with (fx fy) = (fpos faller)
        for (bx by)  in (fblocks faller)
        for col = (+ fx bx) and row = (+ fy by)
        do (setf (aref chamber (+ fy by) (+ fx bx)) #\#)))

(defun start-pos (min-y)
  (pos 2 (- min-y 4)))

(defun find-min-y (chamber &optional min-y)
  (or (loop for i from (max 0 (- (or min-y 0) 4)) below (array-total-size chamber)
            unless (eql #\. (row-major-aref chamber i))
              do (return (truncate i (array-dimension chamber 1))))
      (array-dimension chamber 0)))

(defun turn (chamber faller blocks jets &optional min-y)
  (let* ((min-y (find-min-y chamber min-y))
         (faller (or faller (faller (start-pos min-y) (pop blocks))))
         (d (getf '(< (-1 0) > (1 0)) (pop jets))))
    (flet ((jet-push (f) (or (move chamber f d) f))
           (fall (f) (or (move chamber f '(0 1)) f)))
      (let* ((fallen (fall (jet-push faller)))
             (min-y (min min-y (min-y fallen)))
             (resting? (eq (y (fpos faller)) (y (fpos fallen)))))
        (when resting?
          (fix chamber fallen))
        (values chamber (unless resting? fallen) blocks jets min-y)))))

(defun turns (n jets)
  (loop with blocks = (blocks)
        with heights and fixeds and skip-height = 0
        for (c f b j y) = (multiple-value-list (turn (chamber) nil blocks jets))
          then (multiple-value-list (turn c f b j y))
        for height = (height c y)
        count (not f) into fixed
        while (< fixed n)
        when (eq j jets)
          do (push height heights)
             (push fixed fixeds)
             (when (cdr heights)
               (let ((gain (- (first heights) (second heights)))
                     (f (- (first fixeds) (second fixeds))))
                 (loop while (< (+ fixed f) n)
                       do (incf skip-height gain)
                          (incf fixed f))))
        finally (return (+ height skip-height))))

(defun height (chamber &optional min-y)
  (- (array-dimension chamber 0) (find-min-y chamber min-y)))

(defun part1 (input)
  (turns 2022 (parse input)))

(defun part2 (input)
  (turns 1000000000000 (parse input)))
