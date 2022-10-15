;;;; day15.lisp

(in-package #:aoc2018.day15)

(defstruct unit type (hp 200) strength signalp)

(defun to-array (lines &optional (e-strength 3) (e-signalp))
  (loop with rows = (length lines)
        with cols = (reduce #'max (mapcar #'length lines))
        with array = (make-array (list rows cols) :initial-element #\Space)
        for y from 0
        for line in lines
        do (loop for x from 0
                 for ch across line
                 do (setf (aref array y x)
                          (case ch
                            (#\G (make-unit :type 'g :strength 3))
                            (#\E (make-unit :type 'e :strength e-strength
                                            :signalp e-signalp))
                            (t ch))))
        finally (return array)))

(defun print-array (array)
  (flet ((short (x)
           (if (unit-p x)
               (unit-type x)
               x))
         (long (x)
           (format nil "~a(~3,'0d)" (unit-type x) (unit-hp x))))
    (aoc:print-indexed-lines
     (loop for row below (array-dimension array 0)
           for units = nil
           collect (with-output-to-string (s)
                     (loop for col below (array-dimension array 1)
                           for x = (aref array row col)
                           when (unit-p x)
                             do (push (long x) units)
                           do (format s "~a" (short x)))
                     (when units
                       (format s "   ~{~a~^, ~}" (nreverse units)))
                     (terpri s)))
     (array-dimension array 1))))

(defstruct (pos (:conc-name) (:constructor pos (row col))) row col)

(defun compare (x y)
  (or (and x (null y))
      (and x y (< x y))))

(defun neighbours (pos array)
  (let (result)
    (flet ((try (row col)
             (when (aref-when array (a:curry #'equal #\.) row col)
               (push (pos row col) result))))
      (try (1+ (row pos)) (col pos))
      (try (row pos) (1+ (col pos)))
      (try (row pos) (1- (col pos)))
      (try (1- (row pos)) (col pos))
      result)))

(defun near-target? (array target pos &key (best nil))
  (let (near)
    (flet ((target? (row col)
             (a:when-let (x (aref-when array (a:curry #'target? target) row col))
               (when (or (null near)
                         (< (unit-hp x)
                            (unit-hp (aref array (row near) (col near)))))
                 (setf near (pos row col))
                 (and (not best) near)))))
      (or (target? (1- (row pos)) (col pos))
          (target? (row pos) (1- (col pos)))
          (target? (row pos) (1+ (col pos)))
          (target? (1+ (row pos)) (col pos))
          near))))

(defun aref-when (array p row col)
  (let ((x (and (array-in-bounds-p array row col) (aref array row col))))
    (and (funcall p x) x)))

(defun target? (target x)
  (and (unit-p x) (equal target (unit-type x))))

(defun compare-pos (x y)
  (or (< (row x) (row y))
      (and (eql (row x) (row y))
           (< (col x) (col y)))))

(defun find-target-pos (array source target)
  (a:when-let (found (d:search*
                      source
                      (a:rcurry #'neighbours array)
                      :donep (a:curry #'near-target? array target)
                      :comparef #'compare-pos))
    (values (d:item found) (d:distance found))))

(defun find-path (array from to cutoff)
  (a:when-let (found (d:search*
                      from
                      (a:rcurry #'neighbours array)
                      :donep (a:curry #'equalp to)
                      :comparef #'compare-pos
                      :max-distance cutoff))
    (loop for n = found then (d:previous n)
          while (d:previous n)
          finally (return (d:item n)))))

(defun do-round (array)
  (let ((positions (unit-positions array))
        (complete t))
    (dolist (pos positions complete)
      (a:when-let (unit (aref-when array #'unit-p (row pos) (col pos)))
        (unless (some (lambda (p) (unit-target? unit (aref array (row p) (col p))))
                      positions)
          (setf complete nil))
        (unless (near-target? array (unit-target unit) pos)
          (a:when-let (move (first-move array pos (unit-target unit)))
            (rotatef (aref array (row move) (col move)) (aref array (row pos) (col pos)))
            (setf pos move)))
        (a:when-let* ((target-pos (near-target? array (unit-target unit) pos :best t))
                      (target (aref array (row target-pos) (col target-pos))))
          (when (< (decf (unit-hp target) (unit-strength unit)) 1)
            (if (unit-signalp target)
                (error 'death)
                (setf (aref array (row target-pos) (col target-pos)) #\.))))))))

(defun unit-positions (array)
  (loop for row below (array-dimension array 0)
        nconc (loop for col below (array-dimension array 1)
                    for x = (aref array row col)
                    when (unit-p x) collect (pos row col))))

(defun unit-target (unit)
  (case (unit-type unit) (e 'g) (g 'e)))

(defun unit-target? (unit x)
  (and (unit-p x) (equal (unit-target unit) (unit-type x))))

(defun first-move (array pos target)
  (multiple-value-bind (target-pos distance) (find-target-pos array pos target)
    (when target-pos
      (some (lambda (n) (find-path array n target-pos distance))
            (neighbours pos array)))))

(defun done? (array)
  (loop with types
        for i below (array-total-size array)
        for x = (row-major-aref array i)
        count (and (unit-p x) (eql (unit-type x) 'g)) into g
        count (and (unit-p x) (eql (unit-type x) 'e)) into e
        never (and (plusp g) (plusp e))))

(defun run (array &optional printp)
  (* (loop for i from 1
           for full? = (do-round array)
           when printp
             do (format t "~%After ~3d rounds:~%~%" i)
                (print-array array)
           until (done? array)
           finally (return (if full? i (1- i))))
     (hitpoints array)))

(defun hitpoints (array)
  (loop for i below (array-total-size array)
        for x = (row-major-aref array i)
        when (unit-p x) sum (unit-hp x)))

(defun part1 (input)
  (run (to-array (aoc:lines input))))

(define-condition death () ())

(defun no-goblins? (array)
  (loop for i below (array-total-size array)
        never (eql 'g (row-major-aref array i))))

(defun part2 (input)
  (loop with lines = (aoc:lines input)
        for strength from 4
        for array = (to-array lines strength t)
        for score = (handler-case (run array) (death ()))
        when (and score (no-goblins? array))
          return score))
