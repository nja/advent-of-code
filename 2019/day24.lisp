;;;; day24.lisp

(in-package :aoc2019.day24)

(defun parse (input)
  (parse-integer (remove #\Newline (aoc:tr "#." "10" (reverse input))) :radix 2))

(defun neighbours (state bit)
  (multiple-value-bind (row col) (truncate bit 5)
    (loop for d in '(-1 1 -5 5)
          for i = (+ bit d)
          sum (if (logtest state (ash 1 i))
                  (multiple-value-bind (r c) (truncate i 5)
                    (if (or (eql row r) (eql col c))
                        1
                        0))
                  0))))

(defun next (state)
  (loop with next = 0
        for i below 25
        for n = (neighbours state i)
        when (if (logtest state (ash 1 i))
                 (eql n 1)
                 (or (eql n 1) (eql n 2)))
          do (incf next (ash 1 i))
        finally (return next)))

(defun part1 (input)
  (loop with seen = (make-hash-table)
        for state = (parse input) then (next state)
        for i from 0
        when (gethash state seen)
          return (values state i)
        do (setf (gethash state seen) t)))

(defun get-state (states gen lvl)
  (let ((key (cons gen lvl)))
    (or (gethash key states)
        (setf (gethash key states)
              (if (> (abs lvl) (ceiling gen 2))
                  0
                  (next2 states (1- gen) lvl))))))

(defun next2 (states gen lvl)
  (let ((state (get-state states gen lvl)))
    (loop with next = 0
          for i below 25
          for n = (+ (neighbours state i)
                     (next-level-neighbours states gen lvl i))
          when (and (/= i 12)
                    (if (logtest state (ash 1 i))
                        (eql n 1)
                        (or (eql n 1) (eql n 2))))
            do (incf next (ash 1 i))
          finally (return next))))

(defun next-level-neighbours (states gen lvl bit)
  (let ((lvl (next-level lvl bit)))
    (if (null lvl)
        0
        (loop with state = (get-state states gen lvl)
              for b in (next-level-bits bit)
              count (logtest state (ash 1 b))))))

(defun next-level-bits (bit)
  (case bit
    (0 '(7 11))
    ((1 2 3) '(7))
    (4 '(7 13))
    ((9 14 19) '(13))
    (24 '(13 17))
    ((23 22 21) '(17))
    (20 '(11 17))
    ((15 10 5) '(11))
    (7 '(0 1 2 3 4))
    (13 '(4 9 14 19 24))
    (17 '(20 21 22 23 24))
    (11 '(0 5 10 15 20))))

(defun next-level (lvl bit)
  (case bit
    ((0 1 2 3 4 9 14 19 24 23 22 21 20 15 10 5) (1+ lvl))
    ((7 13 17 11) (1- lvl))))

(defun states (state)
  (let ((states (make-hash-table :test 'equal)))
    (setf (gethash (cons 0 0) states) state)
    states))

(defun bugcount (state gen)
  (loop with states = (states state)
        with depth = (ceiling gen 2)
        for lvl from (- depth) to depth
        sum (logcount (get-state states gen lvl))))

(defun part2 (input)
  (bugcount (parse input) 200))

;;      |     |              |     |
;;   0  |  1  |       2      |  3  |  4
;;      |     |              |     |
;; -----+-----+--------------+-----+-----
;;      |     |              |     |
;;   5  |  6  |       7      |  8  |  9
;;      |     |              |     |
;; -----+-----+--------------+-----+-----
;;      |     | 0| 1| 2| 3| 4|     |
;;      |     |--+--+--+--+--|     |
;;      |     | 5|  | 7|  | 9|     |
;;      |     |--+--+--+--+--|     |
;;  10  | 11  |10|11|??|13|14|  13 |  14
;;      |     |--+--+--+--+--|     |
;;      |     |15|  |17|  |19|     |
;;      |     |--+--+--+--+--|     |
;;      |     |20|21|22|23|24|     |
;; -----+-----+--------------+-----+-----
;;      |     |              |     |
;;  15  | 16  |      17      |  18 |  19
;;      |     |              |     |
;; -----+-----+--------------+-----+-----
;;      |     |              |     |
;;  20  | 21  |      22      |  23 |  24
;;      |     |              |     |
