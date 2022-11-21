;;;; day15.lisp

(in-package #:aoc2016.day15)

(defstruct (disc) pos mod)

(defun parse (input)
  (mapcar #'parse-line (aoc:lines input)))

(defun parse-line (line)
  (ppcre:register-groups-bind ((#'parse-integer offset mod pos))
      ("Disc #(\\d+) has (\\d+) positions; at time=0, it is at position (\\d+)." line)
    (make-disc :pos (mod (+ pos offset) mod) :mod mod)))

(defun tick (disc n)
  (make-disc :pos (mod (+ (disc-pos disc) n) (disc-mod disc))
             :mod (disc-mod disc)))

(defun ticks-left (disc step-size)
  (loop for pos from (disc-pos disc) by step-size
        until (zerop (mod pos (disc-mod disc)))
        finally (return (- pos (disc-pos disc)))))

(defun step-size (discs)
  (reduce #'lcm discs :key #'disc-mod))

(defun unsynced (discs)
  (remove 0 discs :key #'disc-pos))

(defun synced (discs)
  (remove 0 discs :key #'disc-pos :test-not #'eql))

(defun do-step (discs)
  (let ((ticks (ticks-left (first (unsynced discs)) (step-size (synced discs)))))
    (values (mapcar (a:rcurry #'tick ticks) discs)
            ticks)))

(defun sync (discs)
  (loop until (null (unsynced discs))
        for (tmp ticks) = (multiple-value-list (do-step discs))
        sum ticks
        do (setf discs tmp)))

(defun part1 (input)
  (sync (parse input)))

(defparameter *disc7* "Disc #7 has 11 positions; at time=0, it is at position 0.")

(defun part2 (input)
  (sync (append (parse input) (parse *disc7*))))
