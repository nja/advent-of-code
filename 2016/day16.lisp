;;;; day16.lisp

(in-package #:aoc2016.day16)

(defun parse (input size)
  (loop with array = (make-array size :fill-pointer 0 :element-type 'bit)
        for c across (string-trim '(#\Newline #\Return) input)
        do (vector-push (digit-char-p c) array)
        finally (return array)))

(defun expand (bits)
  (let ((n (length bits)))
    (vector-push 0 bits)
    (loop for i from (1- n) downto 0
          while (< (fill-pointer bits) (array-total-size bits))
          do (vector-push (if (eq 0 (aref bits i)) 1 0) bits))
    bits))

(defun expand* (bits)
  (loop while (< (length bits) (array-total-size bits))
        do (expand bits)
        finally (return bits)))

(defun checksum (bits)
  (loop with checksum = (make-array (/ (length bits) 2)
                                    :fill-pointer 0 :element-type 'bit)
        for i from 0 by 2 below (length bits)
        for a = (aref bits i)
        for b = (aref bits (1+ i))
        do (vector-push (if (eql a b) 1 0) checksum)
        finally (return checksum)))

(defun checksum* (bits)
  (loop for checksum = bits then (checksum checksum)
        while (evenp (length checksum))
        finally (return checksum)))

(defun as-string (bits)
  (map 'string #'digit-char bits))

(defun part1 (input)
  (as-string (checksum* (expand* (parse input 272)))))

(defun part2 (input)
  (as-string (checksum* (expand* (parse input 35651584)))))
