;;;; day14.lisp

(in-package #:aoc2016.day14)

(defparameter *digest* (i:make-digest :md5))
(defparameter *stretch* 0)

(defun digest (salt n)
  (let ((buf (make-array 16 :element-type '(unsigned-byte 8)))
        (digest (i:copy-digest *digest*))
        (num (format nil "~d" n))
        (stretch-string (make-array 32 :element-type '(unsigned-byte 8))))
    (copy-char-codes salt buf)
    (copy-char-codes num buf (length salt))
    (i:update-digest digest buf :end (+ (length salt) (length num)))
    (dotimes (i *stretch*)
      (i:produce-digest digest :digest buf)
      (copy-hex-codes buf stretch-string)
      (i:copy-digest *digest* digest)
      (i:update-digest digest stretch-string))
    (i:produce-digest digest :digest buf)
    buf))

(defun copy-char-codes (string buf &optional (start 0))
  (loop for c across string
        for i from start
        do (setf (aref buf i) (char-code c))
        finally (return i)))

(defun copy-hex-codes (bytes buf)
  (loop with chars = "0123456789abcdef"
        for b across bytes
        for hi = (ldb (byte 4 4) b)
        for lo = (ldb (byte 4 0) b)
        for i from 0 by 2
        do (setf (aref buf i) (char-code (aref chars hi))
                 (aref buf (1+ i)) (char-code (aref chars lo)))))

(defun triple? (bytes)
  (let ((count 0) val)
    (flet ((tally (x)
             (= 3 (if (eql val x)
                      (incf count)
                      (setf val x
                            count 1)))))
      (loop for b across bytes
            when (or (tally (ldb (byte 4 4) b))
                     (tally (ldb (byte 4 0) b)))
              do (return val)))))

(defun quintets (bytes)
  (let ((count 0) val result)
    (flet ((tally (x)
             (when (= 5 (if (eql val x)
                            (incf count)
                            (setf val x
                                  count 1)))
               (pushnew val result))))
      (loop for b across bytes
            do (tally (ldb (byte 4 4) b))
               (tally (ldb (byte 4 0) b)))
      result)))

(defun keys (salt n)
  (let ((triples (make-array 16 :initial-element nil))
        key-indices)
    (flet ((past-triples (val min)
             (loop for x in (aref triples val)
                   while (<= min x)
                   do (push x key-indices)
                      (decf n))))
      (loop with max
            for i from 0
            for digest = (digest salt i)
            for triple = (triple? digest)
            do (dolist (qval (quintets digest))
                 (past-triples qval (- i 1000)))
            when triple
              do (push i (aref triples triple))
            when (and (null max) (not (plusp n)))
              do (setf max (+ i 1000))
            while (or (null max) (< i max))
            finally (return (remove-duplicates (sort key-indices #'<)))))))

(defun part1 (input)
  (elt (keys (string-right-trim '(#\Return #\Newline) input) 64) 63))

(defun part2 (input)
  (let ((*stretch* 2016))
    (elt (keys (string-right-trim '(#\Return #\Newline) input) 64) 63)))
