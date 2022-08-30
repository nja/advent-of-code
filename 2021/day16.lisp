;;;; day16.lisp

(in-package #:aoc2021.day16)

(defun to-bit-stream (string &key (radix 16))
  (loop with n = (parse-integer string :radix radix)
        with size = (truncate (* (length string) (log radix 2)))
        with bits = (make-array size :element-type 'bit :fill-pointer 0)
        do (vector-push (logand 1 n) bits)
           (setf n (ash n -1))
        repeat size
        finally (return bits)))

(defun read-bits (length bit-stream)
  (loop repeat length
        for b = (vector-pop bit-stream)
        for n = b then (logior (ash n 1) b)
        finally (return n)))

(defun parse-packet (stream)
  (let* ((version (read-bits 3 stream))
         (type (read-bits 3 stream)))
    (cons (list (case type
                  (0 '+)
                  (1 '*)
                  (2 'min)
                  (3 'max)
                  (4 'quote)
                  (5 '>)
                  (6 '<)
                  (7 '=))
                version)
          (case type
            (4 (parse-literal stream))
            (t (parse-operator stream))))))

(defun parse-literal (stream)
  (list (loop for done = (= 0 (read-bits 1 stream))
              for val = (read-bits 4 stream)
              for result = val then (logior (ash result 4) val)
              until done
              finally (return result))))

(defun parse-operator (stream)
  (let ((length-type (read-bits 1 stream)))
    (case length-type
      (0 (let ((length (read-bits 15 stream)))
           (parse-bits length stream)))
      (1 (let ((length (read-bits 11 stream)))
           (parse-packets length stream))))))

(defun parse-bits (length stream)
  (loop with end = (- (length stream) length)
        while (< end (length stream))
        collect (parse-packet stream)))

(defun parse-packets (length stream)
  (loop repeat length
        collect (parse-packet stream)))

(defun sum-versions (packet)
  (+ (cadar packet)
     (if (eq 'quote (caar packet))
         0
         (reduce #'+ (mapcar #'sum-versions (cdr packet))))))

(defun transform (packet)
  (if (atom packet)
      packet
      (destructuring-bind ((op version) . args) packet
        (declare (ignorable version))
        (let ((args (mapcar #'transform args)))
          (case op
            ((> < =) `(if (,op ,@args) 1 0))
            (quote (first args))
            (t (cons op args) ))))))

(defun part1 (input)
  (sum-versions (parse-packet (to-bit-stream (str:trim input)))))

(defun part2 (input)
  (eval (transform (parse-packet (to-bit-stream (str:trim input))))))
