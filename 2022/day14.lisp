;;;; day14.lisp

(in-package :aoc2022.day14)

(defun parse (input)
  (mapcar (lambda (line)
            (pairs (read-from-string (format nil "(~a)" (aoc:tr "->," "   " line)))))
          (aoc:lines input)))

(deftype unsigned-coord () '(unsigned-byte 24))
(deftype coord () '(signed-byte 24))
(deftype pos () 'fixnum)

(declaim (inline pos upos add ux uy x y dir))

(defun pos (x y)
  (declare (ftype (function (coord coord) pos) pos))
  (dpb y (byte 24 0) (dpb x (byte 24 24) 0)))

(defun upos (x y)
  (declare (ftype (function (unsigned-coord unsigned-coord) pos) upos))
  (dpb y (byte 24 0) (dpb x (byte 24 24) 0)))

(defun ux (pos)
  (declare (ftype (function (pos) unsigned-coord) ux))
  (ldb (byte 24 24) pos))

(defun x (pos)
  (declare (ftype (function (pos) coord) x))
  (if (logbitp 47 pos)
      (mod (ldb (byte 24 24) pos) #x-1000000)
      (ldb (byte 24 24) pos)))

(defun uy (pos)
  (declare (ftype (function (pos) unsigned-coord) uy))
  (ldb (byte 24 0) pos))

(defun y (pos)
  (declare (ftype (function (pos) coord) y))
  (if (logbitp 23 pos)
      (mod (ldb (byte 24 0) pos) #x-1000000)
      (ldb (byte 24 0) pos)))

(defun add (a b)
  (declare (ftype (function (pos pos) pos) add))
  (upos (logand #xffffff (+ (ux a) (ux b)))
        (logand #xffffff (+ (uy a) (uy b)))))

(defun dir (src dst)
  (declare (ftype (function (pos pos) pos) dir))
  (pos (a:clamp (- (ux dst) (ux src)) -1 1)
       (a:clamp (- (uy dst) (uy src)) -1 1)))

(defun pairs (list)
  (loop for (a b) on list by #'cddr
        while b
        collect (pos a b)))

(declaim (type hash-table *world*))
(defvar *world* (make-hash-table))

(declaim (type coord *void* *floor*))
(defvar *void* 0)
(defvar *floor* 0)

(defun world (shapes)
  (let ((*world* (make-hash-table :size #x7fff)))
    (flet ((line (src dst)
             (loop with d = (dir src dst)
                   for pos fixnum = src then (add pos d)
                   do (mark #\# pos)
                      (setf *void* (max *void* (+ 2 (y pos))))
                   until (equal pos dst))))
      (dolist (shape shapes (values *world* *void*))
        (mapc #'line shape (cdr shape))))))

(defun mark (c pos)
  (setf (gethash pos *world*) c))

(defun fall (trace)
  (loop for pos = (first trace)
        for under = (under pos)
        when (solid? under)
          do (return trace)
        until (< *void* (uy pos))
        do (push under trace)))

(defun solid? (pos)
  (declare (ftype (function (pos) t) solid?))
  (if (<= *floor* (uy pos))
      t
      (gethash pos *world*)))

(defun under (pos)
  (add pos (pos 0 1)))

(defun sand (trace)
  (let ((src (car trace)))
    (when (typep src 'pos)
      (cond ((solid? (under src))
             (let ((left (add src (pos -1 1)))
                   (right (add src (pos 1 1))))
               (declare (type pos left right))
               (cond ((not (solid? left)) (sand (cons left trace)))
                     ((not (solid? right)) (sand (cons right trace)))
                     (t (mark #\o src) (cdr trace)))))
            (t (sand (fall trace)))))))

(defun sand-count (world void &optional floor?)
  (let ((*world* world)
        (*void* void)
        (*floor* (if floor? void #x7fffff)))
    (loop with source = (list (pos 500 0))
          for trace = (sand source) then (sand trace)
          count 1
          while trace)))

(defun part1 (input)
  (1- (multiple-value-call #'sand-count (world (parse input)))))

(defun part2 (input)
  (multiple-value-call #'sand-count (world (parse input)) t))
