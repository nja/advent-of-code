;;;; day22.lisp

(in-package :aoc2024.day22)

(defun parse (input)
  (mapcar #'parse-integer (aoc:lines input)))

(defun secret (secret)
  (declare (fixnum secret))
  (flet ((mix (x) (logxor x secret))
         (prune (x) (setf secret (logand x #xffffff))))
    (declare (ftype (function (fixnum) fixnum)  mix prune))
    (prune (mix (ash secret 6)))
    (prune (mix (ash secret -5)))
    (prune (mix (ash secret 11)))))

(defun secrets (n secret)
  (dotimes (i n secret)
    (setf secret (secret secret))))

(defun part1 (input)
  (reduce #'+ (mapcar (a:curry #'secrets 2000) (parse input))))

(defun max-yield (secrets)
  (let ((yields (make-hash-table))
        (seen (make-hash-table))
        (max 0))
    (declare (fixnum max))
    (loop for secret fixnum in secrets
          do (loop with key fixnum
                   for p = (mod secret 10)
                   for i fixnum from 1 to 2000
                   do (setf secret (secret secret))
                      (let* ((price (mod secret 10))
                             (c (- price p)))
                        (setf key (logand #xfffff (logior (ash key 5) (+ c 9))))
                        (when (<= 4 i)
                          (unless (gethash key seen)
                            (a:maxf max (incf (the fixnum (gethash key yields 0)) price))
                            (setf (gethash key seen) t)))))
             (clrhash seen))
    max))

(defun part2 (input)
  (max-yield (parse input)))
