;;;; day02.lisp

(in-package :aoc2025.day02)

(defun ranges (input)
  (mapcar #'aoc:read-as-list (mapcar (a:curry #'aoc:tr "-" " ") (str:split #\, input))))

(defun invalidp (n)
  (let* ((s (format nil "~d" n))
         (l (length s))
         (h (truncate l 2)))
    (and (evenp l)
         (loop for i from 0
               for j from h
               while (< j l)
               always (char= (aref s i) (aref s j))))))

(defun sum-invalids (first last)
  (loop for x from first to last
        when (invalidp x)
          sum x))

(defun part1 (input)
  (loop for (first last) in (ranges input)
        sum (sum-invalids first last)))

(defun split (s n)
  (labels ((rec (s l r)
             (if (zerop (length s))
                 r
                 (rec (subseq s l) l (cons (subseq s 0 l) r)))))
    (when (integerp (/ (length s) n))
      (rec s (/ (length s) n) nil))))

(defun samesp (s n)
  (let ((parts (split s n)))
    (and parts (< 1 (length parts)) (loop for x in (rest parts)
                                          always (string= x (first parts))))))

(defun invalidp* (n)
  (let ((s (format nil "~d" n)))
    (loop for i from 1 upto (length s)
            thereis (samesp s i))))

(defun sum-invalids* (first last)
  (loop for x from first to last
        when (invalidp* x)
          sum x))

(defun part2 (input)
  (loop for (first last) in (ranges input)
        sum (sum-invalids* first last)))


;; 2025 02 1: '24043483400'
;; That's the right answer!
;; (Cached until 2025-12-02 05:12:14)
;; 24043483400

(defparameter *test1*
  "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124")