;;;; day14.lisp

(in-package #:aoc2018.day14)

(setf *print-circle* t)

(defun digits (string)
  (remove nil (map 'list #'digit-char-p string)))

(defstruct lab first second head tail length trailer)

(defun setup (digits)
  (let ((lab (make-lab :first digits
                       :second (cdr digits)
                       :head digits
                       :tail (last digits)
                       :length (length digits)
                       :trailer digits)))
    (setf (cdr (lab-tail lab)) digits)
    lab))

(defun score (lab)
  (list (car (lab-first lab))
        (car (lab-second lab))))

(defun combine (x y)
  (delete 0 (multiple-value-list
             (truncate (+ x y) 10))
          :end 1))

(defun add (lab scores)
  (incf (lab-length lab) (length scores))
  (setf (cdr (lab-tail lab)) scores)
  (setf (lab-tail lab) (last scores))
  (setf (cdr (lab-tail lab)) (lab-head lab)))

(defun advance (lab)
  (when (and *head-start* (> (lab-length lab) *head-start*))
    (setf (lab-trailer lab) (cdr (lab-trailer lab))))
  (setf (lab-first lab) (nthcdr (1+ (car (lab-first lab)))
                                (lab-first lab)))
  (setf (lab-second lab) (nthcdr (1+ (car (lab-second lab)))
                                 (lab-second lab))))

(defun work (digits target &optional print)
  (let ((lab (setup digits))
        (limit (+ 10 target)))
    (loop while (< (lab-length lab) limit)
          do (add lab (apply #'combine (score lab)))
             (advance lab)
             (when print (print-lab lab)))
    (subseq (lab-head lab) target limit)))

(defun part1 (input)
  (parse-integer
   (map 'string #'digit-char
        (work (list 3 7) (parse-integer input)))))

(defun print-lab (lab)
  (loop repeat (lab-length lab)
        for d on (lab-head lab)
        do (format t "~a~a~a"
                   (cond ((eq d (lab-first lab)) #\()
                         ((eq d (lab-second lab)) #\[)
                         (t #\Space))
                   (car d)
                   (cond ((eq d (lab-first lab)) #\))
                         ((eq d (lab-second lab)) #\])
                         (t #\Space))))
  (terpri))

(defvar *head-start* nil)

(defun locate (digits target)
  (let ((lab (setup digits))
        (*head-start* (length target)))
    (flet ((match ()
             (loop for i from 0
                   for a in target
                   for b in (lab-trailer lab)
                   always (= a b)))
           (trailer-position ()
             (loop for i from 0
                   for x on (lab-head lab)
                   when (eq x (lab-trailer lab))
                     do (return i))))
      (loop until (match)
            do (add lab (apply #'combine (score lab)))
               (advance lab)
            finally (return (trailer-position))))))

(defun part2 (input)
  (locate (list 3 7) (digits input)))

;;; take trailer out of lab
;;; when two new recipes, do match, if no match, incf trailer again
;;; and match again.
