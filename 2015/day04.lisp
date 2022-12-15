;;;; day04.lisp

(in-package #:aoc2015.day04)

(defun octets (string)
  (map '(vector (unsigned-byte 8)) #'char-code string))

(defun hasher (input)
  (let ((key (octets (string-right-trim '(#\Newline #\Return) input))))
    (lambda (i)
      (ironclad:digest-sequence
       :md5 (concatenate '(vector (unsigned-byte 8))
                         key (octets (format nil "~d" i)))))))

(defun match? (octets)
  (and (zerop (aref octets 0))
       (zerop (aref octets 1))
       (< (aref octets 2) #x10)))

(defun find-match (hasher test)
  (loop for i from 0
        when (funcall test (funcall hasher i))
          do (return i)))

(defun part1 (input)
  (find-match (hasher input) #'match?))

(defun match2? (octets)
  (and (zerop (aref octets 0))
       (zerop (aref octets 1))
       (zerop (aref octets 2))))

(defun part2 (input)
  (find-match (hasher input) #'match2?))
