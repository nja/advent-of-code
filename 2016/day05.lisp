;;;; day05.lisp

(in-package #:aoc2016.day05)

(defun tick (vector p)
  (let ((i (- (length vector) p 1)))
    (cond ((<= (char-code #\0) (aref vector i) (char-code #\8))
           (incf (aref vector i))
           vector)
          ((= (char-code #\9) (aref vector i))
           (setf (aref vector i) (char-code #\0))
           (tick vector (1+ p)))
          (t (tick (grow vector p) p)))))

(defun grow (vector p)
  (loop with grown = (make-array (1+ (length vector)) :element-type '(unsigned-byte 8))
        with x = (- (length vector) p)
        for i below (length grown)
        do (setf (aref grown i) (cond ((< i x) (aref vector i))
                                      ((= i x) (char-code #\0))
                                      (t (aref vector (1- i)))))
        finally (return grown)))

(defun hi (octet)
  (declare (type (integer 0 255) octet))
  (the (integer 0 15) (ash (logand #xf0 octet) -4)))

(defun lo (octet)
  (declare (type (integer 0 255) octet))
  (the (integer 0 15) (logand #x0f octet)))

(defun octets (string)
  (map '(vector (unsigned-byte 8)) #'char-code string))

(defun interesting (hash)
  (when (= 0
           (aref hash 0)
           (aref hash 1)
           (hi (aref hash 2)))
    (values (lo (aref hash 2))
            (hi (aref hash 3)))))

(defun find-matches (n input)
  (loop with count = 0
        for buf = (grow input 0) then (tick buf 0)
        for val = (interesting (ironclad:digest-sequence :md5 buf))
        when val
          collect val
          and do (incf count)
        until (= count n)))

(defun hex-digit (i)
  (digit-char i 16))

(defun password (values)
  (map 'string #'hex-digit values))

(defun part1 (input)
  (password (find-matches 8 (octets (remove #\Newline input)))))

(defun set-matches (n input)
  (loop with password = (make-sequence 'string n :initial-element #\_)
        with digest = (ironclad:make-digest :md5)
        for buf = (grow input 0) then (tick buf 0)
        for (pos val) = (multiple-value-list
                         (interesting (ironclad::digest-sequence digest buf)))
        do (ironclad::reinitialize-instance digest)
        when (and val (< pos n) (char= #\_ (char password pos)))
          do (setf (char password pos) (hex-digit val))
        until (not (find #\_ password))
        finally (return password)))

(defun part2 (input)
  (set-matches 8 (octets (remove #\Newline input))))
