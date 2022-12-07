;;;; day07.lisp

(in-package #:aoc2022.day07)

(defun parse-command (command)
  (let ((*package* (symbol-package 'parse-command)))
    (with-input-from-string (in command)
      (case (read in)
        (cd (list 'cd (read-line in)))
        (ls (loop for a = (read in nil)
                  for b = (read-line in nil)
                  while a collect (typecase a (symbol b) (integer (list a b))) into files
                  finally (return (cons 'ls files))))))))

(defun parse (input)
  (mapcar #'parse-command (remove 0 (str:split #\$ input) :key #'length)))

(defparameter *cwd* nil)
(defparameter *dirs* nil)

(defun dirs ()
  (make-hash-table :test 'equal))

(defun subdir (x)
  (concatenate 'string (unless (equal *cwd* "/") *cwd*) "/" x))

(defun cd (dir)
  (setf *cwd* (cond ((string= dir "/") dir)
                    ((string= dir "..")
                     (subseq *cwd* 0 (max 1 (position #\/ *cwd* :from-end t))))
                    (t (subdir dir)))))

(defun ls (&rest files)
  (dolist (file files)
    (push (if (stringp file) (subdir file) file)
          (gethash *cwd* *dirs*))))

(defun size (x)
  (if (consp x)
      (car x)
      (reduce #'+ (mapcar #'size (remove-duplicates (gethash x *dirs*) :test 'equal)))))

(defun execute (commands)
  (loop for (command . args) in commands
        do (apply command args)))

(defun part1 (input)
  (let (*cwd* (*dirs* (dirs)))
    (execute (parse input))
    (reduce #'+ (remove-if (a:curry #'<= 100000)
                           (mapcar #'size (a:hash-table-keys *dirs*))))))

(defun size-needed ()
  (- 30000000 (- 70000000 (size "/"))))

(defun part2 (input)
  (let (*cwd* (*dirs* (dirs)))
    (execute (parse input))
    (reduce #'min (remove-if (a:curry #'>= (size-needed))
                             (mapcar #'size (a:hash-table-keys *dirs*))))))
