;;;; writer.lisp

(in-package #:aoc)

(defun setup (&optional (day (default-day)) (year (default-year)))
  (multiple-value-prog1
      (apply #'values
             (append (multiple-value-list (ensure-package (package-designator year day)))
                     (multiple-value-list (ensure-file year day))
                     (multiple-value-list (ensure-asd-day year day))))
    (setf *package* (find-package (package-designator year day)))))

;;; package

(defun package-designator (year day)
  (a:make-keyword (format nil "AOC~4,'0d.DAY~2,'0d" year day)))

(defun ensure-package (designator)
  (let* ((packages (read-package-defs))
         (package (find designator packages :key #'cadr)))
    (unless package
      (push (package-definition designator) packages)
      (setf package (find designator packages :key #'cadr)))
    (with-open-file (out (system-pathname "package.lisp")
                         :direction :output :if-exists :supersede)
      (format out ";;;; package.lisp~%")
      (write-package-defs (canonical-package-definitions packages) out))
    (values (eval package) package)))

(defun slurp-file (filespec)
  (with-open-file (in filespec)
    (let ((eof (gensym)))
      (loop for obj = (read in nil eof)
            until (eq obj eof)
            collect obj))))

(defun read-package-defs ()
  (slurp-file (system-pathname "package.lisp")))

(defun same-year? (a b)
  (flet ((year (x) (ppcre:scan-to-strings "\\d{4}" (symbol-name (cadr x)))))
    (and a b (year a) (equal (year a) (year b)))))

(defun write-package-defs (defs destination)
  (loop for prev = def
        for def in defs
        for pretty? = (< 7 (length (a:flatten def)))
        unless (same-year? prev def)
          do (format destination "~%")
        do (print-form def destination pretty?)))

(defun print-form (form destination &optional (pretty t))
  (let ((*print-pretty* pretty)
        (*print-case* :downcase))
    (format destination "~s~%" form)))

(defun package< (a b)
  (labels ((comparable (x)
             (or (position x '(in-package :use :import-from))
                 (cond ((consp x) (if (str:starts-with-p "DEF" (symbol-name (car x)))
                                      (comparable (cadr x))
                                      (comparable (car x))))
                       ((symbolp x) (comparable (symbol-name x)))
                       ((str:starts-with-p "AOC2" x)
                        (format nil "~a~a" (code-char (1- char-code-limit)) x))
                       (t (coerce x 'string))))))
    (let ((a (comparable a))
          (b (comparable b)))
      (cond ((and (integerp a) (integerp b)) (< a b))
            ((integerp a) t)
            ((and (stringp a) (stringp b)) (string< a b))
            (t nil)))))

(defun canonical-package-definitions (package-definitions)
  (arrow-macros:as-> package-definitions x
    (mapcar #'canonical-package-definition x)
    (sort x #'package<)
    (remove-duplicates x :from-end t :test 'equalp)))

(defun canonical-package-definition (package-definition)
  (labels ((sort-cdr (x) (cons (car x) (sort (cdr x) #'package<))))
    (let* ((head (subseq package-definition 0 2))
           (tail (mapcar #'sort-cdr (subseq package-definition 2))))
      (append head (remove-duplicates (sort tail #'package<)
                                      :from-end t
                                      :test #'equalp)))))

(defun package-definition (designator)
  `(defpackage ,designator (:use :cl) (:local-nicknames (:a :alexandria))))

;;; dayNN.lisp

(defun ensure-file (year day)
  (let ((pathname (system-pathname (format nil "~4,'0d/day~2,'0d.lisp" year day))))
    (ensure-directories-exist pathname)
    (with-open-file (out pathname :direction :output :if-exists nil
                                  :if-does-not-exist :create)
      (when out
        (file-head (package-designator year day) out)))
    pathname))

(defun file-head (package-designator &optional destination)
  (format destination ";;;; ~a~%~%~(~s~)~%~%"
          (if (streamp destination)
              (file-namestring destination)
              package-designator)
          `(in-package ,package-designator)))

;;; asd

(defun ensure-asd-day (year day)
  (let* ((defsystem (read-defsystem))
         (module (ensure-component defsystem :module (format nil "~4,'0d" year)))
         (component (ensure-component module :file (format nil "day~2,'0d" day))))
    (with-open-file (out (asdf:system-source-file "advent-of-code")
                         :direction :output :if-exists :supersede)
      (format out ";;;; advent-of-code.asd~%")
      (print-form defsystem out))
    component))

(defun read-defsystem ()
  (let ((forms (slurp-file (asdf:system-source-file "advent-of-code"))))
    (assert (null (cdr forms)))
    (first forms)))

(defun ensure-component (thing type name)
  (symbol-macrolet ((components (getf (cddr thing) :components nil)))
    (pushnew (list type name) components :key #'cadr :test #'equal)
    (assert (eq type (first (find name components :key #'cadr :test #'equal))))
    (setf components (sort components #'compare-component))
    (find name components :key #'cadr :test #'equal)))

(defun compare-component (a b)
  (let ((type-a (symbol-name (first a)))
        (type-b (symbol-name (first b)))
        (name-a (second a))
        (name-b (second b)))
    (cond ((equal name-a "package") t)
          ((equal name-b "package") nil)
          ((string< type-a type-b) t)
          ((string< type-b type-a) nil)
          ((string< name-a name-b) t))))
