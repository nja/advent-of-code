;;;; web.lisp

(in-package #:aoc)

(defun config-pathname ()
  (asdf:system-relative-pathname :advent-of-code "config.txt"))

(defun read-when-exists (pathname)
  (a:when-let ((truename (probe-file pathname)))
    (read-from-string (a:read-file-into-string truename) nil)))

(defun read-config ()
  (read-when-exists (config-pathname)))

(defun write-config (config)
  (a:with-output-to-file (file (config-pathname) :if-exists :overwrite)
    (prin1 config file)
    (fresh-line file)))

(defun get-config (key)
  (getf (read-config) key))

(defun set-config (key value)
  (let ((config (read-config)))
    (setf (getf config key) value)
    (write-config config)))

(defun session-cookie ()
  (make-instance 'drakma:cookie
                 :name "session"
                 :value (get-config :session)
                 :expires (+ (get-universal-time) (* 365 24 60 60))
                 :domain ".adventofcode.com"
                 :http-only-p t
                 :securep t))

(defun cookie-jar ()
  (make-instance 'drakma:cookie-jar :cookies (list (session-cookie))))

(defun get-aoc-webpage (path)
  (drakma:http-request (format nil "https://adventofcode.com/~a" path)
                       :cookie-jar (cookie-jar)))

(defun get-input (year day)
  (get-aoc-webpage (format nil "~d/day/~d/input" year day)))

(defun save-input (year day)
  (alexandria:write-string-into-file
   (get-input year day)
   (input-path year day)
   :if-exists :supersede))

(defun cache-pathname ()
  (asdf:system-relative-pathname :advent-of-code "cache.txt"))

(defun read-cache ()
  (read-when-exists (cache-pathname)))

(defun get-cached (path)
  (a:when-let* ((cache (read-cache))
                (cached (cdr (find path cache :key #'car :test #'equal))))
    (destructuring-bind (at what) cached
      (when (< (get-universal-time) (+ at 900))
        (values what at)))))

(defun write-cache (cache)
  (a:with-output-to-file (file (cache-pathname) :if-exists :overwrite)
    (prin1 cache file)
    (fresh-line file)))

(defun set-cached (path what)
  (let ((cache (remove path (read-cache) :key #'car :test #'equal))
        (at (get-universal-time)))
    (write-cache (cons (list path at what) cache))
    (values what at)))
