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

(defun post-aoc-webpage (path parameters)
  (drakma:http-request (format nil "https://adventofcode.com/~a" path)
                       :cookie-jar (cookie-jar)
                       :method :post
                       :parameters parameters))

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

(defun get-cached (path &optional (seconds 900))
  (a:when-let* ((cache (read-cache))
                (cached (cdr (find path cache :key #'car :test #'equalp))))
    (destructuring-bind (at what) cached
      (when (< (get-universal-time) (+ at seconds))
        (values what at)))))

(defun write-cache (cache)
  (a:with-output-to-file (file (cache-pathname) :if-exists :overwrite)
    (prin1 cache file)
    (fresh-line file)))

(defun set-cached (path what)
  (let* ((cache (remove path (read-cache) :key #'car :test #'equalp))
         (clean (clean-cache cache))
         (at (get-universal-time))
         (new (cons (list path at what) clean)))
    (write-cache new)
    (values what at)))

(defun clean-cache (cache &optional (grace-seconds (* 60 60)))
  (remove-if (let ((now (get-universal-time)))
               (lambda (at)
                 (< (+ at grace-seconds) now)))
             cache
             :key #'cadr))

(defun submit (part answer &key (year (default-year)) (day (default-day)))
  (declare (type integer year day part) (type (or integer string) answer))
  (assert (<= 2015 year))
  (assert (<= 1 day 25))
  (assert (<= 1 part 2))
  (flet ((to-string (x)
           (etypecase x
             (string x)
             (integer (format nil "~d" x)))))
    (format t "~&~4d ~2,'0d ~d: '~a'~%" year day part (to-string answer))
    (let* ((path (format nil "~d/day/~d/answer" year day))
           (cache-key (cons path part))
           (parameters (list (cons "level" (to-string part))
                             (cons "answer" (to-string answer)))))
      (multiple-value-bind (response at)
          (multiple-value-bind (cached at) (get-cached cache-key 60)
            (if cached
                (values cached at)
                (set-cached cache-key (post-aoc-webpage path parameters))))
        (stash response)
        (format t "~&~{~a~%~}(Cached until ~a)~%"
                (parse-response response) (print-time at nil))
        answer))))

(defun parse-response (response)
  (let* ((relevant '("That's the right answer."
                     "That's not the right answer[^.]*\\."
                     "Please wait[^.]*\\."
                     "You gave an answer too recently"
                     "You have [^.]+ left to wait."
                     "You don't seem to be solving the right level."
                     "Did you already complete it\\?"
                     "Congratulations!"
                     "You've finished every puzzle in Advent of Code \\d+!"
                     "I hope you had as much fun solving them as I had making them for you."))
         (regex (format nil "(~{~a~^|~})" relevant)))
    (or (ppcre:all-matches-as-strings regex response)
        (ppcre:all-matches-as-strings "<article>.*</article>" response)
        (ppcre:all-matches-as-strings "<main>.*</main>" response)
        (ppcre:all-matches-as-strings "<body>.*</body>" response)
        (ppcre:all-matches-as-strings "<html>.*</html>" response)
        response)))

(defun stash (x)
  (let* ((path (system-pathname "stash.txt"))
         (stash (read-from-string (a:read-file-into-string path))))
    (push x stash)
    (a:with-output-to-file (file path :if-exists :overwrite)
      (prin1 stash file)
      (fresh-line file))
    x))

(defun parse-stash ()
  (let* ((path (system-pathname "stash.txt"))
         (stash (read-from-string (a:read-file-into-string path))))
    (mapcar #'aoc::parse-response stash)))
