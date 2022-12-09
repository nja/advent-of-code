;;;; leaderboard.lisp

(in-package #:aoc)

(defun scan-package-name (&optional (package *package*))
  (map 'list #'parse-integer
       (nth-value 1 (ppcre:scan-to-strings "^AOC(\\d{4})\\.DAY(\\d{2})$"
                                           (package-name package)))))

(defun default-year ()
  (or (first (scan-package-name)) (nth-value 5 (get-decoded-time))))

(defun default-day ()
  (a:clamp (or (second (scan-package-name)) (nth-value 3 (get-decoded-time)))
           1 25))

(defun leaderboard (&optional (year (default-year)))
  (multiple-value-bind (board at) (get-leaderboard (get-config :leaderboard) year)
    (when board
      (print-members board)
      (print-time at))))

(defun print-time (utc &optional (stream t))
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time utc (or (get-config :timezone) 0))
    (format stream "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d"
            year month date hour minute second)))

(defun get-leaderboard (id year)
  (let ((path (format nil "~d/leaderboard/private/view/~d.json" year id)))
    (multiple-value-bind (cached at) (get-cached path)
      (if cached
          (values cached at)
          (set-cached path
                      (parse-members (flexi-streams:octets-to-string
                                      (get-aoc-webpage path)
                                      :external-format :utf-8)))))))

(defun parse-members (board)
  (jsown:val (jsown:parse board) "members"))

(defun parse-member (obj)
  (labels ((v (key) (jsown:val obj key))
           (i (i) (format nil "~d" i))
           (stars (obj) (loop for i from 1 upto 25
                              for d = (jsown:val-safe obj (i i))
                              collect (cond ((jsown:keyp d (i 2)) #\*)
                                            ((jsown:keyp d (i 1)) #\')
                                            (t #\.)))))
    (list (v "local_score") (stars (v "completion_day_level")) (v "name"))))

(defun print-members (obj)
  (let* ((members (mapcar (a:compose #'parse-member (a:curry #'jsown:val obj))
                          (jsown:keywords obj)))
         (score (reduce #'max (mapcar #'first members)))
         (rows (length members))
         (rowfmt (getfmt rows))
         (scorefmt (getfmt score)))
    (format t "~a ~a~a~%" (getspc rows) (getspc score) (top-row rows 25 0 1))
    (format t "~a ~a~a~%" (getspc rows) (getspc score) (top-row rows 25 1 1))
    (loop for i from 1
          for m in (sort members #'> :key #'first)
          unless (= 0 (first m))
            do (format t "~?) ~? ~?~%"
                       rowfmt (list i)
                       scorefmt (list (first m))
                       "~{~a~^~} ~a" (rest m)))))

(defun timeline (&optional (day (default-day)) (year (default-year)))
  (multiple-value-bind (board at) (get-leaderboard (get-config :leaderboard) year)
    (when board
      (format t "~d-~2,'0d~%" year day)
      (let* ((ts (member-ts day (cdr board)))
             (names (remove-duplicates (mapcar #'car ts) :from-end t))
             (fmt (format nil "~a ~~a" (make-string (length ts) :initial-element #\.)))
             (lines (mapcar (lambda (name) (cons name (format nil fmt name))) names)))
        (loop for i from 0
              for (name ts) in ts
              do (setf (aref (cdr (assoc name lines :test #'equal)) i) #\*))
                                        ;(format t "~{~a~%~}" (sort (mapcar #'cdr lines) #'string<))
        (format t "~{~a~%~}" (mapcar #'cdr lines))
        (print-time at)))))

(defun member-ts (day board)
  (flet ((members () (mapcar #'cdr (cdr board)))
         (name (member) (jsown:val member "name"))
         (days (member) (jsown:val member "completion_day_level"))
         (day (days) (jsown:val-safe days (format nil "~d" day)))
         (ts (key day) (jsown:val-safe (jsown:val-safe day key) "get_star_ts")))
    (sort
     (remove nil (mapcan (lambda (member)
                           (let ((day (day (days member))))
                             (list (list (name member) (ts "1" day))
                                   (list (name member) (ts "2" day)))))
                         (members))
             :key #'cadr)
     #'< :key #'cadr)))
