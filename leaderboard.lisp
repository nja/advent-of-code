;;;; leaderboard.lisp

(in-package #:aoc)

(defun leaderboard (&optional (year (nth-value 5 (get-decoded-time))))
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
