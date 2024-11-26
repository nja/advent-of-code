;;;; day25.lisp

(in-package :aoc2023.day25)

(defvar *test*
"jqt: rhn xhk nvd
rsh: frs pzl lsr
xhk: hfx
cmg: qnr nvd lhk bvb
rhn: xhk bvb hfx
bvb: xhk hfx
pzl: lsr hfx nvd
qnr: nvd
ntq: jqt hfx bvb xhk
nvd: lhk
lsr: lhk
rzs: qnr cmg lsr rsh
frs: qnr lhk lsr")

(defun parse (input)
  (mapcar (lambda (line) (read-from-string (format nil "(~a)" (aoc:tr ":" " " line))))
          (aoc:lines input)))

(defun pair (a b)
  (if (string< (symbol-name a) (symbol-name b))
      (cons a b)
      (cons b a)))

(defun link (connections)
  (let ((links (make-hash-table :test 'equal)))
    (mapc (lambda (list)
            (mapc (lambda (x) (incf (gethash (pair (first list) x) links 0)))
                  (rest list)))
          connections)
    links))

(defun counts (connections)
  (let ((counts (make-hash-table)))
    (mapc (lambda (list)
            (mapc (lambda (x)
                    (incf (gethash (first list) counts 0))
                    (incf (gethash x counts 0)))
                  (rest list)))
          connections)
    counts))
