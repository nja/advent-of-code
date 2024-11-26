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
  (let ((connections (mapcar (lambda (line) (read-from-string (format nil "(~a)" (aoc:tr ":" " " line))))
                             (aoc:lines input)))
        (nodes (make-hash-table)))
    (dolist (ids connections)
      (let ((nodes (mapcar (lambda (id) (or (gethash id nodes)
                                            (setf (gethash id nodes) (node id))))
                           ids)))
        (let ((a (first nodes)))
          (dolist (b (rest nodes))
            (let ((edge (apply #'edge (sort (list a b) #'string< :key (a:compose #'symbol-name #'id)))))
              (push edge (edges a))
              (push edge (edges b)))))))
    (remove-duplicates (a:flatten (mapcar #'edges (a:hash-table-values nodes))))))

(defstruct (node :conc-name (:constructor node (id &optional edges)) (:print-function print-node)) id edges)
(defstruct (edge :conc-name (:constructor edge (a b)) (:print-function print-edge)) a b)

(defun print-node (node stream depth)
  (declare (ignore depth))
  (format stream "<~a:~{~a~^,~}>" (id node) (mapcar #'id (connected node))))

(defun print-edge (edge stream depth)
  (declare (ignore depth))
  (format stream "~a/~a" (id (a edge)) (id (b edge))))

(defun connected (node)
  (mapcar (lambda (edge)
            (cond ((eq node (a edge)) (b edge))
                  ((eq node (b edge)) (a edge))
                  (t (error "~a is not part of its edge ~a" node edge))))
          (edges node)))

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
