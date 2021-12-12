;;;; day12.lisp

(in-package #:aoc2021.day12)

(defun arcs (input)
  (mapcar #'arc (aoc:lines input)))

(defun arc (line)
  (mapcar #'node-symbol (str:split #\- line)))

(defun node-symbol (string)
  (intern string (find-package '#:aoc2021.day12)))

(defun add (graph from to)
  (unless (or (eq from '|end|) (eq to '|start|))
    (push to (cdr (assoc from graph)))))

(defun connect (graph a b)
  (add graph a b)
  (add graph b a))

(defun nodes (arcs)
  (remove-duplicates (a:flatten arcs)))

(defun empty-graph (arcs)
  (mapcar #'list (nodes arcs)))

(defun graph (arcs)
  (let ((graph (empty-graph arcs)))
    (mapc (lambda (x)
            (apply #'connect graph x))
          arcs)
    graph))

(defun once-only? (node)
  (lower-case-p (aref (symbol-name node) 0)))

(defparameter *allow-one-revisit?* nil)

(defun revisited? (trail)
  (let ((small-caves (remove-if-not #'once-only? trail)))
    (not (equal small-caves (remove-duplicates small-caves)))))

(defun blocked? (trail node)
  (when (once-only? node)
    (if *allow-one-revisit?*
        (and (member node trail) (revisited? trail))
        (member node trail))))

(defun connected (graph node)
  (cdr (assoc node graph)))

(defun next-nodes (graph trail)
  (remove-if (a:curry #'blocked? trail)
             (connected graph (first trail))))

(defun full-path? (trail)
  (eq '|end| (first trail)))

(defun new-trails (graph trail)
  (mapcar (lambda (x)
            (cons x trail))
          (next-nodes graph trail)))

(defun walker (graph trails back result)
  (cond ((null trails)
         (funcall back result))
        ((full-path? (first trails))
         (walker graph (rest trails) back (cons (first trails) result)))
        (t (let ((next (new-trails graph (first trails))))
             (if next
                 (walker graph next (lambda (r) (walker graph (rest trails) back r)) result)
                 (walker graph (rest trails) back result))))))

(defun walk (graph)
  (length (walker graph '((|start|)) #'identity nil)))

(defun part1 (input)
  (walk (graph (arcs input))))

(defun part2 (input)
  (let ((*allow-one-revisit?* t))
    (walk (graph (arcs input)))))
