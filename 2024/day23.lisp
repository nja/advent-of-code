;;;; day23.lisp

(in-package :aoc2024.day23)

(defun parse (input)
  (mapcar (lambda (line)
            (mapcar (lambda (s)
                      (intern (string-upcase s) (symbol-package 'parse)))
                    (str:split "-" line)))
          (aoc:lines input)))

(defun network (pairs)
  (let ((network (make-hash-table :test 'equal)))
    (flet ((add (a b)
             (setf (gethash (cons a b) network) t)
             (push b (gethash a network (list a)))))
      (dolist (pair pairs network)
        (apply #'add pair)
        (apply #'add (reverse pair))))))

(defun connected? (network a b)
  (gethash (cons a b) network))

(defun nodes (network)
  (remove-if-not #'symbolp (a:hash-table-keys network)))

(defun all-interconnected? (network nodes)
  (and (every (lambda (n)
                (every (lambda (o)
                         (or (eq n o) (connected? network n o)))
                       nodes))
              nodes)
       nodes))

(defun starts-with-t? (node)
  (str:starts-with? "T" (symbol-name node)))

(defun neighbourses (network)
  (remove-if-not #'listp (a:hash-table-values network)))

(defun count-interconnected (network size predicate)
  (let ((counts (make-hash-table :test 'equal)))
    (dolist (neighbours (neighbourses network) (hash-table-count counts))
      (a:map-combinations
       (lambda (nodes)
         (when (and (funcall predicate nodes) (all-interconnected? network nodes))
           (setf (gethash (sort nodes #'string< :key #'symbol-name) counts) t)))
       neighbours :length size))))

(defun part1 (input)
  (count-interconnected (network (parse input)) 3 (a:curry #'some #'starts-with-t?)))

(defun largest-interconnected (network)
  (loop for n from 0
        when (some (lambda (neighbours)
                     (some (a:curry #'all-interconnected? network)
                           (all-but n neighbours)))
                   (neighbourses network))
          return it))

(defun all-but (n nodes)
  (let (result)
    (a:map-combinations (lambda (buts) (push (remove-if (a:rcurry #'member buts) nodes) result))
     nodes :length n)
    result))

(defun password (nodes)
  (string-downcase (format nil "~{~a~^,~}" (sort nodes #'string< :key #'symbol-name))))

(defun part2 (input)
  (password (largest-interconnected (network (parse input)))))
