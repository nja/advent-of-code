;;;; day19.lisp

(in-package :aoc2023.day19)

(defun parse (input)
  (list (parse-workflows (first (aoc:sections input)))
        (parse-parts (second (aoc:sections input)))))

(defun parse-workflows (section)
  (flet ((read-s (x)
           (let ((*package* (symbol-package 'parse-workflows)))
             (read-from-string x))))
    (mapcar (a:compose
             (lambda (strings)
               (cons (read-s (first strings))
                     (mapcar (lambda (s)
                               (mapcar #'read-s (remove ":"
                                                        (ppcre:split "\\b" s)
                                                        :test #'string=)))
                             (rest strings))))
             (lambda (line)
               (str:split "," (str:trim (aoc:tr "{}" ", " line)))))
            (aoc:lines section))))

(defun parse-parts (section)
  (mapcar (lambda (line)
            (read-from-string (aoc:tr "{}xmas=," "()      )" line)))
          (aoc:lines section)))

(defun assemble-workflow (workflow)
  (cons (first workflow)
        (mapcar (lambda (rule)
                  (case (length rule)
                    (4 (destructuring-bind (a b c d) rule
                         `(when (,b ,a ,c) (go ,d))))
                    (1 (cons 'go rule))))
                (rest workflow))))

(defun assemble (workflows)
  `(lambda (x m a s)
     (declare ((integer 1 4000) x m a s) (optimize (speed 3) (safety 0) (debug 0) (space 0)))
     (block workflow
       (tagbody
          (go in)
          ,@(mapcan #'assemble-workflow workflows)
        a
          (return-from workflow (+ x m a s))
        r))))

(defun ratings (workflows parts)
  (mapcar (a:curry #'apply (eval (assemble workflows))) parts))

(defun part1 (input)
  (reduce #'+ (remove nil (apply #'ratings (parse input)))))

(defparameter *parts* (second (parse (aoc:input))))

(defun test ()
  (eq 383682 (reduce #'+ (remove nil (mapcar (lambda (x) (apply #'optimized x)) *parts*)))))


(defun test2 (workflows)
  (eq 383682 (reduce #'+ (remove nil (mapcar (a:curry #'apply (eval (assemble workflows))) *parts*)))))


(defparameter *test*
  "px{a<2006:qkq,m>2090:A,rfg}
pv{a>1716:R,A}
lnx{m>1548:A,A}
rfg{s<537:gd,x>2440:R,A}
qs{s>3448:A,lnx}
qkq{x<1416:A,crn}
crn{x>2662:A,R}
in{s<1351:px,qqz}
qqz{s>2770:qs,m<1801:hdj,R}
gd{a>3333:R,R}
hdj{m>838:A,pv}

{x=787,m=2655,a=1222,s=2876}
{x=1679,m=44,a=2067,s=496}
{x=2036,m=264,a=79,s=2244}
{x=2461,m=1339,a=466,s=291}
{x=2127,m=1623,a=2188,s=1013}")

;;; (WHEN (< S 2403) (GO PXM))
;;; (WHEN (< S 1369) (GO VJH))
;;; (WHEN (< S 696) (GO PTN))
;;; (WHEN (< A 2505) (GO LK))

(defun rlis (from to list)
  (sublis (list (cons from to))))

(defun always-go-to-a (workflows)
  (reduce (lambda (workflows workflow)
            (destructuring-bind (tag . ops) workflow
              (if (every (lambda (x) (eq 'a (a:lastcar x))) ops)
                  (mapcar (lambda (other) (subst 'a tag other))
                          (remove workflow workflows))
                  workflows)))
          workflows :initial-value workflows))

(defun always-go-to-r (workflows)
  (reduce (lambda (workflows workflow)
            (destructuring-bind (tag . ops) workflow
              (if (every (lambda (x) (eq 'r (a:lastcar x))) ops)
                  (mapcar (lambda (other) (subst 'r tag other))
                          (remove workflow workflows))
                  workflows)))
          workflows :initial-value workflows))

(defun transforms (workflows)
  (funcall (a:compose #'always-go-to-r #'always-go-to-a) workflows))

(defun fixed (workflows &optional (n 0))
  (if (< 100 n)
      workflows
      (fixed (transforms workflows) (1+ n))))

(defun operations (workflows)
  (mapcar #'assemble-workflow (fixed workflows)))

(defun untangle (operations)
  (reduce (lambda (operations x)
            (destructuring-bind (tag . ops) x
              (if (= 1 (count)))))
          operations :initial-value operations))