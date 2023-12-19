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