;;;; day19.lisp

(in-package :aoc2022.day19)

(defun blueprints (input)
  (mapcar (lambda (line)
            (read-from-string (format nil "(~a)" (ppcre:regex-replace-all "\\D+" line " "))))
          (aoc:lines input)))

(defun prices (numbers)
  (destructuring-bind (id a b c d e f) numbers
    (values (list (make-resources :ore-bots 1 :ores (- a))
                  (make-resources :clay-bots 1 :ores (- b))
                  (make-resources :obsidian-bots 1 :ores (- c) :clays (- d))
                  (make-resources :geode-bots 1 :ores (- e) :obsidians (- f)))
            id)))

(defun max-geodes-dfs (prices minutes)
  (let ((best (make-resources)))
    (labels ((rec (minute prices resources next)
               (flet ((potential ()
                        (+ (cracked resources)
                           (loop for m from minute to minutes
                                 for b from (geode-bots resources)
                                 sum b))))
                 (cond ((eql minute minutes)
                        (when (< (cracked best) (cracked resources))
                          (setf best resources))
                        (funcall next))
                       ((<= (potential) (cracked best))
                        (funcall next))
                       (t
                        (let ((income (income resources)))
                          (labels ((build (resources price minute next)
                                     (cond ((eql minute minutes)
                                            (rec minute prices resources next))
                                           ((affordable? resources price)
                                            (rec (1+ minute)
                                                 prices
                                                 (add income (add resources price))
                                                 next))
                                           (t
                                            (build (add resources income)
                                                   price
                                                   (1+ minute)
                                                   next))))
                                   (next-price (options)
                                     (if (null options)
                                         (funcall next)
                                         (build resources
                                                (first options)
                                                minute
                                                (lambda () (next-price (cdr options)))))))
                            (next-price (prune-prices prices income)))))))))
      (rec 0 prices (make-resources :ore-bots 1) (lambda () best)))))

(defun prune-prices (prices income)
  (flet ((affordable? (key)
           (every (lambda (price) (<= (abs (funcall key price)) (funcall key income)))
                  prices)))
    (remove-if (lambda (price)
                 (or (and (plusp (ore-bots price)) (affordable? #'ores))
                     (and (plusp (clay-bots price)) (affordable? #'clays))
                     (and (plusp (obsidian-bots price)) (affordable? #'obsidians))))
               prices)))

(defstruct (resources :conc-name)
  (ores 0) (clays 0) (obsidians 0) (cracked 0)
  (ore-bots 0) (clay-bots 0) (obsidian-bots 0) (geode-bots 0))

(defun add (a b)
  (make-resources
   :ores (+ (ores a) (ores b))
   :clays (+ (clays a) (clays b))
   :obsidians (+ (obsidians a) (obsidians b))
   :cracked (+ (cracked a) (cracked b))
   :ore-bots (+ (ore-bots a) (ore-bots b))
   :clay-bots (+ (clay-bots a) (clay-bots b))
   :obsidian-bots (+ (obsidian-bots a) (obsidian-bots b))
   :geode-bots (+ (geode-bots a) (geode-bots b))))

(defun affordable? (resources price)
  (let ((bought (add resources price)))
    (and (<= 0 (ores bought))
         (<= 0 (clays bought))
         (<= 0 (obsidians bought))
         bought)))

(defun income (resources)
  (make-resources :ores (ore-bots resources)
                  :clays (clay-bots resources)
                  :obsidians (obsidian-bots resources)
                  :cracked (geode-bots resources)))

(defun quality (blueprint)
  (* (first blueprint) (cracked (max-geodes-dfs (prices blueprint) 24))))

(defun part1 (input)
  (reduce #'+ (mapcar #'quality (blueprints input))))

(defun part2 (input)
  (reduce #'* (mapcar (lambda (blueprint)
                        (cracked (max-geodes-dfs (prices blueprint) 32)))
                      (subseq (blueprints input) 0 3))))
