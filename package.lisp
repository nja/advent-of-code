;;;; package.lisp

(defpackage #:aoc
  (:use #:cl)
  (:export #:input-for #:lines #:trim-lf #:strip-cr #:tr #:print-array #:to-array
           #:symbols #:get-input #:save-input #:sections #:set-config #:leaderboard)
  (:local-nicknames (#:a #:alexandria)))

(defpackage #:aoc2021.day01 (:use #:cl) (:local-nicknames (#:a #:alexandria)))
(defpackage #:aoc2021.day02 (:use #:cl) (:local-nicknames (#:a #:alexandria)))
(defpackage #:aoc2021.day03 (:use #:cl) (:local-nicknames (#:a #:alexandria)))
(defpackage #:aoc2021.day04 (:use #:cl) (:local-nicknames (#:a #:alexandria)))
(defpackage #:aoc2021.day05 (:use #:cl) (:local-nicknames (#:a #:alexandria)))
(defpackage #:aoc2021.day06 (:use #:cl) (:local-nicknames (#:a #:alexandria)))
(defpackage #:aoc2021.day07 (:use #:cl) (:local-nicknames (#:a #:alexandria)))
(defpackage #:aoc2021.day08 (:use #:cl) (:local-nicknames (#:a #:alexandria)))
(defpackage #:aoc2021.day09 (:use #:cl) (:local-nicknames (#:a #:alexandria)))
(defpackage #:aoc2021.day10 (:use #:cl) (:local-nicknames (#:a #:alexandria)))
(defpackage #:aoc2021.day11 (:use #:cl) (:local-nicknames (#:a #:alexandria)))

(defpackage #:aoc2018.day01 (:use #:cl))
(defpackage #:aoc2018.day02 (:use #:cl))
(defpackage #:aoc2018.day03 (:use #:cl))
(defpackage #:aoc2018.day04 (:use #:cl))
(defpackage #:aoc2018.day05 (:use #:cl))
(defpackage #:aoc2018.day06 (:use #:cl))
(defpackage #:aoc2018.day07 (:use #:cl) (:import-from #:alexandria #:compose #:curry))
(defpackage #:aoc2018.day08 (:use #:cl))
(defpackage #:aoc2018.day09 (:use #:cl))
(defpackage #:aoc2018.day10 (:use #:cl) (:import-from #:alexandria #:curry))
(defpackage #:aoc2018.day11 (:use #:cl))
(defpackage #:aoc2018.day12 (:use #:cl))
(defpackage #:aoc2018.day13 (:use #:cl))
(defpackage #:aoc2018.day14 (:use #:cl))

(fiasco:define-test-package #:aoc2018.tests (:use #:aoc))
(fiasco:define-test-package #:aoc2021.tests (:use #:aoc))
