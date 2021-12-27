;;;; package.lisp

(defpackage #:aoc
  (:use #:cl)
  (:export #:input-for #:lines #:trim-lf #:strip-cr #:tr #:print-array #:to-array
           #:symbols #:get-input #:save-input #:sections #:set-config #:leaderboard)
  (:local-nicknames (#:a #:alexandria)))

(defpackage #:aoc2016.day01 (:use #:cl) (:shadow #:step))
(defpackage #:aoc2016.day02 (:use #:cl) (:import-from #:alexandria #:clamp))
(defpackage #:aoc2016.day03 (:use #:cl) (:import-from #:alexandria #:curry))
(defpackage #:aoc2016.day04 (:use #:cl))
(defpackage #:aoc2016.day05 (:use #:cl))
(defpackage #:aoc2016.day06 (:use #:cl))
(defpackage #:aoc2016.day07 (:use #:cl))
(defpackage #:aoc2016.day08 (:use #:cl))
(defpackage #:aoc2016.day09 (:use #:cl))
(defpackage #:aoc2016.day10 (:use #:cl))

(defpackage #:aoc2017.day01 (:use #:cl))
(defpackage #:aoc2017.day02 (:use #:cl))
(defpackage #:aoc2017.day03 (:use #:cl))
(defpackage #:aoc2017.day04 (:use #:cl))
(defpackage #:aoc2017.day05 (:use #:cl))
(defpackage #:aoc2017.day06 (:use #:cl))
(defpackage #:aoc2017.day07 (:use #:cl))
(defpackage #:aoc2017.day08 (:use #:cl))
(defpackage #:aoc2017.day09 (:use #:cl))
(defpackage #:aoc2017.day10 (:use #:cl) (:export #:hash))
(defpackage #:aoc2017.day11 (:use #:cl))
(defpackage #:aoc2017.day12 (:use #:cl) (:export #:join))
(defpackage #:aoc2017.day13 (:use #:cl))
(defpackage #:aoc2017.day14 (:use #:cl #:aoc2017.day10 #:aoc2017.day12))
(defpackage #:aoc2017.day15 (:use #:cl))
(defpackage #:aoc2017.day15.alt (:use #:cl))
(defpackage #:aoc2017.day16 (:use #:cl))
(defpackage #:aoc2017.day17 (:use #:cl))
(defpackage #:aoc2017.day18 (:use #:cl #:queues))
(defpackage #:aoc2017.day19 (:use #:cl))
(defpackage #:aoc2017.day20 (:use #:cl))
(defpackage #:aoc2017.day21 (:use #:cl))
(defpackage #:aoc2017.day22 (:use #:cl))
(defpackage #:aoc2017.day23 (:use #:cl))
(defpackage #:aoc2017.day24 (:use #:cl))
(defpackage #:aoc2017.day25 (:use #:cl))

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
(defpackage #:aoc2018.day15 (:use #:cl))

(defpackage #:aoc2020.day01 (:use #:cl) (:import-from #:alexandria #:curry))
(defpackage #:aoc2020.day02 (:use #:cl))
(defpackage #:aoc2020.day03 (:use #:cl))
(defpackage #:aoc2020.day04 (:use #:cl))
(defpackage #:aoc2020.day05 (:use #:cl))
(defpackage #:aoc2020.day06 (:use #:cl #:alexandria))
(defpackage #:aoc2020.day07 (:use #:cl #:fare-memoization))
(defpackage #:aoc2020.day08 (:use #:cl #:alexandria))
(defpackage #:aoc2020.day09 (:use #:cl #:alexandria))
(defpackage #:aoc2020.day10 (:use #:cl #:alexandria))
(defpackage #:aoc2020.day11 (:use #:cl #:alexandria))
(defpackage #:aoc2020.day12 (:use #:cl #:alexandria))
(defpackage #:aoc2020.day13 (:use #:cl #:alexandria))
(defpackage #:aoc2020.day14 (:use #:cl #:alexandria))
(defpackage #:aoc2020.day15 (:use #:cl #:alexandria))
(defpackage #:aoc2020.day16 (:use #:cl #:alexandria))
(defpackage #:aoc2020.day17 (:use #:cl #:alexandria))
(defpackage #:aoc2020.day18 (:use #:cl #:alexandria))
(defpackage #:aoc2020.day19 (:use #:cl #:alexandria))
(defpackage #:aoc2020.day20 (:use #:cl))
(defpackage #:aoc2020.day21 (:use #:cl #:alexandria))
(defpackage #:aoc2020.day22 (:use #:cl #:alexandria))
(defpackage #:aoc2020.day23 (:use #:cl #:alexandria))
(defpackage #:aoc2020.day24 (:use #:cl #:alexandria))
(defpackage #:aoc2020.day25 (:use #:cl #:alexandria))

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
(defpackage #:aoc2021.day12 (:use #:cl) (:local-nicknames (#:a #:alexandria)))
(defpackage #:aoc2021.day13 (:use #:cl) (:local-nicknames (#:a #:alexandria)))
(defpackage #:aoc2021.day14 (:use #:cl) (:local-nicknames (#:a #:alexandria)))
(defpackage #:aoc2021.day15 (:use #:cl) (:local-nicknames (#:a #:alexandria)))
(defpackage #:aoc2021.day16 (:use #:cl) (:local-nicknames (#:a #:alexandria)))
(defpackage #:aoc2021.day17 (:use #:cl) (:local-nicknames (#:a #:alexandria)))
(defpackage #:aoc2021.day18 (:use #:cl) (:local-nicknames (#:a #:alexandria)))
(defpackage #:aoc2021.day19 (:use #:cl) (:local-nicknames (#:a #:alexandria)))
(defpackage #:aoc2021.day20 (:use #:cl) (:local-nicknames (#:a #:alexandria)))
(defpackage #:aoc2021.day21 (:use #:cl) (:local-nicknames (#:a #:alexandria)))

(fiasco:define-test-package #:aoc2016.tests (:use #:aoc))
(fiasco:define-test-package #:aoc2017.tests (:export #:input-for #:lines))
(fiasco:define-test-package #:aoc2018.tests (:use #:aoc))
(fiasco:define-test-package #:aoc2020.tests (:use #:aoc))
(fiasco:define-test-package #:aoc2021.tests (:use #:aoc))
