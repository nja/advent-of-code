;;;; package.lisp

(in-package :cl-user)

(defpackage :aoc
  (:use :cl)
  (:export :comparisons
           :defanswer
           :get-input
           :input
           :leaderboard
           :lines
           :print-array
           :print-indexed-lines
           :save-input
           :sections
           :set-config
           :setup
           :strip-cr
           :submit
           :symbols
           :timeline
           :to-array
           :tr
           :trim-lf)
  (:local-nicknames (:a :alexandria)))

(defpackage :dijkstra
  (:use :cl)
  (:export :distance :item :previous :search*)
  (:local-nicknames (:a :alexandria) (:q :pettomato-indexed-priority-queue)))

(defpackage :aoc2015.day01 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2015.day02 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2015.day03 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2015.day04 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2015.day05 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2015.day06 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2015.day07 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2015.day08 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2015.day09 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2015.day10 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2015.day11 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2015.day12 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2015.day13 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2015.day14 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2015.day15 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2015.day16 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2015.day17 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2015.day18 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2015.day19 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2015.day20 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2015.day21 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2015.day22 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2015.day23 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2015.day24 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2015.day25 (:use :cl) (:local-nicknames (:a :alexandria)))
(fiasco:define-test-package :aoc2015.tests (:use :aoc))

(defpackage :aoc2016.day01 (:use :cl) (:shadow :step))
(defpackage :aoc2016.day02 (:use :cl) (:import-from :alexandria :clamp))
(defpackage :aoc2016.day03 (:use :cl) (:import-from :alexandria :curry))
(defpackage :aoc2016.day04 (:use :cl))
(defpackage :aoc2016.day05 (:use :cl))
(defpackage :aoc2016.day06 (:use :cl))
(defpackage :aoc2016.day07 (:use :cl))
(defpackage :aoc2016.day08 (:use :cl))
(defpackage :aoc2016.day09 (:use :cl))
(defpackage :aoc2016.day10 (:use :cl))
(defpackage :aoc2016.day11
  (:use :cl)
  (:local-nicknames (:a :alexandria) (:d :dijkstra)))
(defpackage :aoc2016.day12 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2016.day13
  (:use :cl)
  (:local-nicknames (:a :alexandria) (:d :dijkstra)))
(defpackage :aoc2016.day14
  (:use :cl)
  (:local-nicknames (:a :alexandria) (:i :ironclad)))
(defpackage :aoc2016.day15 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2016.day16 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2016.day17
  (:use :cl)
  (:local-nicknames (:a :alexandria) (:d :dijkstra) (:i :ironclad)))
(defpackage :aoc2016.day18 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2016.day19 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2016.day20 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2016.day21 (:use :cl) (:local-nicknames (:a :alexandria)))
(fiasco:define-test-package :aoc2016.tests (:use :aoc))

(defpackage :aoc2017.day01 (:use :cl))
(defpackage :aoc2017.day02 (:use :cl))
(defpackage :aoc2017.day03 (:use :cl))
(defpackage :aoc2017.day04 (:use :cl))
(defpackage :aoc2017.day05 (:use :cl))
(defpackage :aoc2017.day06 (:use :cl))
(defpackage :aoc2017.day07 (:use :cl))
(defpackage :aoc2017.day08 (:use :cl))
(defpackage :aoc2017.day09 (:use :cl))
(defpackage :aoc2017.day10 (:use :cl) (:export :hash))
(defpackage :aoc2017.day11 (:use :cl))
(defpackage :aoc2017.day12 (:use :cl) (:export :join))
(defpackage :aoc2017.day13 (:use :cl))
(defpackage :aoc2017.day14 (:use :cl :aoc2017.day10 :aoc2017.day12))
(defpackage :aoc2017.day15 (:use :cl))
(defpackage :aoc2017.day15.alt (:use :cl))
(defpackage :aoc2017.day16 (:use :cl))
(defpackage :aoc2017.day17 (:use :cl))
(defpackage :aoc2017.day18 (:use :cl :queues))
(defpackage :aoc2017.day19 (:use :cl))
(defpackage :aoc2017.day20 (:use :cl))
(defpackage :aoc2017.day21 (:use :cl))
(defpackage :aoc2017.day22 (:use :cl))
(defpackage :aoc2017.day23 (:use :cl))
(defpackage :aoc2017.day24 (:use :cl))
(defpackage :aoc2017.day25 (:use :cl))
(fiasco:define-test-package :aoc2017.tests (:use :aoc))

(defpackage :aoc2018.day01 (:use :cl))
(defpackage :aoc2018.day02 (:use :cl))
(defpackage :aoc2018.day03 (:use :cl))
(defpackage :aoc2018.day04 (:use :cl))
(defpackage :aoc2018.day05 (:use :cl))
(defpackage :aoc2018.day06 (:use :cl))
(defpackage :aoc2018.day07
  (:use :cl)
  (:import-from :alexandria :compose :curry))
(defpackage :aoc2018.day08 (:use :cl))
(defpackage :aoc2018.day09 (:use :cl))
(defpackage :aoc2018.day10 (:use :cl) (:import-from :alexandria :curry))
(defpackage :aoc2018.day11 (:use :cl))
(defpackage :aoc2018.day12 (:use :cl))
(defpackage :aoc2018.day13 (:use :cl))
(defpackage :aoc2018.day14 (:use :cl))
(defpackage :aoc2018.day15
  (:use :cl)
  (:local-nicknames (:a :alexandria) (:d :dijkstra)))
(defpackage :aoc2018.day16 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2018.day17 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2018.day18 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2018.day19 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2018.day20
  (:use :cl)
  (:local-nicknames (:a :alexandria) (:d :dijkstra)))
(defpackage :aoc2018.day21 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2018.day22
  (:use :cl)
  (:local-nicknames (:a :alexandria) (:d :dijkstra)))
(defpackage :aoc2018.day23 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2018.day24 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2018.day25 (:use :cl) (:local-nicknames (:a :alexandria)))
(fiasco:define-test-package :aoc2018.tests (:use :aoc))

(defpackage :aoc2019.day01 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2019.day02 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2019.day03 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2019.day04 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2019.day05
  (:use :cl)
  (:export :parse :run)
  (:local-nicknames (:a :alexandria)))
(defpackage :aoc2019.day06 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2019.day07
  (:use :cl :aoc2019.day05)
  (:local-nicknames (:a :alexandria)))
(defpackage :aoc2019.day08 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2019.day09 (:use :cl) (:local-nicknames (:a :alexandria)))
(fiasco:define-test-package :aoc2019.tests (:use :aoc))

(defpackage :aoc2020.day01 (:use :cl) (:import-from :alexandria :curry))
(defpackage :aoc2020.day02 (:use :cl))
(defpackage :aoc2020.day03 (:use :cl))
(defpackage :aoc2020.day04 (:use :cl))
(defpackage :aoc2020.day05 (:use :cl))
(defpackage :aoc2020.day06 (:use :alexandria :cl))
(defpackage :aoc2020.day07 (:use :cl :fare-memoization))
(defpackage :aoc2020.day08 (:use :alexandria :cl))
(defpackage :aoc2020.day09 (:use :alexandria :cl))
(defpackage :aoc2020.day10 (:use :alexandria :cl))
(defpackage :aoc2020.day11 (:use :alexandria :cl))
(defpackage :aoc2020.day12 (:use :alexandria :cl))
(defpackage :aoc2020.day13 (:use :alexandria :cl))
(defpackage :aoc2020.day14 (:use :alexandria :cl))
(defpackage :aoc2020.day15 (:use :alexandria :cl))
(defpackage :aoc2020.day16 (:use :alexandria :cl))
(defpackage :aoc2020.day17 (:use :alexandria :cl))
(defpackage :aoc2020.day18 (:use :alexandria :cl))
(defpackage :aoc2020.day19 (:use :alexandria :cl))
(defpackage :aoc2020.day20 (:use :cl))
(defpackage :aoc2020.day21 (:use :alexandria :cl))
(defpackage :aoc2020.day22 (:use :alexandria :cl))
(defpackage :aoc2020.day23 (:use :alexandria :cl))
(defpackage :aoc2020.day24 (:use :alexandria :cl))
(defpackage :aoc2020.day25 (:use :alexandria :cl))
(fiasco:define-test-package :aoc2020.tests (:use :aoc))

(defpackage :aoc2021.day01 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2021.day02 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2021.day03 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2021.day04 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2021.day05 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2021.day06 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2021.day07 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2021.day08 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2021.day09 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2021.day10 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2021.day11 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2021.day12 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2021.day13 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2021.day14 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2021.day15 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2021.day16 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2021.day17 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2021.day18 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2021.day19 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2021.day20 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2021.day21 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2021.day22 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2021.day23
  (:use :cl)
  (:local-nicknames (:a :alexandria) (:q :pettomato-indexed-priority-queue)))
(defpackage :aoc2021.day24 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2021.day25 (:use :cl) (:local-nicknames (:a :alexandria)))
(fiasco:define-test-package :aoc2021.tests (:use :aoc))

(defpackage :aoc2022.day01
  (:use :arrow-macros :cl)
  (:local-nicknames (:a :alexandria)))
(defpackage :aoc2022.day02 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2022.day03 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2022.day04 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2022.day05 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2022.day06 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2022.day07 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2022.day08 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2022.day09 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2022.day10 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2022.day11 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2022.day12
  (:use :cl)
  (:local-nicknames (:a :alexandria) (:d :dijkstra)))
(defpackage :aoc2022.day13 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2022.day14 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2022.day15 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2022.day16
  (:use :cl)
  (:local-nicknames (:a :alexandria) (:d :dijkstra)))
(defpackage :aoc2022.day17 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2022.day18
  (:use :cl)
  (:local-nicknames (:a :alexandria) (:d :dijkstra)))
(defpackage :aoc2022.day19 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2022.day20 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2022.day21 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2022.day22 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2022.day23 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2022.day24 (:use :cl) (:local-nicknames (:a :alexandria)))
(defpackage :aoc2022.day24
  (:use :cl)
  (:local-nicknames (:a :alexandria) (:d :dijkstra)))
(defpackage :aoc2022.day25 (:use :cl) (:local-nicknames (:a :alexandria)))
(fiasco:define-test-package :aoc2022.tests (:use :aoc))
