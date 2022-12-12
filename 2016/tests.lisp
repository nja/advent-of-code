;;;; tests.lisp

(in-package #:aoc2016.tests)

(defanswer day01 273 115)
(defanswer day02 '(4 7 9 7 8) '(6 5 9 :A :D))
(defanswer day03 1032 1838)
(defanswer day04 278221 267)
(defanswer day05 "2414BC77" "437E60FC")
(defanswer day06 "tsreykjj" "hnfbujie")
(defanswer day07 115 231)

(deftest day08 ()
  (is (= 116 (aoc2016.day08::part1 (input 8 2016))))
  (is (equal (aoc:lines (aoc2016.day08::part2 (input 8 2016))) '(""
"X  X XXX   XX    XX XXXX X    XXX   XX  XXXX XXXX "
"X  X X  X X  X    X X    X    X  X X  X X       X "
"X  X X  X X  X    X XXX  X    XXX  X    XXX    X  "
"X  X XXX  X  X    X X    X    X  X X    X     X   "
"X  X X    X  X X  X X    X    X  X X  X X    X    "
" XX  X     XX   XX  X    XXXX XXX   XX  XXXX XXXX "
))))

(defanswer day09 102239 10780403063)
(defanswer day10 73 3965)
(defanswer day11 31 55)
(defanswer day12 317993 9227647)
(defanswer day13 86 127)
(defanswer day14 15168 20864)
(defanswer day15 203660 2408135)
(defanswer day16 "10111110010110110" "01101100001100100")
(defanswer day17 "DUDRDLRRRD"  502)
(defanswer day18 1956 19995121)
(defanswer day19 1842613 1424135)
(defanswer day20 32259706 113)
(defanswer day21 "ghfacdbe" "fhgcdaeb")
