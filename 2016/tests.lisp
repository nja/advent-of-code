;;;; tests.lisp

(in-package #:aoc2016.tests)

(defun system-file (name)
  (asdf:system-relative-pathname :advent-of-code name))

(deftest day01 ()
  (is (= 273 (aoc2016.day01::part1 (input-for 2016 1))))
  (is (= 115 (aoc2016.day01::part2 (input-for 2016 1)))))

(deftest day02 ()
  (is (equal '(4 7 9 7 8) (aoc2016.day02::part1 (input-for 2016 2))))
  (is (equal '(6 5 9 :A :D) (aoc2016.day02::part2 (input-for 2016 2)))))

(deftest day03 ()
  (is (= 1032 (aoc2016.day03::part1 (input-for 2016 3))))
  (is (= 1838 (aoc2016.day03::part2 (input-for 2016 3)))))

(deftest day04 ()
  (is (= 278221 (aoc2016.day04::part1 (input-for 2016 4))))
  (is (= 267 (aoc2016.day04::part2 (input-for 2016 4)))))

(deftest day05 ()
  (is (string= "2414BC77" (aoc2016.day05::part1 (input-for 2016 5))))
  (is (string= "437E60FC" (aoc2016.day05::part2 (input-for 2016 5)))))

(deftest day06 ()
  (is (string= "tsreykjj" (aoc2016.day06::part1 (input-for 2016 6))))
  (is (string= "hnfbujie" (aoc2016.day06::part2 (input-for 2016 6)))))

(deftest day07 ()
  (is (= 115 (aoc2016.day07::part1 (input-for 2016 7))))
  (is (= 231 (aoc2016.day07::part2 (input-for 2016 7)))))

(deftest day08 ()
  (is (= 116 (aoc2016.day08::part1 (input-for 2016 8))))
  (is (equal (aoc:lines (aoc2016.day08::part2 (input-for 2016 8))) '(""
"X  X XXX   XX    XX XXXX X    XXX   XX  XXXX XXXX "
"X  X X  X X  X    X X    X    X  X X  X X       X "
"X  X X  X X  X    X XXX  X    XXX  X    XXX    X  "
"X  X XXX  X  X    X X    X    X  X X    X     X   "
"X  X X    X  X X  X X    X    X  X X  X X    X    "
" XX  X     XX   XX  X    XXXX XXX   XX  XXXX XXXX "
))))

(deftest day09 ()
  (is (= 102239 (aoc2016.day09::part1 (input-for 2016 9))))
  (is (= 10780403063 (aoc2016.day09::part2 (input-for 2016 9)))))

(deftest day10 ()
  (is (= 73 (aoc2016.day10::part1 (input-for 2016 10))))
  (is (= 3965 (aoc2016.day10::part2 (input-for 2016 10)))))

(deftest day11 ()
  (is (= 31 (aoc2016.day11::part1 (input-for 2016 11))))
  (is (= 55 (aoc2016.day11::part2 (input-for 2016 11)))))

(deftest day12 ()
  (is (= 317993 (aoc2016.day12::part1 (input-for 2016 12))))
  (is (= 9227647 (aoc2016.day12::part2 (input-for 2016 12)))))

(deftest day13 ()
  (is (= 86 (aoc2016.day13::part1 (input-for 2016 13))))
  (is (= 127 (aoc2016.day13::part2 (input-for 2016 13)))))

(deftest day14 ()
  (is (= 15168 (aoc2016.day14::part1 (input-for 2016 14))))
  (is (= 20864 (aoc2016.day14::part2 (input-for 2016 14)))))

(deftest day15 ()
  (is (= 203660 (aoc2016.day15::part1 (input-for 2016 15))))
  (is (= 2408135 (aoc2016.day15::part2 (input-for 2016 15)))))

(deftest day16 ()
  (is (string= "10111110010110110" (aoc2016.day16::part1 (input-for 2016 16))))
  (is (string= "01101100001100100" (aoc2016.day16::part2 (input-for 2016 16)))))

(deftest day17 ()
  (is (string= "DUDRDLRRRD" (aoc2016.day17::part1 (input-for 2016 17))))
  (is (= 502 (aoc2016.day17::part2 (input-for 2016 17)))))

(deftest day18 ()
  (is (= 1956 (aoc2016.day18::part1 (input-for 2016 18))))
  (is (= 19995121 (aoc2016.day18::part2 (input-for 2016 18)))))

(deftest day19 ()
  (is (= 1842613 (aoc2016.day19::part1 (input-for 2016 19))))
  (is (= 1424135 (aoc2016.day19::part2 (input-for 2016 19)))))

(deftest day20 ()
  (is (= 32259706 (aoc2016.day20::part1 (input-for 2016 20))))
  (is (= 113 (aoc2016.day20::part2 (input-for 2016 20)))))
