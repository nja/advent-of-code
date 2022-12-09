;;;; tests.lisp

(in-package #:aoc2018.tests)

(defanswer day01 592 241)
(defanswer day02 6448 "evsialkqyiurohzpwucngttmf")
(defanswer day03 110546 819)
(defanswer day04 103720 110913)
(defanswer day05 11042 6872)
(defanswer day06 4011 46054)
(defanswer day07 "BKCJMSDVGHQRXFYZOAULPIEWTN" 1040)
(defanswer day08 42768 34348)
(defanswer day09 380705 3171801582)

(deftest day10 ()
  (multiple-value-bind (message seconds)
      (aoc2018.day10::part1&2 (input-for 2018 10))
    (is (string= message (strip-cr "
#.......#####....####...#####...#####...#....#..######..######
#.......#....#..#....#..#....#..#....#..#....#..#............#
#.......#....#..#.......#....#..#....#..#....#..#............#
#.......#....#..#.......#....#..#....#..#....#..#...........#.
#.......#####...#.......#####...#####...######..#####......#..
#.......#..#....#..###..#.......#....#..#....#..#.........#...
#.......#...#...#....#..#.......#....#..#....#..#........#....
#.......#...#...#....#..#.......#....#..#....#..#.......#.....
#.......#....#..#...##..#.......#....#..#....#..#.......#.....
######..#....#...###.#..#.......#####...#....#..######..######")))
    (is (= seconds 10011))))

(deftest day11 ()
  (destructuring-bind (x y square level) (aoc2018.day11::part1)
    (declare (ignore level))
    (is (= x 19))
    (is (= y 17))
    (is (= 3 square)))
  (destructuring-bind (x y square level) (aoc2018.day11::part2)
    (declare (ignore level))
    (is (= x 233))
    (is (= y 288))
    (is (= square 12))))

(defanswer day12 2736 3150000000905)
(defanswer day13 '(10 116) '(25 116))
(defanswer day14 5115114101 20310465)
(defanswer day15 216270 59339)
(defanswer day16 590 475)
(defanswer day17 36787 29662)
(defanswer day18 427961 103970)
(defanswer day19 912 10576224)
(defanswer day20 4184 8595)
(defanswer day21 5745418 5090905)
(defanswer day22 10115 990)
(defanswer day23 640 113066145)
(defanswer day24 38008 4009)
(defanswer day25 428)
