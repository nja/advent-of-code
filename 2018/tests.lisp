;;;; tests.lisp

(in-package #:aoc2018.tests)

(deftest day01 ()
  (is (= 592 (aoc2018.day01::part1 (input-for 2018 1))))
  (is (= 241 (aoc2018.day01::part2 (input-for 2018 1)))))

(deftest day02 ()
  (is (= 6448 (aoc2018.day02::part1 (input-for 2018 2))))
  (is (string= "evsialkqyiurohzpwucngttmf" (aoc2018.day02::part2 (input-for 2018 2)))))

(deftest day03 ()
  (is (= 110546 (aoc2018.day03::part1 (input-for 2018 3))))
  (is (= 819 (aoc2018.day03::part2 (input-for 2018 3)))))

(deftest day04 ()
  (is (= 103720 (aoc2018.day04::part1 (input-for 2018 4))))
  (is (= 110913 (aoc2018.day04::part2 (input-for 2018 4)))))

(deftest day05 ()
  (is (= 11042 (aoc2018.day05::part1 (input-for 2018 5))))
  (is (= 6872 (aoc2018.day05::part2 (input-for 2018 5)))))

(deftest day06 ()
  (is (= 4011 (aoc2018.day06::part1 (input-for 2018 6))))
  (is (= 46054 (aoc2018.day06::part2 (input-for 2018 6)))))

(deftest day07 ()
  (is (string= "BKCJMSDVGHQRXFYZOAULPIEWTN" (aoc2018.day07::part1 (input-for 2018 07))))
  (is (= 1040 (aoc2018.day07::part2 (input-for 2018 07)))))

(deftest day08 ()
  (is (= 42768 (aoc2018.day08::part1 (input-for 2018 08))))
  (is (= 34348 (aoc2018.day08::part2 (input-for 2018 08)))))

(deftest day09 ()
  (is (= 380705 (aoc2018.day09::part1 (input-for 2018 09))))
  (is (= 3171801582 (aoc2018.day09::part2 (input-for 2018 09)))))

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

(deftest day12 ()
  (is (= 2736 (aoc2018.day12::part1 (input-for 2018 12))))
  (is (= 3150000000905 (aoc2018.day12::part2 (input-for 2018 12)))))

(deftest day13 ()
  (is (equal '(10 116) (aoc2018.day13::part1 (input-for 2018 13))))
  (is (equal '(25 116) (aoc2018.day13::part2 (input-for 2018 13)))))

(deftest day14 ()
  (is (= 5115114101 (aoc2018.day14::part1)))
  (is (= 20310465 (aoc2018.day14::part2))))

(deftest day15 ()
  (is (= 216270 (aoc2018.day15::part1 (input-for 2018 15))))
  (is (= 59339 (aoc2018.day15::part2 (input-for 2018 15)))))

(deftest day16 ()
  (is (= 590 (aoc2018.day16::part1 (input-for 2018 16))))
  (is (= 475 (aoc2018.day16::part2 (input-for 2018 16)))))

(deftest day17 ()
  (is (= 36787 (aoc2018.day17::part1 (input-for 2018 17))))
  (is (= 29662 (aoc2018.day17::part2 (input-for 2018 17)))))

(deftest day18 ()
  (is (= 427961 (aoc2018.day18::part1 (input-for 2018 18))))
  (is (= 103970 (aoc2018.day18::part2 (input-for 2018 18)))))

(deftest day19 ()
  (is (= 912 (aoc2018.day19::part1 (input-for 2018 19))))
  (is (= 10576224 (aoc2018.day19::part2 (input-for 2018 19)))))

(deftest day20 ()
  (is (= 4184 (aoc2018.day20::part1 (input-for 2018 20))))
  (is (= 8595 (aoc2018.day20::part2 (input-for 2018 20)))))

(deftest day21 ()
  (is (= 5745418 (aoc2018.day21::part1 (input-for 2018 21))))
  (is (= 5090905 (aoc2018.day21::part2 (input-for 2018 21)))))

(deftest day22 ()
  (is (= 10115 (aoc2018.day22::part1 (input-for 2018 22))))
  (is (= 990 (aoc2018.day22::part2 (input-for 2018 22)))))

(deftest day23 ()
  (is (= 640 (aoc2018.day23::part1 (input-for 2018 23))))
  (is (= 113066145 (aoc2018.day23::part2 (input-for 2018 23)))))
