;;;; tests.lisp

(in-package #:aoc2021.tests)

(deftest day01 ()
  (is (= 1374 (aoc2021.day01::part1 (input-for 2021 1))))
  (is (= 1418 (aoc2021.day01::part2 (input-for 2021 1)))))

(deftest day02 ()
  (is (= 1882980 (aoc2021.day02::part1 (input-for 2021 2))))
  (is (= 1971232560 (aoc2021.day02::part2 (input-for 2021 2)))))

(deftest day03 ()
  (is (= 3923414 (aoc2021.day03::part1 (input-for 2021 3))))
  (is (= 5852595 (aoc2021.day03::part2 (input-for 2021 3)))))

(deftest day04 ()
  (is (= 25410 (aoc2021.day04::part1 (input-for 2021 4))))
  (is (= 2730 (aoc2021.day04::part2 (input-for 2021 4)))))

(deftest day05 ()
  (is (= 6710 (aoc2021.day05::part1 (input-for 2021 5))))
  (is (= 20121 (aoc2021.day05::part2 (input-for 2021 5)))))

(deftest day06 ()
  (is (= 346063 (aoc2021.day06::part1 (input-for 2021 6))))
  (is (= 1572358335990 (aoc2021.day06::part2 (input-for 2021 6)))))

(deftest day07 ()
  (is (= 340056 (aoc2021.day07::part1 (input-for 2021 7))))
  (is (= 96592275 (aoc2021.day07::part2 (input-for 2021 7)))))

(deftest day08 ()
  (is (= 476 (aoc2021.day08::part1 (input-for 2021 8))))
  (is (= 1011823 (aoc2021.day08::part2 (input-for 2021 8)))))

(deftest day09 ()
  (is (= 478 (aoc2021.day09::part1 (input-for 2021 9))))
  (is (= 1327014 (aoc2021.day09::part2 (input-for 2021 9)))))

(deftest day10 ()
  (is (= 311949 (aoc2021.day10::part1 (input-for 2021 10))))
  (is (= 3042730309 (aoc2021.day10::part2 (input-for 2021 10)))))

(deftest day11 ()
  (is (= 1644 (aoc2021.day11::part1 (input-for 2021 11))))
  (is (= 229 (aoc2021.day11::part2 (input-for 2021 11)))))

(deftest day12 ()
  (is (= 3410)) (aoc2021.day12::part1 (input-for 2021 12))
  (is (= 98796 (aoc2021.day12::part2 (input-for 2021 12)))))

(deftest day13 ()
  (is (= 716 (aoc2021.day13::part1 (input-for 2021 13))))
  (is (string= "
###  ###   ##  #  # #### ###  #    ### 
#  # #  # #  # # #  #    #  # #    #  #
#  # #  # #    ##   ###  ###  #    #  #
###  ###  #    # #  #    #  # #    ### 
# #  #    #  # # #  #    #  # #    # # 
#  # #     ##  #  # #    ###  #### #  #
"
               (aoc2021.day13::part2 (input-for 2021 13)))))

(deftest day14 ()
  (is (= 2967 (aoc2021.day14::part1 (input-for 2021 14))))
  (is (= 3692219987038 (aoc2021.day14::part2 (input-for 2021 14)))))

(deftest day15 ()
  (is (= 592 (aoc2021.day15::part1 (input-for 2021 15))))
  (is (= 2897 (aoc2021.day15::part2 (input-for 2021 15)))))
