(ns y2022.d07-test
  (:require [advent-of-code-clj.y2022.d07 :refer [part-1 part-2]]
            [clojure.test :refer [deftest is]]))

(def test-data "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k")

(deftest part-1-test
  (is (= 95437 (part-1 test-data))))

(deftest part-2-test
  (is (= 24933642 (part-2 test-data))))