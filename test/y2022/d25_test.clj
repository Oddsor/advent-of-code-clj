(ns y2022.d25-test
  (:require [clojure.test :refer [deftest is]]
            [y2022.d25 :refer [part-1 snaf unsnaf]]))

(def test-data "1=-0-2
12111
2=0=
21
2=01
111
20012
112
1=-1=
1-12
12
1=
122")

(deftest unsnaf-test
  (is (= 1 (unsnaf "1")))
  (is (= 2 (unsnaf "2")))
  (is (= 3 (unsnaf "1=")))
  (is (= 4 (unsnaf "1-")))
  (is (= 5 (unsnaf "10")))
  (is (= 6 (unsnaf "11")))
  (is (= 7 (unsnaf "12")))
  (is (= 8 (unsnaf "2=")))
  (is (= 9 (unsnaf "2-")))
  (is (= 10 (unsnaf "20")))
  (is (= 2022 (unsnaf "1=11-2")))
  (is (= 314159265 (unsnaf "1121-1110-1=0"))))
(deftest snaf-test
  (is (= "1" (snaf 1)))
  (is (= "2" (snaf 2)))
  (is (= "1=" (snaf 3)))
  (is (= "10" (snaf 5)))
  (is (= "1=11-2" (snaf 2022)))
  (is (= "1121-1110-1=0" (snaf 314159265))))

(deftest part-1-test
  (is (= "2=-1=0" (part-1 test-data))))