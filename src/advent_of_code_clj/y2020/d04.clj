(ns advent-of-code-clj.y2020.d04
  (:require [clojure.string :as str]))

(def test-data "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in")

(defn split-passports [data]
  (->> (str/split-lines data)
       (partition-by #{""})
       (remove #{'("")})
       (map #(str/join " " %))))

(defn parse-passport [passport]
  (let [facts (str/split passport #"\s")]
    (->> facts
         (map (fn [fact]
                (let [[k v] (str/split fact #"\:")]
                  [(keyword k) v])))
         (into {}))))

(defn parse-number [num-str]
  (try
    (Integer/parseInt num-str)
    (catch Exception e
      nil)))

(defn num-in-range [num-str min-range max-range]
  (if-let [num (parse-number num-str)]
    (< (dec min-range) num (inc max-range))
    false))

(defn valid-height [height-str]
  (let [[_ height-val-str unit] (first (re-seq #"(\d*)(cm|in)" height-str))
        height (parse-number height-val-str)]
    (if (some? height)
      (cond
        (#{"in"} unit) (< (dec 59) height (inc 76))
        (#{"cm"} unit) (< (dec 150) height (inc 193))
        :else false)
      false)))

(defn valid-passport-1? [passport]
  (let [required-keys [:byr :iyr :eyr :hgt :hcl :ecl :pid]]
    (= (count required-keys) (count (select-keys passport required-keys)))))

(defn valid-passport-2? [passport]
  (let [required-keys [:byr :iyr :eyr :hgt :hcl :ecl :pid]]
    (and (= (count required-keys) (count (select-keys passport required-keys)))
         ; BYR-sjekk
         (num-in-range (:byr passport) 1920 2002)
         ; IYR-sjekk
         (num-in-range (:iyr passport) 2010 2020)
         ; EYR-sjekk
         (num-in-range (:eyr passport) 2020 2030)
         ; HGT-sjekk OK
         (valid-height (:hgt passport))
         ; HCL-sjekk OK
         (re-seq #"^\#[a-f0-9]{6,6}$" (:hcl passport))
         ; ECL-sjekk
         (#{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} (:ecl passport))
         ; PID-sjekk
         (re-seq #"^[\d]{9,9}$" (:pid passport))
         )))

(def ugyldige-test-data "eyr:1972 cid:100
hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926

iyr:2019
hcl:#602927 eyr:1967 hgt:170cm
ecl:grn pid:012533040 byr:1946

hcl:dab227 iyr:2012
ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277

hgt:59cm ecl:zzz
eyr:2038 hcl:74454a iyr:2023
pid:3556412378 byr:2007")

(def gyldige-test-data "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
hcl:#623a2f

eyr:2029 ecl:blu cid:129 byr:1989
iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm

hcl:#888785
hgt:164cm byr:2001 iyr:2015 cid:88
pid:545766238 ecl:hzl
eyr:2022

iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719")

(comment
  (->> (slurp "input/y2020/04.txt")
       split-passports
       (map parse-passport)
       (filter valid-passport-2?)
       count))