^{:nextjournal.clerk/visibility {:code :hide}}
(ns y2024.d13
  (:require
   [clojure.core.match :as m]
   [numeric.expresso.core :as e]
   [instaparse.core :as insta]
   [advent-of-code-clj.input :as input]))

; 2024, dag 13

; ## Del 1

; I dag ser det ut til at man kan bruke matematikk for å
; finne svaret på hvor mange ganger man må trykke på en av
; to knapper for å få tak i belønningen.

; Klassisk formel med to ukjente, men å faktisk løse den er jo en
; annen sak! Heldigvis har vi et bibliotek for dette.

; Starter med en liten overkomplisert parsing av dagens input:

(def test-data "Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279")

; Instaparse lar oss definere en parser for problemet:

(def game-parser
  (insta/parser "game = button <ws> button <ws> prize
                 button = <'Button '> type <': X+'> num <', Y+'> num
                 prize = <'Prize: X='> num <', Y='> num
                 type = 'A'|'B'
                 ws = #'[\\s\\n]*'
                 num = #'\\d+'"))

(def first-game-parsed
  (insta/parse game-parser (first (.split test-data "\n\n"))))

; Og core.match lar oss pattern-matche på parset output for å
; sette sammen dataene slik vi vil:

^{:nextjournal.clerk/visibility {:result :hide}}
(defn game-map [game]
  (m/match game
    [:game button-a button-b prize] {:a (game-map button-a)
                                     :b (game-map button-b)
                                     :prize (game-map prize)}
    [:button [:type _] [:num x] [:num y]] {:x (parse-long x)
                                           :y (parse-long y)}
    [:prize [:num x] [:num y]] {:x (parse-long x) :y (parse-long y)}))

(def first-game (game-map first-game-parsed))

^{:nextjournal.clerk/visibility {:result :hide}}
(defn get-games [input]
  (map #(game-map (insta/parse game-parser %))
       (.. input trim (split "\n\n"))))

(get-games test-data)

; For å løse spillet bruker vi expresso for å sette opp to
; formler (løse for x-aksen og y-aksen):

^{:nextjournal.clerk/visibility {:result :hide}}
(defn solve-game [{px :x py :y} {ax :x ay :y} {bx :x by :y}]
  (first (e/solve ['a 'b]
                  (e/ex (= ~px (+ (* ~ax a) (* ~bx b))))
                  (e/ex (= ~py (+ (* ~ay a) (* ~by b)))))))

(let [{:keys [a b prize]} first-game]
  (solve-game prize a b))
;;=> {a 80, b 40}

; Med løsningsfunksjonen i hånd kan vi lage reduceren som løser
; del 1. I del 1 er det ikke lov å trykke mer enn 100 ganger
; på hver knapp, og det er ikke lov med "halve trykk", så vi
; filtrerer vekk løsninger som gir ratio (feks `1/3`):

^{:nextjournal.clerk/visibility {:result :hide}}
(defn part-1 [acc {:keys [prize a b]}]
  (let [{a 'a b 'b} (solve-game prize a b)]
    (if (or (< 100 a) (< 100 b)
            (ratio? a) (ratio? b))
      acc
      (+ acc (* a 3) b))))

^{:nextjournal.clerk/visibility {:result :hide}}
(defn solve [solver input]
  (reduce solver 0
          (get-games input)))

; Nå kan vi løse del 1 for test-dataene:

(solve part-1 test-data)

; Og for reelle data:

(solve part-1 (input/get-input 2024 13))

; # Del 2

; I del 2 er belønningen i en posisjon som er `10000000000000`
; lenger unna i hver akse!

; Nå er det også lov å trykke flere ganger på knappene, som er
; reflektert i den nye reduceren for del 2:

^{:nextjournal.clerk/visibility {:result :hide}}
(defn part-2 [acc {:keys [prize a b]}]
  (let [bigger-prize (-> prize
                         (update :x + 10000000000000)
                         (update :y + 10000000000000))
        {a 'a b 'b} (solve-game bigger-prize a b)]
    (if (or (ratio? a) (ratio? b))
      acc
      (+ acc (* a 3) b))))

; Ellers kan vi løse oppgaven på samme måte som i del 1 for test-input:

(solve part-2 test-data)

; Og faktiske input:

(solve part-2 (input/get-input 2024 13))