(ns advent-of-code-clj.matrix)

(defn transpose [xs-of-xses]
  (apply mapv vector xs-of-xses))

(defn coord-map-fixed
  {:malli/schema [:-> [:sequential [:sequential :any]] [:map-of [:tuple :int :int] :any]]}
  [xs-of-xses]
  (->> xs-of-xses
       (map-indexed (fn [idy xs]
                      (map-indexed (fn [idx v]
                                     [[idy idx] v])
                                   xs)))
       (transduce cat merge)))

(defn text->matrix
  {:malli/schema [:-> :string [:vector [:vector char?]]]}
  [text]
  (mapv vec (.split text "\n")))
