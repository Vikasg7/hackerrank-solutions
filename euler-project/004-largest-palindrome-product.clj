(require 
  '[clojure.string :refer [split join]]
  '[clojure.string :as s])

(defn palindrome? [n]
  (let [s (str n)]
  (= s (s/reverse s))))

(def threeDs? #(= 3 (count (str %))))

(defn product-of-3? [n]
  (let [y (range 110 991 11) ; 3 digit multpile of 11
        pred (fn [x] 
                (let [q (/ n x)]
                (and (integer? q) (threeDs? q))))]
  (some pred y)))

(defn max-palindrome [N]
  (let [l (* 11 (quot (dec N) 11))  ; First number divisible by 11 before N
        x (range l 10100 -11)] ; 6 digit multiple of 11  
  (->> (filter palindrome? x)
       (filter product-of-3?)
       (first))))

(defn solve [T & nz]
  (map max-palindrome nz))

(defn interact [func]
  (let [joinIfSeq #(if (seq? %) (join "\n" %) %)]
  (->> (func (slurp *in*))
       (joinIfSeq)
       (println))))

(defn prepare [input]
  (let [words  #(split % #"\s")]
  (->> (words input)
       (map read-string))))

(defn main []
  (let [program #(->> (prepare %) (apply solve))]
  (interact program)))

(main)