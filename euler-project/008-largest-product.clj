(require 
  '[clojure.string :refer [split join]])

(defn digits [n]
  (let [toInt #(->> (str %) (read-string))]
  (map toInt (str n))))

(defn consecutive-pairs 
  ([n xs]
    (consecutive-pairs n (count xs) xs))
  ([n x xs]
    (let [pair #(->> (drop % xs) (take n))]
    (->> (range 0 (- x (dec n)))
         (map pair)))))

(defn largest-product [[N K n]]
  (->> (digits n)
       (consecutive-pairs K)
       (map #(apply * %))
       (apply max)))

(defn solve [T & ar]
  (let [arr (partition 3 ar)]
  (map largest-product arr)))

(defn interact [func]
  (let [joinIfSeq #(if (seq? %) (join "\n" %) %)]
  (->> (func (slurp *in*))
       (joinIfSeq)
       (println))))

(defn prepare [input]
  (let [words  #(split % #"\s")
        safeInt #(try (Integer/parseInt %)
                      (catch Exception e %))]
  (->> (words input)
       (map safeInt))))

(defn main []
  (let [program #(->> (prepare %) (apply solve))]
  (interact program)))

(main)