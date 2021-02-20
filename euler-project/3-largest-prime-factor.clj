(require 
  '[clojure.string :refer [split join]])

(defn prime-factor [[lf i n]]
  (let [r  (mod n i)
        ni (if (= i 2) (+ i 1) (+ i 2))
        t  (int (Math/sqrt n))
        nn (quot n i)]
  (cond (= 1 n)   [lf 0   0]
        (zero? r) [i  2  nn]
        (> i t)   [n  ni  1]
        :else     [lf ni  n])))

(defn max-prime-factor [n]
  (->> (iterate prime-factor [1 2 n])
       (take-while #(not= (last %) 0))
       (map first)
       (apply max)))

(defn solve [t & nz]
  (map max-prime-factor nz))

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