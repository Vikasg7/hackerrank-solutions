(require 
  '[clojure.string :refer [split join]])

(defn sn [n]
  (-> (+ n 1)
      (* n)
      (/ 2)))

(defn sum-of [multiple n]
  (let [nm (-> (/ n multiple)
               (- 1)
               (Math/ceil)
               (int))]
  (* multiple (sn nm))))

(defn sum-of-multiples [n]
  (-> (+ (sum-of 3 n)
         (sum-of 5 n))
      (- (sum-of 15 n))))

(defn solve [t & nums]
  (map sum-of-multiples nums))

(defn interact [fn]
  (let [joinIfSeq #(if (seq? %) (join "\n" %) %)]
  (->> (fn (slurp *in*))
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