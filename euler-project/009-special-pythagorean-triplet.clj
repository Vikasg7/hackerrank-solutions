(require 
  '[clojure.string :refer [split join]])

(defn sqr [n]
  (* n n))

; Generates pythagorean triples (=n) in reverse order
(defn pytha-triples [n]
  (let [n3 (quot n 3)]
  (for [a (range n3 2 -1)
        :let [b (-> (sqr a)
                    (- (sqr (- a n)))
                    (/ (* 2 (- a n))))
              c (- n a b)]
        :when (< a b c)
        :when (integer? b)]
    [a b c])))

(defn max-pytha-triple [N]
  (let [toResult #(if (nil? %) -1 (apply * %))]
  (->> (first (pytha-triples N))
       (toResult))))

(defn solve [T & Nz]
  (map max-pytha-triple Nz))

(defn prepare [input]
  (let [words  #(split % #"\s")
        safeInt #(try (Integer/parseInt %)
                      (catch Exception e %))]
  (->> (words input)
       (map safeInt))))

(defn interact [func]
  (let [joinIfSeq #(if (seq? %) (join "\n" %) %)]
  (->> (func (slurp *in*))
       (joinIfSeq)
       (println))))

(defn main []
  (let [program #(->> (prepare %) (apply solve))]
  (interact program)))

(main)