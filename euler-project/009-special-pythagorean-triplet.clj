(require 
  '[clojure.string :refer [split join]])

(defn sqr [n]
  (* n n))

;Generates pythagorean triples (=n) in reverse order
(defn pytha-triples
  ([n]
    (let [as (range (quot n 3) 2 -1)]
    (when (and (seq as) (even? n)) 
       (pytha-triples n as))))
  ([n [a & as]]
    (let [b (-> (sqr a)
                (- (sqr (- a n)))
                (/ (* 2 (- a n))))
          c (- n a b)
          valid? (and (every? integer? [a b c])
                      (< a b c))]
    (cond (empty? as) (when valid? (lazy-seq [[a b c]]))
          valid?      (lazy-seq (cons [a b c] (pytha-triples n as)))
          :else       (recur n as)))))

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