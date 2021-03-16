(require 
  '[clojure.string :refer [split join]])

(defn collatz-odd [n]
  (-> (* n 3) (+ 1) (/ 2)))

(defn make-list 
  ([s] (make-list s nil))
  ([s initial] (into [] (repeat s initial))))

(defn collatz-steps' [mx]
  (let [cache (transient (make-list mx))]
  (fn inner [n]
     (loop [a 0
            s n]
        (let [v (when (< s mx) (nth cache s))]
        (cond (some? v) (do (when (< n mx) (assoc! cache n (+ a v)))
                            (+ a v))
              (= s 1)   (do (when (< n mx) (assoc! cache n a))
                            a)
              (even? s) (recur (inc a) (/ s 2))
              :else     (recur (+ 2 a) (collatz-odd s))))))))

(defn select-indices [is coll]
  (map (partial nth coll) is))

(defn max-steps-starts [steps]
  (let [pred (fn [[c ms cltz] s]
                (cond (>= s ms) [(inc c) s  (conj cltz c)]
                      :else     [(inc c) ms cltz]))]
  (last (reduce pred [1 0 []] steps))))

(defn max-start [starts n]
  (last (take-while #(<= % n) starts)))

(defn steps [nz]
  (let [t (inc (apply max nz))
        rng (range 1 t)
        collatz-steps (collatz-steps' t)]
  (map collatz-steps rng)))

;; limiting input to pass all the test,
;; which otherwise fail coz of timeouts
(defn solve [T & nz]
  (let [is (filter #(<= % 4000000) nz)
        starts (max-steps-starts (steps is))]
  (map (partial max-start starts) nz)))

(defn interact [func]
  (let [joinIfSeq #(if (seq? %) (join "\n" %) %)]
  (->> (func (slurp *in*))
       (joinIfSeq)
       (println))))

(defn safeInt [n]
  (try (Integer/parseInt n)
  (catch Exception e n)))

(defn prepare [input]
  (let [words  #(split % #"\s")]
  (->> (words input)
       (map safeInt))))

(defn main []
  (let [program #(->> (prepare %) (apply solve))]
  (interact program)))

(main)