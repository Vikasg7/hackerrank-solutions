(require 
  '[clojure.string :as S :only [split join replace trim]])

(defn triangles 
  ([nz]
    (triangles [] 1 (first nz) (rest nz)))
  ([acc i n nz]
      (let [[cnz nnz] (split-at i nz)
            nacc      (conj acc cnz)]
      (cond (empty? nnz) [nacc]
            (= i n)      (lazy-seq (cons nacc (triangles nnz)))
            :else        (recur nacc (inc i) n nnz)))))

(defn reduce-right
  ([f coll]   (reduce f (reverse coll)))
  ([f s coll] (reduce f s (reverse coll))))

(defn max-path-sum [tri]
  (let [max-sum (fn [a b c] (+ a (max b c)))
        fold-by (fn [bs as] (map max-sum as bs (rest bs)))]
  (first (reduce-right fold-by tri))))

(defn solve [T & nz]
  (->> (triangles nz)
       (map max-path-sum)))

(defn safeInt [n]
  (try (Long/parseLong n)
  (catch Exception e n)))

(defn interact [func]
  (let [joinIfSeq #(if (seq? %) (S/join "\n" %) %)]
  (->> (func (slurp *in*))
       (joinIfSeq)
       (println))))

(defn prepare [input]
  (let [words  #(S/split % #"\s")]
  (->> (words input)
       (map safeInt))))

(defn main []
  (let [program #(->> (prepare %) (apply solve))]
  (interact program)))

(main)