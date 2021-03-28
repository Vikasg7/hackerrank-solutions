(require 
  '[clojure.string :as S :only [split join replace trim]])

(defn parse-one-tri [n cnz]
  (let [inner (fn [acc i nz]
                  (let [[cnz nnz] (split-at i nz)
                        nacc (conj acc cnz)]
                  (cond (= i n) [nacc nnz]
                        :else   (recur nacc (inc i) nnz))))]
  (inner [] 1 cnz)))

(defn parse-all-tris [nz]
  (let [inner (fn [acc [n & nz]]
                 (let [[tri nnz] (parse-one-tri n nz)
                       nacc (conj acc tri)]
                 (cond (empty? nnz) nacc
                       :else        (recur nacc nnz))))]
  (inner [] nz)))

(defn reduce-right
  ([f coll]   (reduce f (reverse coll)))
  ([f s coll] (reduce f s (reverse coll))))

(defn max-path-sum [tri]
  (let [max-sum (fn [a b c] (+ a (max b c)))
        fold-by (fn [bs as] (map max-sum as bs (rest bs)))]
  (first (reduce-right fold-by tri))))

(defn solve [T & nz]
  (->> (parse-all-tris nz)
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