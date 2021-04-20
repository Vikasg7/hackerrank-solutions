(require 
  '[clojure.string :as S :only [split join replace trim]])

(defn debug [n]
  (println n)
  n)

(defn quot-mod [n d]
  [(quot n d) (mod n d)])

(defn list-to-str [ls]
  (apply str ls))

(defn remove-at [i coll]
  (let [head (take i coll)
        body (drop (inc i) coll)]
  (concat head body)))

(def facs (reductions * (cons 1 (range 1 13))))

(defn lexico
  ([n]
    (lexico "abcdefghijklm" n))
  ([coll n]
    (lexico coll n (count coll)))
  ([coll n s]
    (let [f (nth facs (dec s))
          [x y] (quot-mod n f)
          xth (nth coll x)
          res (remove-at x coll)]
    (cond (= s 1) [xth]
          :else   (lazy-seq (cons xth (lexico res y)))))))

(def lexico-permut
  (comp list-to-str lexico dec))

(defn solve [t & nz]
  (map lexico-permut nz))

(defn safeInt [n]
  (try (Long/parseLong n)
  (catch Exception e n)))

(defn interact [func]
  (let [joinIfSeq #(if (seq? %) (S/join "\n" %) %)]
  (->> (func (slurp *in*))
       (joinIfSeq)
       (println))))

(defn prepare [input]
  (let [words  #(S/split % #"\s+")]
  (->> (words input)
       (map safeInt))))

(defn main []
  (let [program #(->> (prepare %) (apply solve))]
  (interact program)))

(main)