(require 
  '[clojure.string :as S :only [split join replace trim]])

(defn debug [n]
  (println n)
  n)

(defn index-of [x coll]
  (let [idx? (fn [i a] (when (= x a) i))]
  (first (keep-indexed idx? coll))))

(defn parse-input [[n & ls]]
  (let [[f r] (split-at n ls)]
  (cond (empty? r) [f]
        :else      (lazy-seq (cons f (parse-input r))))))

(defn alpha-num [c]
  (- (int c) 64))

(defn score [names q]
  (let [i  (index-of q names)
        av (->> (map alpha-num q) (reduce +))]
  (* av (inc i))))

(defn solve [& nz]
  (let [[nms qs] (parse-input nz)
        sorted   (sort nms)]
  (map (partial score sorted) qs)))

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