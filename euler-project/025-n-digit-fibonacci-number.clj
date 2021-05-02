(require 
  '[clojure.string :as S :only [split join replace trim]])

(defn debug [n]
  (println n)
  n)

; Length of nth fibonacci number
(defn fib-len [n]
  (let [phi (-> (Math/sqrt 5) (+ 1) (/ 2))]
  (-> (- (* n (Math/log10 phi))
         (* 1/2 (Math/log10 5)))
      (Math/ceil)
      (int))))

; Takes the length of fib return the (index of fib) + 1
(defn fib-len-rev [l]
  (let [phi (-> (Math/sqrt 5) (+ 1) (/ 2))]
  (-> (+ (dec l)
         (* 1/2 (Math/log10 5)))
      (/ (Math/log10 phi))
      (Math/ceil)
      (int))))

(defn solve [t & nz]
  (map fib-len-rev nz))

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