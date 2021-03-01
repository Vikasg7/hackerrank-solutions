(require 
  '[clojure.string :refer [split join]])

; source: clojure.math.combinatorics/cartesian-product
(defn cartesian [& seqs]
  (let [v-original-seqs (vec seqs)
        increment (fn [v-seqs]
                     (loop [i (dec (count v-seqs)), v-seqs v-seqs]
                        (if (= i -1) 
                           nil
                           (if-let [rst (next (v-seqs i))]
                              (assoc v-seqs i rst)
                              (recur (dec i) (assoc v-seqs i (v-original-seqs i)))))))
        step (fn step [v-seqs]
                (when v-seqs
                  (cons (map first v-seqs)
                        (lazy-seq (step (increment v-seqs))))))]
  (when (every? seq seqs)
    (lazy-seq (step v-original-seqs)))))

(defn solve [n & abs]
  (let [[as bs] (partition n abs)
        at (apply + as)
        bt (apply + bs)

        safeNumer #(if (integer? %) % (numerator %))
        safeDenom #(if (integer? %) 1 (denominator %))

        ;Possible combinations of fat and fbt
        fatfbt  (cartesian (range n (inc at))
                           (range n (inc bt)))
        
        calcM (fn [[fat fbt]]
                 (/ (/ fat at) (/ fbt bt)))
        
        factor (fn [m] (fn [a b]
                  (* m (/ b a))))

        mult-combos (fn [s]
                       (let [rng #(range 1 (inc %))]
                       (apply cartesian (map rng s))))

        validMultCombo? (fn [m factors] (fn [[pma pmb]]
                           (let [pat (->> (map safeDenom factors)
                                          (map * pma)
                                          (apply +))
                                 pbt (->> (map safeNumer factors)
                                          (map * pmb)
                                          (apply +))]
                            (= m (calcM [pat pbt])))))

        validProRate? (fn [m]
                         (let [factors (map (factor m) as bs)
                               ;Possible multiples
                               pma (map #(quot %1 (safeDenom %2)) as factors)
                               pmb (map #(quot %1 (safeNumer %2)) bs factors)
                               ;Multiple Combinations of n product for A and B
                               mcs (cartesian (mult-combos pmb) (mult-combos pma))]
                         (some (validMultCombo? m factors) mcs)))

        valid? (fn [m]
                  (and (> m 1)
                       (validProRate? m)))]

  (->> (map calcM fatfbt)
       (distinct)
       (sort) ;Helped passing all tests in time
       (filter valid?)
       (take 1))))

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