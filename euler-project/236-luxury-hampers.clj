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
        
        ;fbi/fai = m(bi/ai)
        fbi-by-fai (fn [m] (fn [a b]
                       (* m (/ b a))))

        mult-combos (fn [s]
                       (let [rng #(range 1 (inc %))]
                       (apply cartesian (map rng s))))

        validMultCombo? (fn [m fais fbis] (fn [mc]
                           (let [pat (->> (map * fais mc)
                                          (apply +))
                                 pbt (->> (map * fbis mc)
                                          (apply +))]
                            (= m (calcM [pat pbt])))))

        validProRate? (fn [m]
                         (let [fbi-by-fais (map (fbi-by-fai m) as bs)
                               fbis (map safeNumer fbi-by-fais)
                               fais (map safeDenom fbi-by-fais)
                               ; Possible multiples
                               pma (map quot as fais)
                               pmb (map quot bs fbis)
                               ; Minimum multiple per product for both A and B
                               min-mult (map min pma pmb)]
                          (and (every? pos? min-mult)
                               (let [; Multiple Combinations of n product for A and B
                                     mcs (mult-combos min-mult)]
                               (some (validMultCombo? m fais fbis) mcs)))))

        valid? (fn [m]
                  (and (> m 1)
                       (validProRate? m)))]

  (->> (map calcM fatfbt)
       (distinct)
       (filter valid?)
       (take 1))))

(defn interact [func]
  (let [joinIfSeq #(if (seq? %) (join "\n" %) %)]
  (->> (func (slurp *in*))
       (joinIfSeq)
       (println))))

(defn prepare [input]
  (let [words  #(split % #"\s")
        safeInt #(try (Integer/parseInt %)
                      (catch Exception e %))]
  (->> (words input)
       (map safeInt))))

(defn main []
  (let [program #(->> (prepare %) (apply solve))]
  (interact program)))

(main)