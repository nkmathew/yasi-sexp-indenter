;; Test 17
(letfn [(six-times [y]
          (* (twice y) 3))
        (twice [x]
          (* x 2))]
  (println "Twice 15 =" (twice 15))
  (println "Six times 15 =" (six-times 15)))

;; A contrived example of mutual recursion
(defn even2? [n]
  (letfn [(neven? [n]
            (if (zero? n) true (nodd? (dec n))))
          (nodd? [n]
            (if (zero? n) false (neven? (dec n))))]
    (neven? n)))

(defn primes [n]
  (letfn [(sieve [table removal]
            (assoc table removal false))
          (primebools [i table]
            (cond
             (= i n) table
             (table i) (recur (inc i)
                              (reduce sieve table
                                      (range (* i i) n i)))
             :else (recur (inc i) table)))]
    (let [prime? (primebools 2 (apply vector (repeat n true)))]
      (filter prime? (range 2 n)))))
