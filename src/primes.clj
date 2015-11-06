(defn divide? [n q]
  (= (mod n q) 0))

(defn isPrimeX? [n]
  (or
    (= n 2)
    (and
      (not= (mod n 2) 0)
      (not-any? #(= (mod n %) 0) (take-while #(<= (* % %) n) (iterate #(+ % 2) 3)))
      )))

(defn isPrime? [n]
  (not-any?
    #(divide? n %)
    (range 2 (- n 1))))

(defn isPrime2? [n]
  (not-any?
    #(divide? n %)
    (range 3 (Math/sqrt n))))

(defn isPrime2b? [n]
  (not-any?
    #(divide? n %)
    (range 3 (+ 1 (Math/sqrt n)) 2)))

(defn isPrime2c? [n primes]
  (not-any?
    #(divide? n %)
    primes))

(defn isPrime3? [n]
  (not-any?
    #(divide? n %)
    (for
      [q (range 2 n) :when(<= (* q q) n)]
      q)))

(defn isPrime4? [n]
  (not-any?
    #(divide? n %)
    (take-while
      #(<= (* % %) n)
      (range 2 n))))

(defn isPrime5? [n]
  (not-any?
    #(divide? n %)
    (take-while
      #(<= (* % %) n)
      (range 3 n 2))))

(defn isPrime6? [n primes]
  (not-any?
    #(divide? n %)
    (take-while
      #(<= (* % %) n)
      primes)))

(defn printPrimes []

  (println 2)
  ;(doseq [n (filter odd? (range 2 1000000))]
  (doseq [n (range 3 1000000 2)]
    (if (isPrime2b? n)
      (println n)))
  )

(defn printPrimes2 []

  (def foundPrimes [2])
  (println 2)
  (doseq [n (range 3 1000000 2)]
    (if (isPrime6? n foundPrimes)
      (
        do (def foundPrimes (conj foundPrimes n))
           (println n))))
  )

(defn printPrimes3 []

  (println 2)
  (let [foundPrimes (atom [])]
    (doseq [n (range 3 1000000 2)]
      (if (isPrime6? n @foundPrimes)
        (
          do (swap! foundPrimes conj n)
             (println n))))
    )
  )

;(defn printPrimes3b []
;
;  (println 2)
;  (loop [foundPrimes []]
;    (doseq [n (range 3 1000000 2)]
;      (if (isPrime6? n @foundPrimes)
;        (do
;          (println n)
;          (recur (conj foundPrimes n)))))
;    )
;  )


(defn power-of-twos []
  (iterate (partial * 2) 1))

(defn findPowerOf2GreaterThanN [n]
  (first (for [p (power-of-twos) :when (> (bit-shift-left 1 p) n)] p))
  )


(defn findNewPrimes[previousPrimes previousMax max realmax]
  ;(println "findNewPrimes" previousPrimes previousMax max)
  (for [n (range (+ 1 previousMax) (min max realmax) 2) :when (isPrime6? n previousPrimes)]
    n)
  )

(defn findPrimesImpl [max realmax]

  (if (= max 1)
    [2]
    (let [
          previousMax (/ max 2)
          previousPrimes (findPrimesImpl previousMax realmax)]
      ;(println max)
      (concat previousPrimes (findNewPrimes previousPrimes (bit-shift-left 1 previousMax) (bit-shift-left 1 max) realmax))
      )
    )
  )

(defn findPrimes [max]

  (findPrimesImpl (findPowerOf2GreaterThanN max) max)
  )

(defn printPrimes4 []
  (println (clojure.string/join "\n" (findPrimes 1000000)))
  )

; a bit slower, not sure why.
(defn printPrimes4b []
  (->> (findPrimes 1000000)
       (clojure.string/join "\n")
       (println))
  )

;(defn f []
;  ; define x inside of f.
;  (let [x "hello"]))
;
;; outside of f, call f and print x.
;(f)
;(let [x "hello"]
;  (println x)) ; this print "hello"
;(println x)

(println (take 10 (power-of-twos)))
(println (findPowerOf2GreaterThanN 10000000))

(time (dotimes [i 10] (printPrimes4)))
