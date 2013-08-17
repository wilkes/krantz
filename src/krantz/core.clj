(ns krantz.core)

(def chromatic [:b2 :2 :b3 :3 :4 :b5 :5 :b6 :6 :b7 :7])

(def chromatic-map [{:b2 1}
                    {:2  2}
                    {:b3 3} 
                    {:3  4} 
                    {:4  5} 
                    {:b5 6} 
                    {:5  7} 
                    {:b6 8} 
                    {:6  9} 
                    {:b7 10} 
                    {:7  11}])

(def notes [:C :Db :D :Eb :E :F :Gb :G :Ab :A :Bb :B])

(def c-scale [:C :D :E :F :G :A :B])

(def natural-modes [:Ionian :Dorian :Phrygian :Lydian :Mixolydian :Aoelian :Locrian])

(def strings [:E :A :D :G :B :e])

(def string-pairs  [[:E :A]
                    [:A :D]
                    [:D :G]
                    [:G :B]
                    [:B :e]])

(def chords [:maj7 :7 :9 :13 :11 :#11 :sus4 :sus2 :7b5])

(def triads (flatten [(repeat 100 :maj) (repeat 50 :min)  (repeat 10 :dim) (repeat 5 :aug)]))

(def fingerings
  [1234
   1243
   1324
   1342
   1423
   1432

   2134
   2143
   2314
   2341
   2413
   2431

   3124
   3142
   3214
   3241
   3412
   3421

   4123
   4132
   4213
   4231
   4312
   4321])

(defn rand-n-of [n coll]
  (loop [result [] remaining coll]
    (if (= (count result) n)
      result
      (let [remaining (if (empty? remaining) coll remaining)
            n (rand-nth (shuffle remaining))
            remaining (remove #{n} remaining)]
        (recur (conj result n) remaining)))))

(defn rand-zone []
  (let [start (inc (rand-int 12))]
    (vec (range start (+ 4 start)))))

(defn rand-key []
  (nth notes (rand-int (count notes))))

(defn rand-formula [size]
  (->> chromatic-map
       (rand-n-of size)
       (concat [{:1 0}])
       (apply merge)
       (sort-by val < )
       (mapv first)))

(defn rand-tempo []
  (rand-nth (range 50 (inc 120))))

(defn rand-n-tones []
  (inc (rand-int 12)))

(defn improv
  ([] (improv (rand-n-tones)))
  ([size]
      {:key (rand-key)
       :formula (rand-formula size)
       :size size
       :zone (rand-zone)
       :tempo (rand-tempo)}))

(defn vamp []
  {:string (rand-nth strings)
   :mode (rand-nth natural-modes)
   :tempo (rand-tempo)})

(defn vamp-2-strings []
  {:strings (rand-nth string-pairs)
   :mode (rand-nth natural-modes)
   :tempo (rand-tempo)})

(defn one-string-formula []
  (let [size (rand-n-tones)]
    {:string (rand-nth strings)
     :formula (rand-formula (rand-n-tones))
     :size size
     :key (rand-key)
     :tempo (rand-tempo)}))

(defn two-string-formula []
  (let [size (rand-n-tones)]
    {:strings (rand-nth string-pairs)
     :formula (rand-formula size)
     :size size
     :key (rand-key)
     :tempo (rand-tempo)}))

(defn rand-fingering []
  (rand-n-of 4 [1 2 3 4]))

(defn rand-fingerings []
  (let [r (rand-n-of 6 fingerings)]
    {:count (count r)
     :fingerings r}))

(defn rand-triads
  ([] (rand-triads (max 2 (rand-int 8))))
  ([n]
     (mapv vector
          (rand-n-of n notes)
          (rand-n-of n triads))))
