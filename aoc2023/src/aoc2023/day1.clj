(ns aoc2023.day1
  [:require clojure.string]) 

;; NOTES: 
 ;; so far, I had a solution that only worked with digits in a numeric representation.
 ;; after looking at the input, there are digits that are literally spelled out, so we
 ;; need to account for those too. 
 ;; TODO: 
 ;; - get a list of [idx match] for spelled digits in each row
 ;; - combine those matches with the matches we get for [idx match] items from normal digits
 ;; - parse all those matches, sort by idx, take first and last. then proceed with the rest!
 ;; 



;; Day 1
(def spelled-int->int
  {"zero" 0 "one" 1 "two" 2 "three" 3 "four" 4 "five" 5 "six" 6 "seven" 7 "eight" 8 "nine" 9})


(defn parse-spelled-int
  "Find instances of spelled-int (ex: 'one') in `in-str` and replace them
   with the value specified by spelled-int->int."
  [in-str spelled-int]
  (let [chars (clojure.string/split in-str #"")
        spelled-word-len (count spelled-int)]
    (map-indexed (fn [idx _] (when (<= (+ idx spelled-word-len) (count in-str))
                               (when (= spelled-int (subs in-str idx (+ idx spelled-word-len)))
                                 [idx (spelled-int->int spelled-int)]))) chars)))

(defn remove-nil-rows
  "removes rows of nils from a collection, flattening out the collection
   at the end to just be a single seq of [idx match] vecs.
   Ex: ((nil nil) ([1] nil) ([2] nil)) => ([1] [2])"
  [coll]
  (apply concat
         (filter not-empty (map (fn [row] (filter some? row)) coll))))

(defn parse-spelled-row
  "returns a list of all spelled ints as [idx matching-int] vecs."
  [row]
  (map (fn [spelled-int] (parse-spelled-int row spelled-int)) (keys spelled-int->int)))


(defn parse-input-row-str
  "turn str into vec of chars then parse out 
   the ints and return them in a seq of [idx parsed-int]."
  [row]
  (let [chars (clojure.string/split row #"")]
    (filter some? (map-indexed (fn [idx char]
                                 (try
                                   [idx (Integer/parseInt char)]
                                   (catch Exception _))) chars))))

;; combine the spelled results with the pure digit results

(defn parse-spelled-and-digit-matches 
  "combines spelled matches and digit matches, sorting by idx. 
   this will allow us to correctly choose the 'first' and 'last' numbers.
   pops off idx from each [idx match] at the end so that we're
   left with a seq of ints, sorted by their occurrence in `row`."
  [row]
  (map second (sort-by first
                (into (parse-input-row-str row)
                      (remove-nil-rows (parse-spelled-row row))))))

;; read in input, and split into a vec 
;; of vecs of chars that have been run through
;; parse-spelled-and-digit-matches
(def input-pt-1 (->
            (slurp "resources/day1_input.txt")
            (clojure.string/split #"\n")
            (#(reduce (fn [init-val row] (conj init-val (parse-input-row-str row))) [] %))))


(def input-pt-2 (->
            (slurp "resources/day1_input.txt")
            (clojure.string/split #"\n")
            (#(reduce (fn [init-val row] (conj init-val (parse-spelled-and-digit-matches row))) [] %))))


(defn first-and-last
  "gets the first and last item of coll and puts them in a vec together"
  [coll]
  (filter some? [(first coll) (last (rest coll))]))

(defn coll-of-ints-to-base-10
  "turns a coll of up to 2 ints into a base 10 int"
  [coll]
  (condp = (count coll)
    ;; 0 (conj (empty coll) 0) ; no ints, so return 0 wrapped in whatever 
    ;; type of collection we got, i.e. vec, list, etc.
    1 (map * [10 1] (apply concat (repeat 2 coll))) ; only one int, so parse as (10*num 1*num)
    2 (map * [10 1] coll))) ; 2 ints, so we parse it as
    ;; (10*firstnum 1*secondnum)

(defn get-calibration-val-pt-1
  "gets the calibration val for a coll containing ints and nils. expects
   input from something that has been run through `parse-input-row-str`."
  [instr]
  (->>  instr
        (#(map second %))
        (first-and-last) ;; take first and last val
        (coll-of-ints-to-base-10) ;; do base 10 conversion
        (apply +) ;; sum up the conversion
        ))


(defn get-calibration-val-pt-2
  "gets the calibration val for a coll containing ints and nils. expects
   input from something that has been run through `parse-spelled-and-digit-matches`."
  [instr]
  (->>  instr
        (first-and-last) ;; take first and last val
        (coll-of-ints-to-base-10) ;; do base 10 conversion
        (apply +) ;; sum up the conversion
        ))

;; first row from input data is nqninenmvnpsz874, 
;; so the calibration val should be 94, since we 
;; have a spelled "nine" as the first num, and the 
;; digit 4 as the last num.
(= 94 (get-calibration-val-pt-2 (first input-pt-2)))

;; ANSWER pt 1
(apply + (map get-calibration-val-pt-1 input-pt-1))


;; ANSWER pt 2
(apply + (map get-calibration-val-pt-2 input-pt-2))
