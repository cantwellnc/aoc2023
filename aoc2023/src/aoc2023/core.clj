(ns aoc2023.core
   [:require clojure.string])

;; main fn, ignore
 (defn foo
   "I don't do a whole lot."
   [x]
   (println x "Hello, World!"))


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
   {#"zero" "0" #"one" "1" #"two" "2" #"three" "3" #"four" "4" #"five" "5" #"six" "6" #"seven" "7" #"eight" "8" #"nine" "9"})

 

 ;; these functions will have to be totally revamped i think...
 ;; we need to be able to identify [idx match]
 (defn find-and-replace
   "find keys of replacement-map in instr and replace them with their 
   respective values. returns a seq of [index-of-val-replaced replaced-val]"
   [instr replacement-map]
   (map (fn [[k v]] [(clojure.string/index-of instr (str k)) (clojure.string/replace instr k v)]) replacement-map))

 (defn parse-spelled-int
   "A specific instance of find and replace with spelled-int->int replacement map."
   [instr]
   (find-and-replace instr spelled-int->int))

 (sort-by first (filter #(some? (first %)) (parse-spelled-int "qmdtwone66gcnlhtnjmfour")))


(defn parse-input-row-str
  "turn str into vect of chars then parse out 
   the ints and return them in a vec. if a char is not 
   parseable as an int, return nil."
  [row]
  (let [chars (clojure.string/split row #"")] 
    (map-indexed (fn [idx char]
           (try
             [idx (Integer/parseInt char)]
             (catch Exception _))) chars)))

(parse-input-row-str "66")

;; read in input, and split into a vec 
;; of vecs of chars that have been run through
;; parse-input-row-str. 
  (def input (->
              (slurp "resources/day1_input.txt")
              (clojure.string/split #"\n")
              (#(reduce (fn [init-val row] (conj init-val (parse-input-row-str row))) [] %))))

  (defn first-and-last
    "gets the first and last item of coll and puts them in a vec together"
    [coll] (filter some? [(first coll) (last (rest coll))]))

  (defn coll-of-ints-to-base-10
    "turns a coll of up to 2 ints into a base 10 int"
    [coll]
    (condp = (count coll)
      0 (conj (empty coll) 0) ; no ints, so return 0 wrapped in whatever 
    ;; type of collection we got, i.e. vec, list, etc.
      1 (conj (empty coll) (first coll)) ; only one int, so parse as itself + 
    ;; return in collection wrapper
      2 (map * [10 1] coll))) ; 2 ints, so we parse it as
    ;; 10*first + 1*second


  (defn get-calibration-val
    "gets the calibration val for a coll containing ints and nils. expects
   input from something that has been run through `parse-input-row-str`."
    [instr]
    (->>  instr
          (filter int?) ;; collect only the ints
          (first-and-last) ;; take first and last val
          (coll-of-ints-to-base-10) ;; do base 10 stuff
          (apply +) ;; sum up 
          ))

;; first row from input data is nqninenmvnpsz874, 
;; so the calibration val should be 84
  (= 84 (get-calibration-val (first input)))

;; ANSWER
  (apply + (map get-calibration-val input))


  (clojure.string/replace "42five5sevenjjfbdtrdmb36" #"five" "5"))