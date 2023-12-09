(ns aoc2023.day2
  (:require [clojure.string :as s])) 


;;12 red cubes, 13 green cubes, and 14 blue cubes

(def input (-> "resources/day2_input.txt"
               slurp
               s/split-lines)
  )

;; testing
(def test-row (first input))
test-row
(def temp ( clean-game-row test-row))
(def game-map (parse-game-rounds temp) )



(def color->kw {"red" :red "blue" :blue "green" :green})

;; Encoding valid game constraints
;;12 red cubes, 13 green cubes, and 14 blue cubes
(def color->max-occurrence {:red 12 :green 13 :blue 14})

(defn get-rounds [game-row]
  (s/split game-row #";");; split into individual rounds 
  )

(defn clean-game-row 
  "returns a list of rounds (each round is a string) for a given game row"
  [game-row]
  (let [results (get-rounds game-row)]
    (cons (s/replace (first results) #"^Game \d+: " "") (into [] (rest results)))))

(defn game->map
  "turns a a round into a map containing the  
   counts and colors that occurred in that round."
  [outcome]
  (let  [parsed-outcome (s/split (s/triml outcome) #" ")
         color (second parsed-outcome)
         number (first parsed-outcome)]
    {(color->kw color) (parse-long number)}))

(defn parse-game-rounds 
  "turns the rounds of a game into a vect of maps containing the  
   counts and colors that occurred in each round, flattened.
   expects input in the form of a seq of strings (each string is a round)
   ex: ('7 blue, 6 green, 3 red' ' 3 red, 5 green, 1 blue'), 
   => [{:blue 7} {:green 6} {:red 3} {:red 3} {:green 5} {:blue 1}]"
  [game-row-seq]
  (let [rounds (map #(s/split % #",") game-row-seq)] ;; parse rounds
    (into [] (flatten (map (fn [game]
                             (map game->map game)) rounds)))))

(defn valid-count? 
  "{:blue 7} => true"
  [color-count]
   (condp = (first (keys color-count))
     :blue (<= (color-count :blue) (color->max-occurrence :blue))
     :green (<= (color-count :green) (color->max-occurrence :green))
     :red (<= (color-count :red) (color->max-occurrence :red))))


(defn is-game-possible? [game-map]
  (reduce #(or %1 %2) false ( map valid-count? game-map)) 
  )

(is-game-possible? game-map)

(defn get-id [game-row]
  (parse-long (second (s/split (first (s/split game-row #":")) #" "))))


(defn valid-game? [game-row]
  (let [id (get-id game-row)
        possible? (-> game-row
                      clean-game-row
                      parse-game-rounds
                      is-game-possible?)]
    (when possible? id) 
    )
  )

( parse-game-rounds (clean-game-row test-row))

(first (s/split test-row #":"))

(valid-game? test-row)

(def day2-pt1 )
