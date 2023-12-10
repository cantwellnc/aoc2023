(ns aoc2023.day2
  (:require [clojure.string :as s])) 

(def input (-> "resources/day2_input.txt"
               slurp
               s/split-lines)
  )

(def color->kw {"red" :red "blue" :blue "green" :green})

;; Encoding valid game constraints
;; 12 red cubes, 13 green cubes, and 14 blue cubes are the # of 
;; cubes in the bag. Counts for a color cannot exceed this in a round.
(def color->max-occurrence {:red 12 :green 13 :blue 14})

(defn get-rounds [game-row]
  (s/split game-row #";");; split into individual rounds 
  )

(defn clean-game-row 
  "returns a seq of rounds (each round is a string) for a given game row. 
   ex: Game 84: 4 red, 2 blue; 1 green => ('4 red 2 blue' ' 1 green')"
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


(defn is-game-possible? 
  "do an and-scan over the bools prooduced 
   by `valid-count?`. Only true if all cube color counts are 
   valid."
  [game-map] 
  (reduce #(and %1 %2) true ( map valid-count? game-map)) 
  )

(defn get-id [game-row]
  (parse-long (second (s/split (first (s/split game-row #":")) #" "))))


(defn valid-game? 
  "determines if a game is valid or not."
  [game-row]
  (let [id (get-id game-row)
        possible? (-> game-row
                      clean-game-row
                      parse-game-rounds
                      is-game-possible?)]
    (if possible? id 0) 
    )
  )


(def day2-pt1 (apply + ( map valid-game? input)))

day2-pt1
