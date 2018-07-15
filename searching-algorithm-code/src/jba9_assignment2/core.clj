(ns jba9-assignment2.core
  (:gen-class)
  (:require [clojure.pprint :as pp])
  (:require [clojure.string :as str])
  (:require [clojure.set :as set])
  (:require [clojure.data.priority-map :refer [priority-map]]))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defn read-map-from-file
  "Takes a filename and reads in a map, returns a new state with a zero length path"
  [filename]
  (let[file (slurp filename)]
    (hash-map :map (into[](str/split-lines file)) :path [])))

(defn return-coords
  "take a state and returns the coordinates of the starting point marked by an s"
  [state char]
  ;get the map
  (let [state-map (get state :map)]
    ;get the coordinates where the character equals a character in the map
    (first(for[[y row] (map-indexed vector state-map)
               [x col] (map-indexed vector row)
               :when (= char col)]
            [x y]))))

(defn start
  "take a state and returns the coordinates of the starting point marked by a S"
  [state]
  (return-coords state \S))

(defn goal
  "take a state and returns the coordinates of the ending point marked by a G"
  [state]
  (return-coords state \G))

(defn position
  "takes a state and returns the coordinates of the ship on the map, can be worked out by following path from start"
  [state]
  (let [;get the frequiencies of items in the path
         path (frequencies(get state :path))
         ;if there is no directions return nil, otherwise return the frequencie of that direction
         up (if(nil? (get path :up)) 0 (get path :up))
         down (if(nil? (get path :down)) 0 (get path :down))
         left (if(nil? (get path :left)) 0 (get path :left))
         right (if(nil? (get path :right)) 0 (get path :right))
         ;get the coordinates via subtracting frequencies of directions from eachother
         y (- down up)
         x (- right left)
         start-pos (start state)]
    [(+ x (get start-pos 0)) (+ y (get start-pos 1))]))

(defn cost
  "returns the cost of a state *length of the path*"
  [state]
  (count (get state :path)))

(defn heuristic
  "takes a state and computes it heuristic value using the euclidean distance metric"
  [state]
  (let [curr-pos (position state)
        goal-pos (goal state)]
    ;mathmatical function for heuristic
    (Math/sqrt (+ (Math/pow (- (get curr-pos 0) (get goal-pos 0)) 2)
                  (Math/pow (- (get curr-pos 1) (get goal-pos 1)) 2)))))

(defn positions-vector
  "gets vector of coordinates for all travelled paths"
  [state]
  ;loop through path and create a vector of coordinates for the travelled path
  (loop [p (get state :path) counter 0 pl [(start state)]]
    (if (= (count p) counter)
      (into [] pl)
      (let [pll (if(= (get p counter) :up)
                  (conj pl [(get (first (take-last 1 pl)) 0) (- (get (first (take-last 1 pl)) 1) 1)])
                  (if(= (get p counter) :down)
                    (conj pl [(get (first (take-last 1 pl)) 0) (+ (get (first (take-last 1 pl)) 1) 1)])
                    (if(= (get p counter) :left)
                      (conj pl [(- (get (first (take-last 1 pl)) 0) 1) (get (first (take-last 1 pl)) 1)])
                      (if(= (get p counter) :right)
                        (conj pl [(+ (get (first (take-last 1 pl)) 0) 1) (get (first (take-last 1 pl)) 1)])))))]
        (recur p (inc counter) pll)))))

(defn draw-path
  "draws path of periods"
  [state]
  (let [;get the positions vector of the state
         pos-list (positions-vector state)
         ;maps states to their positions gets the character at each maps position and if it is a space replace with a dot
         dotmap (reduce
                  (fn [m [x y]]
                    (let [c (get-in m [y x])]
                      (if (not= c \space) m
                        (assoc-in m [y x] \.))))
                  (mapv vec (:map state)) pos-list)]
    (assoc state :map (mapv #(apply str %) dotmap))))

(defn print-state
  "Takes a state and pretty prints it to the console, map and path with associated periods should be displayed"
  [state]
  (let [state-map (get state :map)
        state-path (get state :path)
        pos (start state)
        pos-vec (positions-vector state)]
    ;prints each line in the map
    (doseq [l (:map (draw-path state))]
      (println l))
    (pp/pprint state-path)))

(defn return-state
  "Returns nil or a state"
  [state [x y] dir]
  (let [;create a vector of maps
         curr-map (mapv vec (:map (draw-path state)))
         ;gets the character at a position in the map
         chr (get-in curr-map [y x])]
    ;if it is a space or the goal return a state with the new direction
    (if (or (= chr \space) (= chr \G))
      {:map (:map state) :path (conj (:path state) dir)})))

(defn expand
  "Takes a state and returns a list of new states obtained by extending the length
  of the given states path by one in all possible valid directions"
  [state]
  (let [p-state (draw-path state)
        [x y] (position p-state)
        up [x (dec y)]
        down [x (inc y)]
        left [(dec x) y]
        right [(inc x) y]]
    ;return a list of all expansions and remove nil expansions
    (remove nil?
            (list (return-state state up :up)
                  (return-state state down :down)
                  (return-state state left :left)
                  (return-state state right :right)))))

(defn unique-states
  "returns unique-states *states without the same position as other states*"
  [new-states ppos]
  ;for every new state, check if its coordinates are the same as any in the position vector, return the unique ones
  (for [x new-states
        :let [y (position x)]
        :when (not (.contains ppos y))]
    x))

;global flag for verbose
(def verbose false)

(defn best-first-search
  "algorithm for best first search"
  ;for the first option call best first on the state
  ([start-state]
   (best-first-search (priority-map start-state (heuristic start-state)) 0 []))
  ;for the second option recur a best first search
  ([open counter prevpos]
   (let [;get the first thing in the priority map
          curr-state (first (peek open))]
     (if (= (position curr-state) (goal curr-state))
       ;if it is the goal state return it
       (do
         (print-state curr-state)
         (println "---")
         (println "Expansions: " counter " Cost: " (cost curr-state))
         (println "---"))
       ;if it is not the goal state recur
       (do
         (when verbose
           (print-state curr-state)
           (println "---")
           (println "Expansions: " counter)
           (println "---"))
         (recur
           ;pop of the front off the priority map and add unique states mapped to their heuristics
           (into (pop open) (map #(vector % (heuristic %)) (unique-states (expand curr-state) prevpos)))
           (inc counter)
           ;get positions of all expanded states and the current state and add them to the previous position list
           (into prevpos (into (position curr-state) (map position (expand curr-state))))))))))

(defn best-first
  "calls best first search using a state read from a file"
  [filename]
  (let [state (read-map-from-file filename)]
    (best-first-search state)))

(defn heuristic-cost
  "add the heuristic and the cost"
  [state]
  (+ (heuristic state) (cost state)))

(defn a-star-search
  "algorithm for the a star search"
  ;for the first option call a-star on the state
  ([start-state]
   (a-star-search (priority-map start-state (heuristic-cost start-state)) 0 []))

  ;for the second option recur a-star
  ([open counter prevpos]
   (let [;get the first thing in the priority-map
          curr-state (first (peek open))]
     (if (= (position curr-state) (goal curr-state))
       ;if it is the goal state return it
       (do
         (print-state curr-state)
         (println "---")
         (println "Expansions: " counter " Cost: " (cost curr-state))
         (println "---"))
       ;if it is not the goal state return
       (do
         (when verbose
           (print-state curr-state)
           (println "---")
           (println "Expansions: " counter)
           (println "---"))
         (recur
           ;pop of the front off the priority map and add unique states mapped to their heuristic + cost
           (into (pop open) (map #(vector % (heuristic-cost %)) (unique-states (expand curr-state) prevpos)))
           (inc counter)
           ;get positions of all expanded states and the current state and add them to the previosu position list
           (into prevpos (into (position curr-state) (map position (expand curr-state))))))))))

(defn a-star
  "calls the a-star algorithm using a state read from a file"
  [filename]
  (let [state (read-map-from-file filename)]
    (a-star-search state)))










