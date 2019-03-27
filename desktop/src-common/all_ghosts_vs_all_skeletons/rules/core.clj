(ns all-ghosts-vs-all-skeletons.rules.core
  (:require [play-clj.core :refer :all]
            [play-clj.ui :refer :all]
            [cljan.core :refer :all]
            [cljan.state-monad :refer :all]
            [clojure.pprint :as pp]
            [all-ghosts-vs-all-skeletons.components.core :as c]
            [all-ghosts-vs-all-skeletons.systems.core :as s]
            [all-ghosts-vs-all-skeletons.utilities :as u]
            [all-ghosts-vs-all-skeletons.entities.core :as e]))

(defn raw-moves [x y]
  [[(+ x 1) y]
   [(- x 1) y]
   [x (+ y 1)]
   [x (- y 1)]])

(defn valid-position? [[x y]] 
  (and (>= x 0)
       (< x 6)
       (>= y 0)
       (< y 6)))

(defn position-empty? [[x y]]
  (state-do 
   [:bind ent (s/piece-at x y)]
   (state-return (if ent false true))))

(defn north [[x y]]
  [x (+ y 1)])

(defn south [[x y]]
  [x (- y 1)])

(defn east [[x y]]
  [(+ x 1) y])

(defn west [[x y]]
  [(- x 1) y])

(def north-east (comp north east))
(def north-west (comp north west))
(def south-west (comp south west))
(def south-east (comp south east))

(defn twice [f] (comp f f))

(defn l-shape [center long short]
  [;(long center)
   ((comp long long) center)
   ((comp short long long) center)])



(defn run-in-direction 
  ([start-position friend? foe? dir length] 
     (run-in-direction start-position friend? foe? dir length []))
  ([start-position friend? foe? dir] 
     (run-in-direction start-position friend? foe? dir Double/POSITIVE_INFINITY []))
  ([start-position friend? foe? dir length acc]
     (if (and (> length 0) (valid-position? start-position))
       (state-do 
        [:bind ent (s/piece-at start-position)]
        [:cond 
         (friend? ent) (state-return acc)
         (foe? ent) (state-return (conj acc start-position))
         (run-in-direction (dir start-position) friend? foe? dir (- length 1) (conj acc start-position))])
       (state-return acc))))

(defn raw-ghost-moves [pos]
  (into [] 
        (into #{} 
              (concat (l-shape pos north east) 
                      (l-shape pos north west) 
                      (l-shape pos south east) 
                      (l-shape pos south west)                      
                      (l-shape pos east north)
                      (l-shape pos east south)
                      (l-shape pos west north)
                      (l-shape pos west south)))))

(defn valid-ghost-moves [ghost]
  (state-do 
   [:bind 
    [x y] (get-ent-component ghost c/board-position)
    moves (state-filter 
           (fn [pos]
             ((lift1 not) (s/ghost-at pos)))
           (filter valid-position? (raw-ghost-moves [x y])))]
   (state-return moves)))

(defn valid-skeleton-moves [skeleton]
  (state-do 
   [:bind 
    [x y] (get-ent-component skeleton c/board-position)
    valid-positions (state-map #(run-in-direction 
                                 (% [x y]) 
                                  e/skeleton? e/ghost? % 2)                            
                               [north-west south-west south-east north-east])]
   (state-return 
    (into [] (apply concat valid-positions)))))

(defn game-over? []
  (state-call #(state-return 
                (or (= 0 (count %1))
                    (= 0 (count %2))))
              (system-entities-snapshot s/ghosts)
              (system-entities-snapshot s/skeletons)))
