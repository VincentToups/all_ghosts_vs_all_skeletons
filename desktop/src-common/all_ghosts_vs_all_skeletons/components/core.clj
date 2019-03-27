(ns all-ghosts-vs-all-skeletons.components.core
  (:require [play-clj.core :refer :all]
            [play-clj.ui :refer :all]
            [cljan.core :refer :all]
            [cljan.state-monad :refer :all]))

(defn coordinate [x y]
  [x y])

(defn tween-info [tween-function & {:keys [transform dones] :or {transform identity dones []}}]
  {:tween-function tween-function 
   :transform transform 
   :dones dones})

(def renderable 'renderable)
(def board-position 'board-position)
(def display-position 'display-position)
(def age 'age)
(def callbacks 'callbacks)
(def duration 'duration)
(def tween 'tween)
(def skeleton 'skeleton)
(def ghost 'ghost)
(def name 'name)
(def piece 'piece)
(def select-cue 'select-cue)
(def z-index 'z-index)

(defn always [v]
  (fn [& rest]
    v))

(defn collect [& rest]
  rest)

(defn init []
  (state-do 
   (component renderable)
   (component board-position coordinate)
   (component display-position coordinate)
   (component age (always 0))
   (component callbacks collect)
   (component duration)
   (component tween tween-info)
   (component name)
   (component z-index)
   (tag skeleton)
   (tag ghost)
   (tag piece)
   (tag select-cue)))





