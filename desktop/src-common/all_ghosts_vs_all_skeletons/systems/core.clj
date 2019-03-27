(ns all-ghosts-vs-all-skeletons.systems.core
  (:require [play-clj.core :refer :all]
            [play-clj.ui :refer :all]
            [cljan.core :refer :all]
            [cljan.state-monad :refer :all]
            [clojure.pprint :as pp]            
            [all-ghosts-vs-all-skeletons.components.core :as c]))

(def drawn 'drawn)

(defn drawn-enter [renderable [x y] ent]
  (dip-component ent c/renderable 
                 #(assoc % :x x :y y)))

(defn drawn-every [renderable [x y] ent]
  (dip-component ent c/renderable 
                 #(assoc % :x x :y y)))

(defn extract-drawn [cljan-state]
  (map first 
       (sort-by second 
                (first ((system-map drawn
                                    (fn [ent]
                                      (state-do 
                                       [:bind 
                                        r (get-ent-component ent c/renderable)
                                        z (get-ent-component ent c/z-index)]
                                       (state-return [r (or z 0)])))) 
                        cljan-state)))))

(def ageing 'ageing)

(defn ageing-every [age ent]
  (state-call #(set-ent-component ent c/age (+ age %)) (state-get :dt)))

(def timeouts 'timeouts)

(defn timeouts-every [age duration callbacks ent]
  (if (> age duration)
    (state-do 
     (state-map 
      (fn [f] (f)) 
      callbacks)
     (delete ent))
    (state-return nil)))

(def tweens 'tweens)
(defn tweens-every [age duration {:keys [tween-function transform dones]} ent]
  (let [percentage (/ age duration)]
    (if (> percentage 1.0)
      (state-do 
       (tween-function (transform 1))
       (state-map (fn [f] (f)) dones)
       (delete ent))
      (tween-function (transform percentage)))))

(def skeletons 'skeletons)
(def ghosts 'ghosts)
(def pieces 'pieces)
(def select-cues 'select-cues)

(defn init []
  (state-do 
   (system drawn [c/renderable c/display-position] 
           {:every drawn-every :enter drawn-enter})
   (system ageing [c/age] 
           {:every ageing-every})
   (system timeouts [c/age c/duration c/callbacks]
           {:every timeouts-every})
   (system tweens [c/age c/duration c/tween]
           {:every tweens-every})
   (system skeletons [c/skeleton])
   (system ghosts [c/ghost])
   (system pieces [c/piece])
   (system select-cues [c/select-cue])))

(defn skeleton-at 
  ([[x y]] (skeleton-at x y))
  ([x y]
     (system-reduce 
      skeletons 
      (fn [previous ent]
        (state-do
         [:bind [sx sy] (get-ent-component ent c/board-position)]
         (if (and (= sx x)
                  (= sy y))
           (state-return ent)
           (state-return previous))))
      nil)))

(defn ghost-at 
  ([[x y]] (ghost-at x y))
  ([x y]
     (system-reduce 
      ghosts 
      (fn [previous ent]
        (state-do
         [:bind [sx sy] (get-ent-component ent c/board-position)]
         (if (and (= sx x)
                  (= sy y))
           (state-return ent)
           (state-return previous))))
      nil)))

(defn piece-at 
  ([[x y]] (piece-at x y))
  ([x y]
     (system-reduce 
      pieces 
      (fn [previous ent]
        (state-do
         [:bind [sx sy] (get-ent-component ent c/board-position)]
         (if (and (= sx x)
                  (= sy y))
           (state-return ent)
           (state-return previous))))
      nil)))

(defn skeletons-have-lost? []
  (state-call #(state-return (= 0 (count %))) (system-entities-snapshot skeletons)))

(defn ghosts-have-lost? []
  (state-call #(state-return (= 0 (count %))) (system-entities-snapshot ghosts)))






