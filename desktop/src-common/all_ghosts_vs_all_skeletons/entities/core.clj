(ns all-ghosts-vs-all-skeletons.entities.core
  (:require [clojure.pprint :as pp] 
            [play-clj.core :refer :all]
            [play-clj.ui :refer :all]
            [play-clj.g2d :refer [texture]]
            [cljan.core :refer :all]
            [cljan.state-monad :refer :all]
            [clojure.string :as string]
            [all-ghosts-vs-all-skeletons.components.core :as c]
            [all-ghosts-vs-all-skeletons.systems.core :as s]
            [all-ghosts-vs-all-skeletons.utilities :as u]
            [all-ghosts-vs-all-skeletons.colors :as colors]))



(def first-names ["Veronica" "James" "Archie" "Susan" "Nico" "Tanya" "Patrick" "Reginald" "Booker" "Jane" "Tyler"])
(def middle-initial "ABCDEFGHIJKLMNOPQRSTUVQXYZ")
(def last-names 
  {
   :skeletons ["Skellington", "Bones", "Rattler", "Socket", "Patella", "Clavicle", "Scapula", "Ulna", "Phalanges"]
   :ghosts ["Spook", "Boo", "Spectro", "Phantom", "Gaspard", "Mist", "Draft", "Smokey", "Shadow", "Shifty", "Shade"]
   })

(defn make-name [type]
  (string/upper-case 
   (str (rand-nth first-names) "  "
        (rand-nth middle-initial) ".  "
        (rand-nth (type last-names)))))

(defn piece [x y image]
  (entity [c/renderable (texture image)]
          [c/piece]
          (into [] (concat [c/display-position] (u/board->screen x y)))
          [c/board-position (int (Math/floor x)) (int (Math/floor y))]))

(defn piece? [ent]
  (if ent (get-ent-component ent c/piece) (state-return nil)))

(defn skeleton [x y]
  (state-call 
   #(state-do 
     (add-component % c/skeleton)
     (add-component % c/name (make-name :skeletons))) 
   (piece x y "skeleton-large.png")))

(defn skeleton? [ent]
  (if ent (get-ent-component ent c/skeleton) (state-return nil)))

(defn ghost [x y]
  (state-call 
   #(state-do 
     (add-component % c/ghost)
     (add-component % c/name (make-name :ghosts)))
   (piece x y "ghost-large.png")))

(defn ghost? [ent]
  (if ent (get-ent-component ent c/ghost) (state-return nil)))

(defn board-highlight [x y]
  (entity 
   (into [] (concat [c/display-position] (u/board->screen x y)))
   [c/board-position x y]
   [c/z-index -200]
   [c/renderable 
    (shape 
     :filled
     :set-color (colors/board (mod (+ x y) 2))
     :rect 0 0 64 64)]))

(defn build-board []
  (state-repeat 
   6 
   (fn [i]
     (state-repeat 6
      (fn [j]
        (board-highlight i j))))))

(defn highlight 
  ([x y clr]
     (entity 
      (into [] (concat [c/display-position] (u/board->screen x y)))
      [c/board-position x y]
      [c/z-index -100]
      [c/renderable (shape 
                     :filled
                     :set-color clr
                     :rect 8 8 (- 64 16) (- 64 16))]))
  ([x y]
     (highlight x y (color :white))))

(defn cursor [x y]
  (entity 
      (into [] (concat [c/display-position] (u/board->screen x y)))
      [c/board-position x y]
      [c/z-index -10]
      [c/renderable (shape 
                     :filled
                     :set-color (colors/autumn 0)
                     :rect 11 11 (- 64 22) (- 64 22))]))

(defn select-cue [x y]
  (state-do 
   [:bind cue (highlight x y (colors/autumn 4))]
   (add-component cue c/select-cue)))

(defn select-cue-position= [ent x y]
  (state-do 
   [:bind [scx scy] (get-ent-component ent c/board-position)]
   (state-return (and (= x scx)
                      (= y scy)))))

(defn delete-all-select-cues []
  (system-for-each s/select-cues #(delete %)))

(defn select-cues [lst]
  (state-map 
   (fn [[x y]]
     (select-cue x y))
   lst))

(defn reset-select-cues [lst]
  (state-do 
   (delete-all-select-cues)
   (select-cues lst)))

(defn set-board-position 
  ([ent x y]
     (state-do
      (set-ent-component ent c/board-position [x y])
      (set-ent-component ent c/display-position (u/board->screen x y))))
  ([ent {:keys [x y]}]
     (set-board-position ent x y)))



(defn timeout [ms & callbacks]
  (entity 
   [c/age]
   [c/duration ms]
   (into [] (concat [c/callbacks] callbacks))))

(defn tween [duration tween-function & {:keys [transform dones] :or {transform identity dones []}}]
  (entity 
   [c/age]
   [c/duration duration]
   [c/tween tween-function :transform transform :dones dones]))

(defn ease-board-position 
  ([ent x y]
     (ease-board-position ent x y []))
  ([ent x y dones]
     (state-do 
      [:bind [dx dy] (get-ent-component ent c/display-position)]
      [:let [tx ty] (u/board->screen x y)]
      (set-ent-component ent c/board-position [x y])
      (tween 200 
             (fn [p]
               (set-ent-component ent c/display-position 
                                  [(u/interpolate dx tx p)
                                   (u/interpolate dy ty p)]))
             :transform #(* % %)
             :dones dones))))

(defn initial-skeletons []
  (state-repeat 6 
                (fn [i]
                  (skeleton i 0))))

(defn initial-ghosts []
  (state-repeat 6
                (fn [i]
                  (ghost i 5))))

(defn text 
  ([string x y]
     (text string x y (color :white)))
  ([string x y clr]
              (entity
               [c/renderable (assoc (label string clr) :x x :y y)]
               [c/display-position x y])))

(defn restart-button []
  (entity 
   [c/renderable (texture "restart-button.png")]
   (into [] (concat [c/display-position] (u/board->screen 2 -1)))
   [c/z-index 100]))


(defn delete-all-pieces []
  (system-for-each s/pieces delete))
