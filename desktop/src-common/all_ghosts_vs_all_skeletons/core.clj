(ns all-ghosts-vs-all-skeletons.core
  (:require [play-clj.core :refer :all]
            [play-clj.ui :refer :all]
            [cljan.core :refer :all]
            [cljan.state-monad :refer :all]
            [clojure.pprint :as pp]
            [all-ghosts-vs-all-skeletons.components.core :as c]
            [all-ghosts-vs-all-skeletons.systems.core :as s]
            [all-ghosts-vs-all-skeletons.utilities :as u]
            [all-ghosts-vs-all-skeletons.entities.core :as e]
            [all-ghosts-vs-all-skeletons.rules.core :as r]
            ))

;; ;; Trivial change 

(def state (atom
            nil))

(defn ghost-tween-out [ent]
  (state-do 
   [:bind [x y] (get-ent-component ent c/display-position)]
   [:let 
    target-x x 
    target-y 564]
   ((lift1 #(sound! % :play)) (state-get :ghost-die-sound))
   (e/tween 500 (fn [p]
            (set-ent-component ent c/display-position
                               [(+ x (* 32 (Math/sin (* 10 p))))
                                (u/interpolate y target-y (* p p))]))
          :dones [(fn [] (delete ent))])))

(defn skeleton-tween-out [ent]
  (state-do 
   [:bind [x y] (get-ent-component ent c/display-position)]
   [:let 
    target-x (rand-int 500)     
    y0 y
    yf -64.0
    y-off 64.0
    a (/ (+ (* 100 y-off) (* 10 y0) (- (* 10 yf))) -9)
    b (/ (+ (* 100 y-off) y0 (- yf)) 9)
    c y0
    ]
   ((lift1 #(sound! % :play)) (state-get :skeleton-die-sound))   
   (e/tween 500 (fn [p]
            (set-ent-component ent c/display-position
                               (let [y-next (u/quadratic p a b c)]
                                 [(u/interpolate x target-x p)
                                  y-next])))
          :dones [(fn [] (delete ent))])))

(defn tween-out [ent]
  (state-do 
   [:ifm (e/skeleton? ent)
    (state-do 
     (skeleton-tween-out ent)
     (remove-component ent c/skeleton))
    (state-do
     (ghost-tween-out ent)
     (remove-component ent c/ghost))]
   (remove-component ent c/piece)))

(defn floor-xy [x y]
  [(Math/floor x) (Math/floor y)])

(defn input->screen* [screen x y]
  (let [mp (input->screen screen x y)]
    [(:x mp) (:y mp)]))

(defn reposition-cursor [board-x board-y]
  (state-do [:bind 
             cursor (state-get :cursor)
             [old-x old-y] (get-ent-component cursor c/board-position)
             sound (state-get :cursor-move-sound)]            
            (if (not (and (= board-x old-x)
                          (= board-y old-y)))
              (state-do 
               (state-return (sound! sound :play))
               (e/set-board-position cursor board-x board-y))
              (state-return nil))))

(defn set-entity-label [board-x board-y]
  (state-do 
   [:bind 
    ent-at (s/piece-at board-x board-y)
    entity-label (state-get :entity-label)
    str (if ent-at (get-ent-component ent-at c/name) (state-return "---"))]
   (dip-component entity-label c/renderable 
                  (fn [lbl]
                    (label! lbl :set-text str)
                    lbl))))

(defn update-turn-label [string]
  (state-call #(dip-component % c/renderable 
                              (fn [lbl]
                                (label! lbl :set-text string)
                                lbl))
              (state-get :turn-label)))

(defn show-skeleton-moves [board-x board-y]
  (state-do 
   [:bind 
    ent-at (s/piece-at board-x board-y)
    skeleton? (e/skeleton? ent-at)]
   (if skeleton? 
     (state-call #(e/reset-select-cues %) (r/valid-skeleton-moves ent-at))
     (e/reset-select-cues []))))

(defn show-ghost-moves [board-x board-y]
  (state-do 
   [:bind 
    ent-at (s/piece-at board-x board-y)
    ghost? (e/ghost? ent-at)]
   (if ghost? 
     (state-call #(e/reset-select-cues %) (r/valid-ghost-moves ent-at))
     (e/reset-select-cues []))))

(declare with-cue)
(declare start-skeleton-turn)
(declare start-ghost-turn)
(declare restart-game)

(defn move-and-kill [ent-at x y and-then]
  (state-do                       
   [:bind other-ent (s/piece-at x y)]
   (state-assoc :input-handler nil)
   (e/ease-board-position ent-at x y
                          [(fn []
                             (state-do
                              (if other-ent  
                                (tween-out other-ent)
                                (state-return nil))
                              [:ifm (r/game-over?)
                               (restart-game)
                               (and-then)]))])))

(defn turn-handler [opponent-at move-shower then cancel]
  (fn [type & args] 
    (case type      
           :mouse-moved 
           (let [[x y] (u/screen->board (first args) (second args))]
             (state-do 
              (e/reset-select-cues [])
              (reposition-cursor x y)
              (set-entity-label x y)
              (move-shower x y)))
           :touch-down
           (state-do 
            [:let [x y] (u/screen->board (first args) (second args))]
            [:bind ent-at (opponent-at x y)]
            [:cond 
             (state-return ent-at) (with-cue #(move-and-kill ent-at %1 %2 then)
                                     cancel)
             (state-return 
              (and (= y -1)
                   (or (= x 2)
                       (= x 3)))) (restart-game)
             (state-return nil)])
           (state-return nil))))

(defn with-cue [result-fun cancel-fun]
  (state-do
   (update-turn-label "SELECT A SPACE")
   (state-assoc
    :input-handler
    (fn with-cue-input-handler [type & args]
      (case type 
        :mouse-moved 
        (let [[x y] (u/screen->board (first args) (second args))]
          (state-do 
           (set-entity-label x y)
           [:bind matching-cues (system-filter s/select-cues #(e/select-cue-position= % x y))]
           (case (count matching-cues)
             0 (state-return nil)
             1 (state-do 
                (reposition-cursor x y))
             (reposition-cursor x y))))
        :touch-down         
        (let [[x y] (u/screen->board (first args) (second args))]
          (state-do 
           (set-entity-label x y)
           [:bind matching-cues (system-filter s/select-cues #(e/select-cue-position= % x y))]
           (case (count matching-cues)
             0 (cancel-fun)
             1 (result-fun x y)
             (result-fun x y))))
        (state-return nil))))))


(declare skeleton-turn-handler)
(declare ghost-turn-handler)
(defn start-skeleton-turn []
  (state-do
   (update-turn-label "SKELETON TURN") 
   (state-assoc :input-handler 
                skeleton-turn-handler)))

(defn start-ghost-turn []
  (state-do 
   (update-turn-label "GHOST TURN")
   (state-assoc :input-handler 
                ghost-turn-handler)))

(def skeleton-turn-handler (turn-handler s/skeleton-at show-skeleton-moves start-ghost-turn start-skeleton-turn))
(def ghost-turn-handler (turn-handler s/ghost-at show-ghost-moves start-skeleton-turn start-ghost-turn))


(defn cljan-init []
  (state-do 
   (c/init)
   (s/init)
   (state-assoc :cursor-move-sound (sound "cursor-move-quiet.wav"))
   (state-assoc :skeleton-die-sound (sound "skeleton-die.wav"))
   (state-assoc :ghost-die-sound (sound "ghost-die.wav"))
   (state-assoc :dt 16)
   (state-assoc :last-tick (u/now-ms))
   (e/restart-button)
   (e/build-board)
   (state-call #(state-assoc :cursor %) (e/cursor 0 0))
   (state-call #(state-assoc :turn-label %) (e/text "SKELETON TURN" 4 480))
   (state-call #(state-assoc :entity-label %) (e/text "---" 4 460))
   (e/initial-skeletons)
   (e/initial-ghosts)
   (start-skeleton-turn)))

(defn restart-game []
  (state-do 
   (e/delete-all-pieces)
   (e/initial-skeletons)
   (e/initial-ghosts)
   (start-skeleton-turn)))

(defn init [_]
  (run-cljan-for-state (cljan-init)))

(defn update [cljan-function]
  (fn [state]
    (let [last-tick (:last-tick state)
          now (u/now-ms)
          dt (min 20 (- now last-tick))]
      (second (cljan-function (assoc state :dt dt :last-tick now))))))

(defn cljan-update []
  (state-do 
   (execute-system s/ageing)
   (execute-system s/timeouts)
   (execute-system s/tweens)
   (execute-system s/drawn)))

(defn process-input! [& args]
  (let [f (:input-handler @state)]
    (if f 
      (swap! state 
            #(second ((apply f args) %)))
      nil))
  )




(defscreen main-screen
  :on-show
  (fn [screen entities]
    (update! screen :renderer (stage))    
    (swap! state init)
    (let [ents (s/extract-drawn @state)]
      (pp/pprint ents) (newline) (flush)
      ents)
    )
  
  :on-render
  (fn [screen entities]
    (clear!)
    (swap! state (update (cljan-update)))
    (let [ents (s/extract-drawn @state)]
      (render! screen ents)
      ents)
    )
  :on-key-down
  (fn [screen entities]
    (process-input! :key-down (:key screen))
    entities)
  ; a key was typed
  :on-key-typed
  (fn [screen entities]
    (process-input! :key-typed (:character screen))
    entities)
  ; a key was released
  :on-key-up
  (fn [screen entities]
    (process-input! :key-up (:key screen))    
    entities)
  ; the mouse was moved without pressing any buttons
  :on-mouse-moved
  (fn [screen entities]
    (apply process-input! :mouse-moved (input->screen* screen (:input-x screen) (:input-y screen)))
    entities)
  ; the mouse wheel was scrolled
  :on-scrolled
  (fn [screen entities]
    (process-input! :scrolled (:amount screen))
    entities)
  ; the screen was touched or a mouse button was pressed
  :on-touch-down
  (fn [screen entities]
    (let [[x y] (input->screen* screen (:input-x screen) (:input-y screen))] 
      (process-input! :touch-down x y
                      (:pointer screen) (:button screen)))
    entities)
  ; a finger or the mouse was dragged
  :on-touch-dragged
  (fn [screen entities]
    (process-input! :touch-dragged (:input-x screen) (:input-y screen)
                    (:pointer screen))
    entities)
  ; a finger was lifted or a mouse button was released
  :on-touch-up
  (fn [screen entities]
    (process-input! :touch-up (:input-x screen) (:input-y screen)
                    (:pointer screen)
                    (:button screen))
    entities))

(defgame all_ghosts_vs_all_skeletons
  :on-create
  (fn [this]
    (set-screen! this main-screen)))

;; ;;;;;;;;;;;;; Compiling Version ;;;;;;;;;;;;;;;;

;; (ns all-ghosts-vs-all-skeletons.core
;;   (:require [play-clj.core :refer :all]
;;             [play-clj.ui :refer :all]
;;             [cljan.core :refer :all]
;;             [cljan.state-monad :refer :all]
;;             [clojure.pprint :as pp]
;;             ))

;; (defscreen main-screen
;;   :on-show
;;   (fn [screen entities]
;;     (update! screen :renderer (stage))    
;;     )
  
;;   :on-render
;;   (fn [screen entities]
;;     (clear!)
;;     )

;;   )

;; (defgame all_ghosts_vs_all_skeletons
;;   :on-create
;;   (fn [this]
;;     (set-screen! this main-screen)))
