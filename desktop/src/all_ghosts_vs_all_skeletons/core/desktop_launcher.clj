(ns all-ghosts-vs-all-skeletons.core.desktop-launcher
  (:require [all-ghosts-vs-all-skeletons.core :refer :all])
  (:import [com.badlogic.gdx.backends.lwjgl LwjglApplication LwjglApplicationConfiguration]
           [org.lwjgl.input Keyboard])
  (:gen-class))

(defn -main
  []
  (let [cfg (LwjglApplicationConfiguration.)] 
    (set! (. cfg title) "ALL GHOSTS VS ALL SKELETONS")
    (set! (. cfg width) 500)
    (set! (. cfg height) 500)
    (set! (. cfg resizable) false)
    (LwjglApplication. all_ghosts_vs_all_skeletons cfg))
  (Keyboard/enableRepeatEvents true))
