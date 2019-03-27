(ns all-ghosts-vs-all-skeletons.colors
  (:require 
   [play-clj.core :refer :all]
   [play-clj.ui :refer :all]))

(defn clr255 [r g b]
  (color (/ r 255)
         (/ g 255)
         (/ b 255)
         1))

(def autumn 
  [(clr255 104 103 36)
   (clr255 205 142 39)
   (clr255 198 87 0)
   (clr255 153 51 39)
   (clr255 89 40 33)])

(def board 
  [(clr255 31 22 12)
   (clr255 19 9 26)])

