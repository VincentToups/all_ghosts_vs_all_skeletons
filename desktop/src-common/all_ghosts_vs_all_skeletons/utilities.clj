(ns all-ghosts-vs-all-skeletons.utilities)

(defn now-ms []
  (* 1.0e-6 (System/nanoTime)))

(defn interpolate [s e p]
  (+ (* s (- 1 p))
     (* e p)))

(defn board->screen [x y]
  [(+ 58 (* x 64))
   (+ 58 (* y 64))])

(defn screen->board [x y]
  [(int (Math/floor (/ (- x 58) 64)))
   (int (Math/floor (/ (- y 58) 64)))])

(defn quadratic [x a b c]
  (+ (* x x a) (* x b) c))



