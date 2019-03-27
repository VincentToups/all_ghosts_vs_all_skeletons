(defproject all_ghosts_vs_all_skeletons "0.0.1-SNAPSHOT"
  :description "FIXME: write description"
  
  :dependencies [[com.badlogicgames.gdx/gdx "1.4.1"]
                 [com.badlogicgames.gdx/gdx-backend-lwjgl "1.4.1"]
                 [com.badlogicgames.gdx/gdx-box2d "1.4.1"]
                 [com.badlogicgames.gdx/gdx-box2d-platform "1.4.1"
                  :classifier "natives-desktop"]
                 [com.badlogicgames.gdx/gdx-bullet "1.4.1"]
                 [com.badlogicgames.gdx/gdx-bullet-platform "1.4.1"
                  :classifier "natives-desktop"]
                 [com.badlogicgames.gdx/gdx-platform "1.4.1"
                  :classifier "natives-desktop"]
                 [org.clojure/clojure "1.6.0"]
                 [org.clojure/core.match "0.2.1"]
                 [play-clj "0.4.1"]]
  
  :source-paths ["src" "src-common"]
  :javac-options ["-target" "1.6" "-source" "1.6" "-Xlint:-options"]
  :aot [all-ghosts-vs-all-skeletons.core.desktop-launcher]
  :main all-ghosts-vs-all-skeletons.core.desktop-launcher)
