package all_ghosts_vs_all_skeletons.core;

import clojure.lang.RT;
import clojure.lang.Symbol;

import com.badlogic.gdx.backends.android.AndroidApplication;
import com.badlogic.gdx.Game;

public class AndroidLauncher extends AndroidApplication {
	public void onCreate (android.os.Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
          RT.var("clojure.core", "require").invoke(Symbol.intern("all-ghosts-vs-all-skeletons.core"));
		try {
			Game game = (Game) RT.var("all-ghosts-vs-all-skeletons.core", "all_ghosts_vs_all_skeletons").deref();
			initialize(game);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
}
