package all_ghosts_vs_all_skeletons.core;

import clojure.lang.RT;
import clojure.lang.Symbol;

import com.badlogic.gdx.*;
import com.badlogic.gdx.backends.iosrobovm.*;

import org.robovm.apple.foundation.*;
import org.robovm.apple.uikit.*;

public class IOSLauncher extends IOSApplication.Delegate {
	protected IOSApplication createApplication() {
		IOSApplicationConfiguration config = new IOSApplicationConfiguration();
		RT.var("clojure.core", "require").invoke(Symbol.intern("all-ghosts-vs-all-skeletons.core"));
		try {
			Game game = (Game) RT.var("all-ghosts-vs-all-skeletons.core", "all_ghosts_vs_all_skeletons").deref();
			return new IOSApplication(game, config);
		} catch (Exception e) {
			e.printStackTrace();
		}
		return null;
	}

	public static void main(String[] argv) {
		NSAutoreleasePool pool = new NSAutoreleasePool();
		UIApplication.main(argv, null, IOSLauncher.class);
		pool.close();
	}
}
