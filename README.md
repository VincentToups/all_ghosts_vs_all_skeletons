## Introduction

A play-clj game in which a small army of skeletons fights a small army
of ghosts.


## How to Run

    cd dekstop
    lein run

## Contents

* `android/src` Android-specific code
* `desktop/resources` Images, audio, and other files
* `desktop/src` Desktop-specific code
* `desktop/src-common` Cross-platform game code
* `ios/src` iOS-specific code

## Building

All projects can be built using [Nightcode](https://nightcode.info/), or on the command line using [Leiningen](https://github.com/technomancy/leiningen) with the [lein-droid](https://github.com/clojure-android/lein-droid) and [lein-fruit](https://github.com/oakes/lein-fruit) plugins.
