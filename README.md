# Super Tetris
#### It's work in progress...

Immutable Tetris. Tetris in ClojureScript. Super Tetris!

## Overview

A version of the famous game written in ClojureScript, doesn't use any mutation. On each frame, 
the state is modified and passed as a parameter to requestAnimationFrame. 

## Setup

To get an interactive development environment run:

    lein figwheel

and open your browser at [localhost:3449](http://localhost:3449/).
This will auto compile and send all changes to the browser without the
need to reload. After the compilation process is complete, you will
get a Browser Connected REPL. An easy way to try it is:

    (js/alert "Am I connected?")

and you should see an alert in the browser window.

To clean all compiled files:

    lein clean

To create a production build run:

    lein do clean, cljsbuild once min

And open your browser in `resources/public/index.html`. You will not
get live reloading, nor a REPL. 

## License

Copyright Georgy Petukhov © 2016
