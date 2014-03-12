# LiLu

## WIP this works but is not ready for use.

	(import print)
	(import unpack)

	(setv foo "Lisp on luajit? Fuck yeah!")
	(print foo)

	(defn add [a, b] (+ a b))

	(print (add 1 1))

	(defn alwaysTrue [] true)

	(while (== (alwaysTrue) true) (print "awesomesauce"))

## What works?
* Basic code formatted in [EDN](https://github.com/edn-format/edn)

## Planned
* [Terra](http://terralang.org/) functions with macro integration
* Compile to JS with a runtime based on [Moonshine](http://moonshinejs.org/)
* Build system with macro support
