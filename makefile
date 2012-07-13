default : view.scala dama.scala game.scala
	fsc -d build view.scala
	fsc -d build dama.scala
	fsc -d build game.scala
clean : 
	rm *.class
