SWING_PATH = /home/moreno/play-2.1.0/repository/local/org.scala-lang/scala-swing/2.9.1/jars

default : view.scala dama.scala game.scala
	fsc -classpath $(SWING_PATH)/scala-swing.jar -d build view.scala
	fsc -classpath $(SWING_PATH)/scala-swing.jar -d build dama.scala
	fsc -classpath $(SWING_PATH)/scala-swing.jar -d build game.scala
clean : 
	rm *.class
