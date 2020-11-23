rd /s /q classes
mkdir classes
call scalac -classpath classes/ -d classes/ src/Data.scala
call scalac -classpath classes/ -d classes/ src/Caesar.scala
call scalac -classpath classes/ -d classes/ src/Enigma.scala
call scalac -classpath classes/ -d classes/ src/Test.scala
