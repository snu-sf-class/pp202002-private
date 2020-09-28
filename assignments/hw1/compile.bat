rd /s /q classes
mkdir classes
call scalac -classpath classes/ -d classes/ src/Main.scala
call scalac -classpath classes/ -d classes/ src/Test.scala