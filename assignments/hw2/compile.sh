#!/usr/bin/env bash

rm -rf classes
mkdir classes
scalac -classpath classes/ -d classes/ src/Data.scala
scalac -classpath classes/ -d classes/ src/Main.scala
scalac -classpath classes/ -d classes/ src/Test.scala
