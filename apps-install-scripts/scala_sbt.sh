#!/bin/sh

scalav=2.12.1
sbtv=0.13.13

sudo apt remove scala-library scala
wget http://www.scala-lang.org/files/archive/scala-$scalav.deb
sudo gdebi scala-$scalav.deb --n
sudo apt update
sudo apt install scala -y

wget http://dl.bintray.com/sbt/debian/sbt-$sbtv.deb
sudo gdebi sbt-$sbtv.deb --n
sudo apt update
sudo apt install sbt -y
