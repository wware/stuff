#!/bin/bash

javac -cp .:mysql-connector-java-5.1.21-bin.jar:/usr/share/java/junit.jar $(find * -name "*.java")
