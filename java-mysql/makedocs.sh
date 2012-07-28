#!/bin/bash

javadoc -private -d html -classpath .:mysql-connector-java-5.1.21-bin.jar:/usr/share/java/junit.jar $(find * -name "*.java")
