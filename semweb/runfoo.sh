#!/bin/bash
# Jena jars
JENA=/home/wware/Jena-2.6.2
CLASSPATH=bin:/usr/share/java/tagsoup.jar
CLASSPATH=$CLASSPATH:$JENA/lib/lucene-core-2.3.1.jar
CLASSPATH=$CLASSPATH:$JENA/lib/jena-2.6.2-tests.jar
CLASSPATH=$CLASSPATH:$JENA/lib/xercesImpl-2.7.1.jar
CLASSPATH=$CLASSPATH:$JENA/lib/iri-0.7.jar
CLASSPATH=$CLASSPATH:$JENA/lib/arq-2.8.1.jar
CLASSPATH=$CLASSPATH:$JENA/lib/icu4j-3.4.4.jar
CLASSPATH=$CLASSPATH:$JENA/lib/junit-4.5.jar
CLASSPATH=$CLASSPATH:$JENA/lib/slf4j-log4j12-1.5.6.jar
CLASSPATH=$CLASSPATH:$JENA/lib/stax-api-1.0.1.jar
CLASSPATH=$CLASSPATH:$JENA/lib/jena-2.6.2.jar
CLASSPATH=$CLASSPATH:$JENA/lib/wstx-asl-3.2.9.jar
CLASSPATH=$CLASSPATH:$JENA/lib/slf4j-api-1.5.6.jar
CLASSPATH=$CLASSPATH:$JENA/lib/log4j-1.2.13.jar

#javac -classpath $CLASSPATH Foo.java
#java -classpath $CLASSPATH Foo $@

#rm -rf doc/*
#javadoc -package -d doc -classpath $CLASSPATH net/willware/semweb/*.java

#java -classpath $CLASSPATH net.willware.semweb.Airport
java -classpath $CLASSPATH net.willware.semweb.Foo
