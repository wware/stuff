.SUFFIXES: .java .class ;

CLASSPATH := \
	/usr/local/neo4j-kernel-1.0/neo4j-kernel-1.0.jar \
	/usr/local/neo4j-kernel-1.0/geronimo-jta_1.1_spec-1.1.1.jar \
	.

# Convert spaces to colons.
EMPTY:=
SPACE:=$(EMPTY) $(EMPTY)
CLASSPATH:=$(subst $(SPACE),:,$(CLASSPATH))

JFLAGS=-cp $(CLASSPATH)
#JFLAGS=-server -cp $(CLASSPATH)

.java.class:
	javac $(JFLAGS) $(@:.class=.java)

run: Foo.class
	java $(JFLAGS) Foo

j:
	jython $(JFLAGS)

clean:
	rm -f *.class *~
	rm -rf my-neo4j-db/
