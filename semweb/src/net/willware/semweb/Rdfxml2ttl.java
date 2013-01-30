package net.willware.semweb;

/*
 * javac -cp /opt/apache-jena/lib/jena-core-2.7.4.jar Rdfxml2ttl.java
 * export CP=$(ls /opt/apache-jena/lib/*.jar | python -c \
 *    'import sys;print ":".join(map(lambda x:x.rstrip(),sys.stdin.readlines()))')
 * java -cp war/WEB-INF/classes:$CP Rdfxml2ttl < rdfxmlfile > turtlefile
 */

import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.ModelFactory;
import java.io.IOException;

class Rdfxml2ttl {
	public static void main(String[] args) throws IOException {
		Model m = ModelFactory.createDefaultModel();
		m.read(System.in, null).write(System.out, "TTL");
	}
}

