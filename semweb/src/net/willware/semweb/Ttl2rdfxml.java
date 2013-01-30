package net.willware.semweb;

/*
 * javac -cp /opt/apache-jena/lib/jena-core-2.7.4.jar Ttl2rdfxml.java
 * export CP=$(ls /opt/apache-jena/lib/*.jar | python -c \
 *    'import sys;print ":".join(map(lambda x:x.rstrip(),sys.stdin.readlines()))')
 * java -cp war/WEB-INF/classes:$CP Ttl2rdfxml < turtlefile  > rdfxmlfile
 */

import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.ModelFactory;
import java.io.IOException;

class Ttl2rdfxml {
	public static void main(String[] args) throws IOException {
		Model m = ModelFactory.createDefaultModel();
		m.read(System.in, null, "TTL").write(System.out);
	}
}
