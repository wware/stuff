/*
This is a Turtle-to-RDF/XML converter I ended up not needing. But it's
short and elegant, so we preserve it for posterity.

javac -cp /opt/apache-jena/lib/jena-core-2.7.4.jar Hack.java
export CP=$(ls /opt/apache-jena/lib/*.jar | python -c \
 'import sys;print ":".join(map(lambda x:x.rstrip(),sys.stdin.readlines()))')
java -cp .:$CP Hack turtlefile rdfxmlfile
 */

import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.ModelFactory;
import java.io.IOException;

class Hack {
	public static void main(String[] args) throws IOException {
		Model m = ModelFactory.createDefaultModel();
		m.read(System.in, null, "TTL").write(System.out);
	}
}

