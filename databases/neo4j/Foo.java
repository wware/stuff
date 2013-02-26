/**
 * javac -cp /usr/local/neo4j-kernel-1.0/neo4j-kernel-1.0.jar Foo.java
 */

import java.util.Map;
import org.neo4j.graphdb.RelationshipType;
import org.neo4j.graphdb.GraphDatabaseService;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.kernel.EmbeddedGraphDatabase;
import org.neo4j.graphdb.Transaction;

public class Foo {
    public enum MyRelationshipTypes implements RelationshipType
    {
	KNOWS
    }

    public void go() {

	GraphDatabaseService graphDb;
	Node firstNode, secondNode;
	Relationship relationship;

	if (false) {
	    Map<String,String> configuration =
		EmbeddedGraphDatabase.loadConfigurations("neo4j_config.props");
	    graphDb =
		new EmbeddedGraphDatabase("my-neo4j-db/", configuration);
	} else {
	    graphDb =
		new EmbeddedGraphDatabase("my-neo4j-db/");
	}

	Transaction tx = graphDb.beginTx();
	try {
	    firstNode = graphDb.createNode();
	    secondNode = graphDb.createNode();
	    relationship =
		firstNode.createRelationshipTo(secondNode,
					       MyRelationshipTypes.KNOWS);
	    firstNode.setProperty("message", "Hello, ");
	    secondNode.setProperty("message", "world!");
	    relationship.setProperty("message", "brave Neo4j ");
	    tx.success();
	}
	finally {
	    tx.finish();
	}

	System.out.print(firstNode.getProperty("message"));
	System.out.print(relationship.getProperty("message"));
	System.out.print(secondNode.getProperty("message"));
	System.out.println();
	graphDb.shutdown();
    }

    public static void main(String[] args) {
	Foo foo = new Foo();
	foo.go();
    }
}
