package net.willware.semweb;

/*
 * export CP=$(ls /opt/apache-jena/lib/*.jar | python -c \
 *   'import sys;print ":".join(map(lambda x:x.rstrip(),sys.stdin.readlines()))')
 * java -cp war/WEB-INF/classes:$CP net.willware.semweb.Hack
 */

import java.io.File;
import java.io.FileReader;
import java.util.Iterator;

import com.hp.hpl.jena.rdf.model.InfModel;
import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.ModelFactory;
import com.hp.hpl.jena.rdf.model.Statement;
import com.hp.hpl.jena.rdf.model.StmtIterator;
import com.hp.hpl.jena.reasoner.Derivation;
import com.hp.hpl.jena.reasoner.Reasoner;
import com.hp.hpl.jena.reasoner.ReasonerRegistry;
import com.hp.hpl.jena.reasoner.rulesys.GenericRuleReasoner;
import com.hp.hpl.jena.reasoner.rulesys.Rule;
import com.hp.hpl.jena.util.PrintUtil;

public class Hack {

	private static final String baseUri = "http://willware.net/rdf/";
	private static final boolean SHOW_WORK = true;

	public static void main(String[] args) throws Exception {
		Model model = ModelFactory.createDefaultModel();
		File f = new File(args[0]);
		FileReader fr = new FileReader(f);
		if (args[0].endsWith(".ttl")) {
			model.read(fr, baseUri, "TTL");  // turtle
		} else {
			model.read(fr, baseUri);   // rdf/xml
		}

		Reasoner reasoner = ReasonerRegistry.getOWLReasoner();
		reasoner.setDerivationLogging(SHOW_WORK);

		String rules = "[ DefinitionOfVertebrates: (?x rdf:type " + baseUri+"Vertebrate) -> " +
				"(?x " + baseUri+"has " + baseUri+"Vertebrae) ]";
		Reasoner reasoner2 = new GenericRuleReasoner(Rule.parseRules(rules));
		reasoner2.setDerivationLogging(SHOW_WORK);

		/*
		 * We can stack up multiple reasoners to handle different kinds of reasoning.
		 */
		InfModel inf = ModelFactory.createInfModel(reasoner2,
				ModelFactory.createInfModel(reasoner, model));

		if (SHOW_WORK) {
			StmtIterator siter = inf.listStatements();
			while (siter.hasNext()) {
				Statement s = siter.nextStatement();
				Iterator<Derivation> id = inf.getDerivation(s);
				if (id.hasNext()) {
					System.out.println(PrintUtil.print(s));
					while (id.hasNext()) {
						System.out.println("because of " + PrintUtil.print(id.next()));
					}
				}
			}
		}

		inf.write(System.out, "TTL");
	}
}
