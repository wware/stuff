/* -*- mode: java; c-basic-offset: 4; indent-tabs-mode: nil -*- */

package net.willware.semweb;

import java.util.ArrayList;
import java.io.PrintWriter;
import java.io.PrintStream;

import javax.servlet.jsp.JspWriter;

import com.hp.hpl.jena.rdf.model.InfModel;
import com.hp.hpl.jena.rdf.model.Literal;
import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.ModelFactory;
import com.hp.hpl.jena.rdf.model.Property;
import com.hp.hpl.jena.rdf.model.Resource;
import com.hp.hpl.jena.reasoner.Reasoner;
import com.hp.hpl.jena.reasoner.ReasonerRegistry;
import com.hp.hpl.jena.sparql.core.ResultBinding;
import com.hp.hpl.jena.vocabulary.RDFS;

public class Foo {

    private static boolean USE_OWL = true;
    private static boolean DUMP_AND_QUIT = false;
    private static boolean NOT_REALLY = false;

    private static final String Name = "<http://xmlns.com/foaf/0.1/name>";
    private static final String Knows = "<http://xmlns.com/foaf/0.1/knows>";
    private static final String Interest = "<http://xmlns.com/foaf/0.1/interest>";
    private static final String Event = "<http://purl.org/vocab/bio/0.1/event>";
    private static final String Birth = "<http://purl.org/vocab/bio/0.1/Birth>";
    private static final String Marriage = "<http://purl.org/vocab/bio/0.1/Marriage>";
    private static final String Death = "<http://purl.org/vocab/bio/0.1/Death>";
    private static final String Gender = "<http://xmlns.com/foaf/0.1/gender>";
    private static final String Rdftype = "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>";
    private static final String Date = "<http://purl.org/vocab/bio/0.1/date>";

    //private static final String Parent = "<" + GenealogyModel.familyRdf + "/parent>";
    private static final String Father = "<http://purl.org/vocab/bio/0.1/father>";
    private static final String Mother = "<http://purl.org/vocab/bio/0.1/mother>";
    private static final String childOf = "<http://purl.org/vocab/relationship/childOf>";

    private static final String danbriFoaf =
        "http://danbri.livejournal.com/data/foaf";

    /**
     * Crawling seeAlso data in FOAF files turns out to be a very round-about fishing
     * trip. It's slow because you're fetching all sorts of documents from all over the
     * place. For now, treat it as an interesting exercise.<p>
     * If I need something like this in real life, the thing to do is to cache all this
     * data in some kind of overnight crawl, then stow it away in a model graph as a file
     * and reload the file whenever I want it.
     */
    public static void foafCrawl() {
        int howMany = 10;
        GenealogyModel model = GenealogyModel.getInstance();
        model.read(danbriFoaf);

        // Conjoin graphs from many different FOAF docs
        model.crawlSeeAlsos(howMany, new Tester() {
            public boolean test(String uri) {
                return (uri.startsWith("http://") // no funky "../../" URIs
                        && !uri.endsWith("scutterplan.rdf") // broken etags
                        && !uri.endsWith(".rss") // no news feeds, thanks
                       );
            }
        });

        // Let's do a query on this data
        model.query("SELECT ?name ?interest WHERE {\n"
                    + "    ?person " + Interest + " ?interest.\n"
                    + "    ?person " + Name + " ?name }",
        new QueryProcessor() {
            public void process(ResultBinding binding) {
                Literal n = (Literal) binding.get("name");
                Resource i = (Resource) binding.get("interest");
                System.out.println(n + " is interested in " + i);
            }
        });
    }

    public static void main(String[] _args) {
        doStuff(System.out);
    }

    public static void doStuff(JspWriter out) {
        doStuff(new java.io.PrintStream(new net.willware.semweb.WriterOutputStream(out)));
    }

    public static void doStuff(final PrintStream pw) {

        GenealogyModel model = GenealogyModel.getInstance();

        /* What do I get with the OWL reasoner? It should be able to
         * do things like merging people, or at least declaring them
         * to be the same person. For purposes of the genealogy, I
         * don't think I need the OWL reasoner for anything yet.
         */
        if (USE_OWL) {
            Reasoner owlReasoner = ReasonerRegistry.getOWLReasoner();
            model = model.think(owlReasoner);
        } else {
            model = model.think();
        }

        if (NOT_REALLY) {
            foafCrawl();
            if (DUMP_AND_QUIT) {
                model.dump();
                System.exit(0);
            }
        }

        GenealogyModel.Query query = new GenealogyModel.Query(new String[] { "?name1", "?name2" });
        query.addTriplet("?person1", Name, "?name1");
        query.addTriplet("?person2", Name, "?name2");
        query.addTriplet("?person1", Knows, "?person2");
        model.query(query,
        new QueryProcessor() {
            public void process(ResultBinding binding) {
                pw.println((Literal) binding.get("name1")
                                   + " knows "
                                   + (Literal) binding.get("name2"));
            }
        }, pw);

        query = new GenealogyModel.Query(new String[] { "?name", "?date" });
        query.addTriplet("?person", Name, "?name");
        query.addTriplet("?person", Event, "?death");
        query.addTriplet("?death", Rdftype, Death);
        query.addTriplet("?death", Date, "?date");
        model.query(query, new QueryProcessor() {
            private ArrayList<String> alreadySeen = new ArrayList<String>();
            public void process(ResultBinding binding) {
                String name = ((Literal) binding.get("name")).toString();
                String date = ((Literal) binding.get("date")).toString();
                String combo = name + " " + date;
                if (!alreadySeen.contains(combo)) {
                    pw.println(name + " died on " + date);
                    alreadySeen.add(combo);
                }
            }
        }, pw);

        query = new GenealogyModel.Query(new String[] { "?name", "?aname" });
        query.addTriplet("?person", Name, "?name");
        model.query(query, null);

        // For each person with a name and a birthday, show the name and birthday.
        // To identify the birthday, we find an event attached to that person, make
        // sure that it's the person's birth, and then find its date.
        GenealogyModel.Query birthdateQuery =
            new GenealogyModel.Query(new String[] { "?name", "?date" });
        birthdateQuery.addTriplet("?person", Name, "?name");
        birthdateQuery.addTriplet("?person", Event, "?birth");
        birthdateQuery.addTriplet("?birth", Rdftype, Birth);
        birthdateQuery.addTriplet("?birth", Date, "?date");

        model.query(birthdateQuery);

        model.query(birthdateQuery,
        new QueryProcessor() {
            public void process(ResultBinding binding) {
                pw.println(
                    (Literal) binding.get("name")
                    + " was born on "
                    + (Literal) binding.get("date"));
            }
        }, pw);

        model.query("SELECT ?name1 ?name2 ?date\n"
                    + "   WHERE { ?person1 " + Name + " ?name1 .\n"
                    + "           ?person2 " + Name + " ?name2 .\n"
                    + "           ?person1 " + Event + " ?marriage .\n"
                    + "           ?person2 " + Event + " ?marriage .\n"
                    + "           ?marriage " + Rdftype + " " + Marriage + " .\n"
                    + "           ?marriage " + Date + " ?date }",
        new QueryProcessor() {
            public void process(ResultBinding binding) {
                // Print each marriage only once, and don't say a person
                // is married to him- or herself.
                String n1 = ((Literal) binding.get("name1")).getString();
                String n2 = ((Literal) binding.get("name2")).getString();
                if (n1.compareTo(n2) < 0) {
                    pw.println(n1
                                       + " and "
                                       + n2
                                       + " were married on "
                                       + (Literal) binding.get("date"));
                }
            }
        }, pw);

        model.query("SELECT ?eldername ?kidname\n"
                    + "    WHERE { ?elder " + Name + " ?eldername.\n"
                    + "            ?kid " + Name + " ?kidname.\n"
                    + "            ?kid " + Father + " ?elder. }",
        new QueryProcessor() {
            public void process(ResultBinding binding) {
                pw.println((Literal) binding.get("kidname")
                                   + " has father: "
                                   + (Literal) binding.get("eldername"));
            }
        });

        model.query("SELECT ?eldername ?kidname\n"
                    + "    WHERE { ?elder " + Name + " ?eldername.\n"
                    + "            ?kid " + Name + " ?kidname.\n"
                    + "            ?kid " + Mother + " ?elder. }",
        new QueryProcessor() {
            public void process(ResultBinding binding) {
                pw.println((Literal) binding.get("kidname")
                                   + " has mother: "
                                   + (Literal) binding.get("eldername"));
            }
        });

        model.query("SELECT ?eldername ?kidname\n"
                    + "    WHERE { ?elder " + Name + " ?eldername.\n"
                    + "            ?kid " + Name + " ?kidname.\n"
                    + "            ?kid " + Gender + " 'female'.\n"
                    + "            ?kid " + childOf + " ?elder. }",
        new QueryProcessor() {
            public void process(ResultBinding binding) {
                pw.println((Literal) binding.get("eldername")
                                   + " has daughter: "
                                   + (Literal) binding.get("kidname"));
            }
        });

        model.query("SELECT ?eldername ?kidname\n"
                    + "    WHERE { ?elder " + Name + " ?eldername.\n"
                    + "            ?kid " + Name + " ?kidname.\n"
                    + "            ?kid " + Gender + " 'male'.\n"
                    + "            ?kid " + childOf + " ?elder. }",
        new QueryProcessor() {
            public void process(ResultBinding binding) {
                pw.println((Literal) binding.get("eldername")
                                   + " has son: "
                                   + (Literal) binding.get("kidname"));
            }
        });

        model.query("SELECT ?eldername ?kidname\n"
                    + "    WHERE { ?elder " + Name + " ?eldername.\n"
                    + "            ?kid " + Name + " ?kidname;\n"
                    + "                 " + childOf + " ?parent.\n"
                    + "            ?parent " + childOf + " ?elder. }",
        new QueryProcessor() {
            public void process(ResultBinding binding) {
                pw.println((Literal) binding.get("eldername")
                                   + " has grandchild: "
                                   + (Literal) binding.get("kidname"));
            }
        });

        class PeopleList implements QueryProcessor {
            private String name1list;
            private Literal nameLit;
            PeopleList(Model model, String name) {
                nameLit = model.createLiteral(name);
                name1list = "";
            }
            String listOfPeople() {
                // remove the trailing ", " from the end of the list
                int n = name1list.length();
                return name1list.substring(0, n - 2);
            }
            public void process(ResultBinding binding) {
                if (nameLit.equals(binding.get("name2"))) {
                    name1list += ((Literal) binding.get("name1")).getString()
                                 + ", ";
                }
            }
        };

        PeopleList plist = new PeopleList(model, "Sue Bauter");
        model.query("SELECT ?name1 ?name2\n"
                    + "    WHERE { ?x " + Knows + " ?y.\n"
                    + "            ?x " + Name + " ?name1.\n"
                    + "            ?y " + Name + " ?name2 }",
                    plist);
        pw.println(plist.listOfPeople() + " all are people who know SueB");
        pw.println("fini");
    }

    static Model inferenceFun(Model model) {

        /* p and q are properties, p is a subproperty of q, and a has
         * p-property "foo", therefore a has q-property "foo" as well.
         */
        String NS = "urn:x-hp-jena:eg/"; // is this arbitrary?
        Property p = model.createProperty(NS, "p");
        Property q = model.createProperty(NS, "q");
        model.add(p, RDFS.subPropertyOf, q);
        model.createResource(NS + "a").addProperty(p, "foo");

        /*
         * The conclusion "a q foo" is to be concluded by an inference
         * engine. This requires an InfModel.
         */
        InfModel inf = ModelFactory.createRDFSModel(model);
        Resource a = inf.getResource(NS + "a");
        System.out.println("a has q-property: " + a.getProperty(q).getObject());
        return inf;
    }
}
