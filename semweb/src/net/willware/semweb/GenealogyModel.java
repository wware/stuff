/* -*- mode: java; c-basic-offset: 4; indent-tabs-mode: nil -*- */

package net.willware.semweb;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.io.PrintStream;

import com.hp.hpl.jena.graph.Graph;
import com.hp.hpl.jena.graph.Node;
import com.hp.hpl.jena.graph.Triple;
import com.hp.hpl.jena.query.QueryExecution;
import com.hp.hpl.jena.query.QueryExecutionFactory;
import com.hp.hpl.jena.query.QueryFactory;
import com.hp.hpl.jena.query.ResultSet;
import com.hp.hpl.jena.query.ResultSetFormatter;
import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.ModelFactory;
import com.hp.hpl.jena.rdf.model.Resource;
import com.hp.hpl.jena.rdf.model.StmtIterator;
import com.hp.hpl.jena.rdf.model.impl.InfModelImpl;
import com.hp.hpl.jena.reasoner.InfGraph;
import com.hp.hpl.jena.reasoner.Reasoner;
import com.hp.hpl.jena.reasoner.TriplePattern;
//import com.hp.hpl.jena.reasoner.ReasonerRegistry;
import com.hp.hpl.jena.reasoner.ValidityReport;
import com.hp.hpl.jena.reasoner.ValidityReport.Report;
import com.hp.hpl.jena.reasoner.rulesys.GenericRuleReasoner;
import com.hp.hpl.jena.reasoner.rulesys.Rule;
import com.hp.hpl.jena.sparql.core.ResultBinding;
import com.hp.hpl.jena.util.PrintUtil;
import com.hp.hpl.jena.util.iterator.ExtendedIterator;

public class GenealogyModel extends InfModelImpl {

    private static final boolean verbose = true;
    private static boolean USE_RETE = true;

    public static final String waresRdf = "file:///home/wware/stuff/semweb/wares.rdf";

    /**
     * This is just a convenience class for packaging up queries in a
     * way that's a little less cumbersome. Because it's a little more
     * programmatic, it might turn out to be helpful in bigger ways later.
     */
    public static class Query {
        private List<TriplePattern> lst = new ArrayList<TriplePattern>();
        private String[] varList;
        public Query(String[] variables) {
            varList = variables;
        }
        public void addTriplet(String s, String p, String o) {
            Node sNode, pNode, oNode;
            if (s.startsWith("http://"))
                sNode = Node.createURI(s);
            else
                sNode = Node.createLiteral(s);
            if (p.startsWith("http://"))
                pNode = Node.createURI(p);
            else
                pNode = Node.createLiteral(p);
            if (o.startsWith("http://"))
                oNode = Node.createURI(o);
            else
                oNode = Node.createLiteral(o);
            lst.add(new TriplePattern(sNode, pNode, oNode));
        }
        private String unquoted(Node n) {
            // lose the double quotes on the ends
            String s = n.toString();
            return s.substring(1, s.length() - 1);
        }
        public String toString() {
            String r = "SELECT";
            for (String varname : varList)
                r += " " + varname;
            r += "\nWHERE {\n";
            int n = lst.size();
            for (int i = 0; i < n; i++) {
                TriplePattern tp = lst.get(i);
                r += "    ";
                r += unquoted(tp.getSubject()) + " ";
                r += unquoted(tp.getPredicate()) + " ";
                r += unquoted(tp.getObject());
                r += " .\n";
            }
            return r + "}";
        }
    }

    static GenealogyModel getInstance() {

        /* Let's make up some arbitrary rules. First we need to register some prefixes.
         * http://jena.sourceforge.net/javadoc/com/hp/hpl/jena/reasoner/rulesys/Rule.html
         *
         * This is where we get to tinker with some inference engine stuff and make
         * some deductions.
         */
        PrintUtil.registerPrefix("wares", waresRdf + "/");
        PrintUtil.registerPrefix("foaf", "http://xmlns.com/foaf/0.1/");
        PrintUtil.registerPrefix("bio", "http://purl.org/vocab/bio/0.1/");
        PrintUtil.registerPrefix("rel", "http://purl.org/vocab/relationship/");
        PrintUtil.registerPrefix("rdf", "http://www.w3.org/1999/02/22-rdf-syntax-ns#");
        PrintUtil.registerPrefix("rdfs", "http://www.w3.org/2000/01/rdf-schema#");
        String rules =
            "[rule1: (?x bio:father ?y)"
            + " -> (?y bio:child ?x)]"
            + "[rule2: (?x bio:mother ?y)"
            + " -> (?y bio:child ?x)]"
            + "[rule3: (?x bio:child ?y)"
            + " -> (?y rel:childOf ?x)]"
            + "[rule4: (?x rel:childOf ?y)"
            + " -> (?y bio:child ?x)]"
            ;
        GenericRuleReasoner ruleReasoner =
            new GenericRuleReasoner(Rule.parseRules(rules));
        if (USE_RETE)
            ruleReasoner.setMode(GenericRuleReasoner.FORWARD_RETE);

        // Create a generic model, populate it with some genealogical graph stuff.
        Model model = ModelFactory.createDefaultModel();
        //model.read(familyRdf);
        model.read(waresRdf);
        InfGraph graph = ruleReasoner.bind(model.getGraph());
        GenealogyModel gm = new GenealogyModel(graph);
        return gm.think(ruleReasoner);
    }

    private GenealogyModel(InfGraph graph) {
        super(graph);
    }

    public void mergeFrom(Model otherguy) {
        Graph g = getGraph();
        ExtendedIterator<Triple> iter = otherguy.getGraph().find(null, null, null);
        while (iter.hasNext()) {
            g.add(iter.next());
        }
    }

    /**
     * Dump all the triplet statements in the model to standard output in
     * human-readable form, subject-predicate-object. Format them as
     * appropriate to make as much sense as possible.
     */
    public void dump() {
        dump(this);
    }

    private static void dump(Triple tr) {
        Node subject = tr.getSubject();
        Node predicate = tr.getPredicate(); // get the predicate
        Node object = tr.getObject(); // get the object
        String suri = subject.getURI();
        System.out.print(((suri != null) ? suri : subject) +
        		" " + predicate + " " +
        		((object instanceof Resource) ? object : "\"" + object.toString() + "\"") +
        		" .");
    }

    public static void dump(Graph graph) {
        ExtendedIterator<Triple> iter = graph.find(null, null, null);
        int n = 0;
        // print out the predicate, subject and object of each statement
        while (iter.hasNext()) {
            n++;
            dump(iter.next());
        }
        System.out.println("There were " + n + " triplets.");
    }
    
    public static void dump(Model model) {
        StmtIterator iter = model.listStatements();
        int n = 0;
        while (iter.hasNext()) {
            n++;
            dump(iter.nextStatement().asTriple());
        }
        System.out.println("There were " + n + " triplets.");
        System.out.println("====================================");
    }

    /**
     * Query the model for situations satisfying the queryString and print them
     * in a sort of SQL-response kind of way.
     * @param queryString a SPARQL query to run against the model
     */
    public void query(String queryString) {
        query(queryString, null, System.out);
    }

    /**
     */
    public void query(Query wq) {
        query(wq, null, System.out);
    }

    /**
     */
    public void query(String s, QueryProcessor prb) {
        query(s, prb, System.out);
    }

    /**
     */
    public void query(Query wq, QueryProcessor prb) {
        query(wq.toString(), prb, System.out);
    }

    /**
     */
    public void query(Query wq, QueryProcessor prb, PrintStream pw) {
        query(wq.toString(), prb, pw);
    }

    /**
     * Query the model for situations satisfying the queryString (which might be
     * something like "W, whose name is X, is the father of Y, whose name is Z")
     * and for each result, run the processor which specifies an action to be done
     * (something like print the string "X is the father of Z").
     * @param queryString a SPARQL query to run against the model
     * @param prb a QueryProcessor telling what to do with query results
     */
    public void query(String queryString, QueryProcessor prb, PrintStream pw) {
        if (verbose)
            pw.println("SPARQL query: " + queryString);
        com.hp.hpl.jena.query.Query query = QueryFactory.create(queryString);
        QueryExecution qe = QueryExecutionFactory.create(query, this);
        ResultSet results = qe.execSelect();
        if (prb == null) {
            ResultSetFormatter.out(pw, results, query);
        } else {
            while (results.hasNext()) {
                prb.process((ResultBinding) results.next());
            }
        }
        qe.close();
    }

    /**
     * Sift through the current model looking for seeAlso documents,
     * put them all into a list and then fetch those documents to grow
     * the model. Put a limit on the size of the list of seeAlso docs.
     *
     * @param maxcount the maximum size of the seeAlso doc list
     * @param tester a Tester which tests URIs for inclusion
     */
    public void crawlSeeAlsos(int maxcount, Tester tester) {
        final ArrayList<String> seeAlsoList = new ArrayList<String>();
        int startingPoint = 0;
        class SpiderHelp implements QueryProcessor {
            private Tester testClosure;
            SpiderHelp(Tester tc) {
                testClosure = tc;
            }
            public void process(ResultBinding rb) {
                String str = rb.get("otherdoc").toString();
                if (!seeAlsoList.contains(str) && testClosure.test(str)) {
                    if (verbose)
                        System.out.println("adding " + str);
                    seeAlsoList.add(str);
                }
            }
        };
        SpiderHelp spiderHelp = new SpiderHelp(tester);
        final String seeAlsoUri =
            "http://www.w3.org/2000/01/rdf-schema#seeAlso";
        while (true) {
            if (seeAlsoList.size() < maxcount) {
                // Scan the model to pick up new docs
                query("SELECT ?otherdoc\n"
                      + "WHERE  { ?x <" + seeAlsoUri + "> ?otherdoc }",
                      spiderHelp);
            }
            if (startingPoint >= maxcount || startingPoint >= seeAlsoList.size())
                return;
            // pull in stuff from the next in line
            String nextFoaf = (String) seeAlsoList.get(startingPoint++);
            if (verbose)
                System.out.println("reading " + nextFoaf);
            try {
                read(nextFoaf);
            } catch (Exception e) {
            }
        }
    }

    private Reasoner reasoner;

    public GenealogyModel think() {
        return think(reasoner);
    }

    public GenealogyModel think(Reasoner reasoner) {
        InfGraph graph = reasoner.bind(getGraph());
        graph.prepare();
        graph.getDeductionsGraph();
        GenealogyModel gm = new GenealogyModel(graph);
        gm.reasoner = reasoner;
        return gm;
    }

    public ValidityReport validate() {
        ValidityReport validity = super.validate();
        if (validity.isValid()) {
            System.out.println("This model is consistent");
        } else {
            System.out.println("Conflicts in model");
            for (Iterator<Report> i = validity.getReports(); i.hasNext();) {
                System.out.println(" - " + i.next());
            }
        }
        return validity;
    }
}
