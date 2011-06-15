/* -*- mode: java; c-basic-offset: 4; indent-tabs-mode: nil -*- */

package net.willware.semweb;

import org.xml.sax.Attributes;
import org.xml.sax.InputSource;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.DefaultHandler;
import org.xml.sax.helpers.XMLReaderFactory;

import com.hp.hpl.jena.graph.Triple;
import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.ModelFactory;
import com.hp.hpl.jena.rdf.model.Property;
import com.hp.hpl.jena.rdf.model.Resource;
import com.hp.hpl.jena.util.iterator.ExtendedIterator;

public class Airport {
    static final boolean DEBUG = false;
    static final String base = "http://www.airnav.com/airport/K";

    interface TripleTester {
        boolean test(Triple tr);
    }

    static void blabModel(Model model, TripleTester tt) {
        if (tt == null) {
            tt = new TripleTester() {
                public boolean test(Triple tr) {
                    return true;
                }
            };
        }
        ExtendedIterator<Triple> iter = model.getGraph().find(null, null, null);
        while (iter.hasNext()) {
            Triple tr = iter.next();
            if (tt.test(tr)) {
                System.out.println("" + tr.getSubject() +
                        " " + tr.getPredicate() +
                        " " + tr.getObject());
//                System.out.println("" + tr.getSubject().hashCode() +
//                        " " + tr.getPredicate().hashCode() +
//                        " " + tr.getObject().hashCode());
            }
        }
    }

    static class AirportHandler extends DefaultHandler {
        String airportName, lat, lon;
        public void startElement(String uri, String localName, String qName,
                Attributes attributes) {
            if ("a".equals(localName) && attributes.getLength() == 3) {
                String googleMapsUrl = attributes.getValue("href");
                if (googleMapsUrl != null && googleMapsUrl.startsWith("http://maps.google.com/")) {
                    int a = googleMapsUrl.indexOf("?ll=");
                    int b = googleMapsUrl.indexOf("%2C");
                    int c = googleMapsUrl.indexOf("&spn=");
                    this.lat = googleMapsUrl.substring(a+4, b);
                    this.lon = googleMapsUrl.substring(b+3, c);
                }
            }
            if (DEBUG) {
                System.out.println("localName = " + localName);
                int n = attributes.getLength();
                for (int i = 0; i < n; i++) {
                    System.out.println("    " + attributes.getLocalName(i) + " "
                            + attributes.getValue(i));
                }
            }
        }

        public void endElement(String uri, String localName, String qName)  {
        }

        private boolean firstData = true;

        public void characters(char[] ch, int start, int length) {
            String x = new String(ch).substring(start, start+length).trim();
            if (x != null && x.length() > 0) {
                if (firstData) {
                    int n = x.indexOf(" - ");
                    this.airportName = x.substring(n + 3);
                    firstData = false;
                }
                if (DEBUG) {
                    System.out.println("DATA: " + x);
                }
            }
        }
    }

    static class AirportOntology {
        Property name, location, latitude, longitude, elevation, iataCode, icaoCode;
        Model airport_ont;
        public AirportOntology() {
            airport_ont = ModelFactory.createDefaultModel();
            airport_ont.read("http://www.daml.org/2001/10/html/airport-ont");

            if (DEBUG) {
                blabModel(airport_ont, new TripleTester() {
                    public boolean test(Triple tr) {
                        String pred = tr.getPredicate().toString();
                        return "http://www.w3.org/2002/07/owl#onProperty".equals(pred);
                    }
                });
                System.out.println();
            }

            this.name = airport_ont.getProperty("http://www.daml.org/2001/10/html/airport-ont#name");
            this.location = airport_ont.getProperty("http://www.daml.org/2001/10/html/airport-ont#location");
            this.latitude = airport_ont.getProperty("http://www.daml.org/2001/10/html/airport-ont#latitude");
            this.longitude = airport_ont.getProperty("http://www.daml.org/2001/10/html/airport-ont#longitude");
            this.elevation = airport_ont.getProperty("http://www.daml.org/2001/10/html/airport-ont#elevation");
            this.iataCode = airport_ont.getProperty("http://www.daml.org/2001/10/html/airport-ont#iataCode");
            this.icaoCode = airport_ont.getProperty("http://www.daml.org/2001/10/html/airport-ont#icaoCode");
        }
        public Model getModel() {
            return airport_ont;
        }
    }

    private static AirportOntology airport_ont = new AirportOntology();

    public static Model getModel(String symbol) {
        Model model = airport_ont.getModel();
        return getModel(symbol, model);
    }

    public static Model getModel(String symbol, Model model) {
        try {
            String url = base + symbol.toUpperCase();
            // handle ugly messy HTML
            XMLReader xr = XMLReaderFactory.createXMLReader("org.ccil.cowan.tagsoup.Parser");
            AirportHandler handler = new AirportHandler();
            xr.setContentHandler(handler);
            xr.setErrorHandler(handler);
            InputSource is = new InputSource((new java.net.URL(url))
                    .openStream());
            xr.parse(is);

            Resource airportResource = model.createResource("Airport" + symbol);
            model.add(airportResource, airport_ont.name, handler.airportName);
            model.add(airportResource, airport_ont.latitude, handler.lat);
            model.add(airportResource, airport_ont.longitude, handler.lon);
            model.add(airportResource, airport_ont.icaoCode, "K" + symbol.toUpperCase());
            model.add(airportResource, airport_ont.iataCode, symbol.toUpperCase());
            // Some day, do location
        } catch (Exception e) {
            e.printStackTrace();
        }
        return model;
    }

    public static void main(String args[]) {
        Model model = Airport.getModel("LAX");
        model = Airport.getModel("BOS", model);
        blabModel(model, null);
    }
}
