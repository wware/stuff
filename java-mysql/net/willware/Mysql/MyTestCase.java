package net.willware.Mysql;

import java.util.ArrayList;
import java.sql.SQLException;

import junit.framework.AssertionFailedError;
import junit.framework.TestCase;
import junit.framework.TestSuite;
import junit.textui.TestRunner;

public class MyTestCase extends TestCase {
	private Schema schema;

	private Row sol;
    private Row earth;
    private Row mars;
    private Row jupiter;
    private Row luna;
    private Row phobos;
    private Row deimos;
    private Row io;
    private Row europa;
    private Row ganymede;

	private Table body;
	private Table orbit;
	private Table userdata;

	private Row getBody(String name) {
		final java.util.ArrayList<Row> rows = new java.util.ArrayList<Row>();
		try {
			schema.getTable("body").processRows(
					"name=\"" + name + "\"", new Row.Processor() {
				@Override
				public void process(Row r) {
					rows.add(r);
				}
			});
		} catch (SQLException e) {
			throw new RuntimeException(e);
		}
		assertEquals(1, rows.size());
		return rows.get(0);
	}

	public void setUp() {
		try {
	        schema = Schema.getInstance("jdbc:mysql://localhost/javatest", "root", "");
		} catch (SQLException e) {
			throw new RuntimeException(e);
		}
		sol = this.getBody("Sol");
        earth = this.getBody("Earth");
        mars = this.getBody("Mars");
        jupiter = this.getBody("Jupiter");
        luna = this.getBody("Luna");
        phobos = this.getBody("Phobos");
        deimos = this.getBody("Deimos");
        io = this.getBody("Io");
        europa = this.getBody("Europa");
        ganymede = this.getBody("Ganymede");
		body = schema.getTable("body");
		orbit = schema.getTable("orbit");
		userdata = schema.getTable("userdata");
	}

	public void testTableAttributes() {
		assertEquals("body", body.getName());
		ArrayList<String> lst = new ArrayList<String>();
		lst.add("id");
		lst.add("name");
		assertEquals(lst, body.getColumnNames());
		lst = new ArrayList<String>();
		lst.add("id");
		assertEquals(lst, body.getKeyNames());
		assertEquals("id", body.getPrimaryKey());
		assertEquals("orbit", orbit.getName());
		lst = new ArrayList<String>();
		lst.add("big_id");
		lst.add("small_id");
		assertEquals(lst, orbit.getColumnNames());
	}

	public void testPlanets() {
		try {
			ArrayList<Row> planets = getSatellites(sol);
			for (String pname : new String[] {"Mercury", "Venus", "Earth", "Mars", "Jupiter",
					"Saturn", "Uranus", "Neptune", "Pluto"}) {
				Row x = body.getUniqueBy("name", pname);
				assertTrue(planets.contains(x));
			}
		} catch (SQLException e) {
			throw new AssertionFailedError(e.toString());
		}
	}
	
	private ArrayList<Row> getSatellites(Row center) throws SQLException {
		return center.dereference("big_id", orbit, "small_id", body);
	}

	public void testMoons() {
		try {
			ArrayList<Row> lst = new ArrayList<Row>();
			lst.add(luna);
			assertEquals(lst, getSatellites(earth));
			lst = new ArrayList<Row>();
			lst.add(phobos);
			lst.add(deimos);
			assertEquals(lst, getSatellites(mars));
			lst = new ArrayList<Row>();
			lst.add(io);
			lst.add(europa);
			lst.add(ganymede);
			assertEquals(lst, getSatellites(jupiter));
		} catch (SQLException e) {
			throw new AssertionFailedError(e.toString());
		}
	}

	public void testMultiplePrimaryKeys() {
		try {
			ArrayList<String> keynames = new ArrayList<String>();
			keynames.add("userid");
			keynames.add("userdataid");
			assertEquals(keynames, userdata.getPrimaryKey());
			ArrayList<Object> keyvalues = new ArrayList<Object>();
			keyvalues.add(1);
			keyvalues.add(1);
			Row r = userdata.get(keyvalues);
			assertEquals("Alpha", r.get("info"));
		} catch (SQLException e) {
			throw new AssertionFailedError(e.toString());
		}
	}

	public static void main(String[] args) throws Exception {
		Class.forName("com.mysql.jdbc.Driver");
        TestRunner.run(new TestSuite(MyTestCase.class));
	}
}
