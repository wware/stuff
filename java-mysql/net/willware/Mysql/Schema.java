package net.willware.Mysql;

import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.Map;

import com.mysql.jdbc.Connection;

public class Schema implements SqlProxy.Processor {
	private Map<String,Table> tables;
	private SqlProxy proxy;
	/**
	 * Get a list of the tables in the database. Run the {@link Table#Table(SqlProxy, String)}
	 * constructor on each to build a schema for that table. Store all the table instances in
	 * a HashMap, keyed by the table names.
	 * @param db the name of the database being examined
	 * @param user MySQL user name
	 * @param pw MySQL password
	 * @throws SQLException
	 */
	private Schema(String db, String user, String pw) throws SQLException {
        Connection conn = (Connection) DriverManager.getConnection(db, user, pw);
        proxy = new SqlProxy(conn);
		this.tables = new HashMap<String,Table>();
	    proxy.processSql("show tables", this);
	}
	
	/**
	 * Return the {@link Table} instance for a given table name
	 * @param table name
	 * @return the Table instance
	 */
	public Table getTable(String tablename) {
		return tables.get(tablename);
	}

	/**
	 * For each table in the database, run the {@link Table} constructor to get
	 * the schema for that table. Store each Table in a HashMap.
	 */
	@Override
	public void process(SqlProxy proxy, ResultSet rs) throws SQLException {
		Table t = new Table(proxy, rs.getString(1));
		tables.put(t.getName(), t);
	}

	private static Schema instance = null;
	/**
	 * Given the database name, MySQL username and password, get a schema for this database.
	 * This is the correct way to initialize this entire package.
	 * @param db the name of the database being examined
	 * @param user MySQL user name
	 * @param pw MySQL password
	 * @return
	 * @throws SQLException
	 */
	public static Schema getInstance(String db, String user, String pw) throws SQLException {
		if (instance == null) {
	        instance = new Schema(db, user, pw);
		}
		return instance;
	}

	public SqlProxy getProxy() {
		return proxy;
	}
}
