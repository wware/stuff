package net.willware.Mysql;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.sql.Connection;

public class SqlProxy {
	private Connection conn;
	public Connection getConnection() {
		return conn;
	}
	public SqlProxy(Connection conn) {
		this.conn = conn;
	}
	public interface Processor {
		public void process(SqlProxy proxy, ResultSet rs) throws SQLException;
	}
	public void processSql(String sql, Processor sproc) throws SQLException {
		ResultSet rs;
	    Statement stmt = null;
	    try {
	        stmt = (Statement) conn.createStatement();
        	rs = stmt.executeQuery(sql);
	        while (rs.next()) {
	        	sproc.process(this, rs);
	        }
	    } catch (SQLException e) {
	    	throw e;
	    } finally {
	    	if (stmt != null) {
	    	    try {
		    		stmt.close();
	    	    } catch (SQLException e) {
	    	    	throw e;
	    	    }
	    	}
	    }
	}
}
