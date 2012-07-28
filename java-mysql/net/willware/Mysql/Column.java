package net.willware.Mysql;

import java.sql.ResultSet;
import java.sql.SQLException;

public class Column {
	private String name;
	private String mysqlType;
	private boolean primaryKey;
	Column(ResultSet rs) throws SQLException {
		name = rs.getString(1);
    	mysqlType = rs.getString(2);
    	primaryKey = "PRI".equals(rs.getString(4));
	}
	public String toString() {
		String r = "Column<\"" + name + "\": " + mysqlType;
		if (primaryKey)
			r += ", primary key";
		return r + ">";
	}
	public String getName() {
		return name;
	}
	public boolean isPrimaryKey() {
		return primaryKey;
	}
	public String getType() {
		return mysqlType;
	}
}
