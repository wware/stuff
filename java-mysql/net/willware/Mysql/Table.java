package net.willware.Mysql;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.List;
import java.util.ArrayList;

/**
 * A Java representation of a MySQL table.
 */
public class Table {
	private String name;
	private SqlProxy proxy;
	private ArrayList<Column> columns;
	private ArrayList<String> keyNames;

	/**
	 * The Table constructor fetches the column definitions for the table from
	 * the database, so that it can build a list of {@link Column}s that provides
	 * a schema for this table.
	 * @param proxy needed for MySQL communication
	 * @param name the name of the table
	 * @throws SQLException
	 */
	public Table(SqlProxy proxy, String name) throws SQLException {
		this.proxy = proxy;
		this.name = name;
	    final ArrayList<Column> fcolumns = new ArrayList<Column>();
	    proxy.processSql("describe " + name, new SqlProxy.Processor() {
			@Override
			public void process(SqlProxy proxy, ResultSet rs) throws SQLException {
	        	fcolumns.add(new Column(rs));
			}
	    });
	    columns = fcolumns;
		keyNames = new ArrayList<String>();
		for (Column c : columns)
			if (c.isPrimaryKey())
				keyNames.add(c.getName());
	}

	/**
	 * Get a list of {@link Column} instances that form the schema for this table.
	 * @return a list of Column instances
	 */
	@SuppressWarnings("unchecked")
	public List<Column> getColumns() {
		return (List<Column>) columns.clone();
	}

	/**
	 * Get a list of all the column names for this table.
	 * @return a list of column names
	 */
	public List<String> getColumnNames() {
		ArrayList<String> result = new ArrayList<String>();
		for (Column c : columns)
			result.add(c.getName());
		return result;
	}

	/**
	 * Get a list of the primary keys for this table.
	 * @return a list of column names for the columns that are private keys
	 */
	@SuppressWarnings("unchecked")
	public List<String> getKeyNames() {
		return (List<String>) keyNames.clone();
	}

	/**
	 * Given a column name and a value, return an ArrayList<Row> containing all
	 * the rows whose columns of that name match that value.
	 * @param colname the column name
	 * @param value the value to be matched
	 * @return the list of matching rows
	 * @throws SQLException
	 */
	public ArrayList<Row> getBy(String colname, Object value) throws SQLException {
		final ArrayList<Row> results = new ArrayList<Row>();
		if (value instanceof String)
			value = "\"" + value + "\"";
		processRows(colname + "=" + value, new Row.Processor() {
			@Override
			public void process(Row r) throws SQLException {
				results.add(r);
			}
		});
		return results;
	}

	/**
	 * Behaves just like {@link #getBy(String, Object)} but instead of returning an
	 * ArrayList<Row>, it returns the single matching row. If there is not exactly one
	 * matching row, throw a RuntimeException.
	 * @param colname the column name
	 * @param value the value to match
	 * @return the matching row
	 * @throws SQLException
	 */
	public Row getUniqueBy(String colname, Object value) throws SQLException {
		final ArrayList<Row> results = getBy(colname, value);
		if (results.size() != 1)
			throw new SQLException(getName() + ".get(" + colname + "=" + value + ") should " +
					"produce exactly one result");
		return results.get(0);
	}

	/**
	 * Given a value, fetch the row with that value as its primary key, assuming that (1) the
	 * table has exactly one primary key and (2) exactly one row has that primary key value. 
	 * @param value the primary key value
	 * @return the selected row
	 * @throws SQLException
	 */
	public Row get(Object value) throws SQLException {
		ArrayList<Object> lst = new ArrayList<Object>();
		lst.add(value);
		return get(lst);
	}

	/**
	 * If this table is only one primary key, return a String. If the table has multiple primary
	 * keys, return an ArrayList<String>. If there are no primary keys, return null.
	 * @return either a String or ArrayList<String>
	 */
	public Object getPrimaryKey() {
		List<String> result = new ArrayList<String>();
		int n = 0;
		for (Column c : columns)
			if (c.isPrimaryKey()) {
				result.add(c.getName());
				n++;
			}
		if (n == 0)
			return null;
		if (n == 1)
			return result.get(0);
		return result;
	}

	/**
	 * Given an SQL WHERE clause, process the selected rows of the table. The WHERE clause makes this method
	 * preferable to {@link Table#processRows(Row.Processor)} because the database
	 * is performing the selection, not the Java code.
	 * @param whereClause a selection of rows to be processed
	 * @param rproc a processor for the rows
	 * @throws SQLException
	 */
	public void processRows(String whereClause, final Row.Processor rproc) throws SQLException {
		proxy.processSql("SELECT * FROM " + name + " WHERE " + whereClause, new SqlProxy.Processor() {
			@Override
			public void process(SqlProxy proxy, ResultSet rs) throws SQLException {
	        	rproc.process(RowFactory.getInstance().makeRow(Table.this, rs));
			}
		});
	}
	/**
	 * This is likely to be less efficient than {@link #processRows(String, Row.Processor)}
	 * because there is no opportunity here for a WHERE clause, which means that we will step through every
	 * row of the table, create a {@link Row} for it, and process that row.
	 * @param rproc
	 * @throws SQLException
	 */
	public void processRows(final Row.Processor rproc) throws SQLException {
		proxy.processSql("SELECT * FROM " + name, new SqlProxy.Processor() {
			@Override
			public void process(SqlProxy proxy, ResultSet rs) throws SQLException {
	        	rproc.process(RowFactory.getInstance().makeRow(Table.this, rs));
			}
		});
	}

	/**
	 * Tell how many columns this table has.
	 * @return the number of columns
	 */
	public int numColumns() {
		return columns.size();
	}

	/**
	 * Get the name of this table
	 * @return the table's name
	 */
	public String getName() {
		return name;
	}

	/**
	 * Given a list of objects representing the values of this table's primary
	 * keys, fetch the table row with that set of primary key values. Throw an
	 * SQLException if the row does not exist or is not unique, or if the table
	 * has no primary keys, or if it's the wrong number of values.
	 * @param keyvalues the list of objects
	 * @return the set of rows
	 * @throws SQLException
	 */
	public Row get(ArrayList<Object> keyvalues) throws SQLException {
		if (keyNames.size() == 0)
			throw new SQLException(name + " has no primary keys");
		if (keyvalues.size() != keyNames.size())
			throw new SQLException(name + " wrong size primary key value " + keyvalues);
		if (keyNames.size() == 1)
			return getUniqueBy(keyNames.get(0), keyvalues.get(0));
		String whereClause = "";
		boolean first = true;
		for (int i = 0; i < keyNames.size(); i++) {
			if (!first)
				whereClause += " AND ";
			first = false;
			whereClause += keyNames.get(i) + "=";
			Object value = keyvalues.get(i);
			if (value instanceof String)
				value = "\"" + value + "\"";
			whereClause += value;
		}
		final ArrayList<Row> results = new ArrayList<Row>();
		processRows(whereClause, new Row.Processor() {
			@Override
			public void process(Row r) throws SQLException {
				results.add(r);
			}
		});
		if (results.size() != 1)
			throw new SQLException(this.getName() +
					" should have exactly one matching row, not " + results.size());
		return results.get(0);
	}
}
