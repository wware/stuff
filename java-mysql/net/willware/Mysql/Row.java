package net.willware.Mysql;

import java.sql.SQLException;
import java.util.HashMap;
import java.util.Map;
import java.util.List;
import java.util.ArrayList;

/**
 * A Java representation of a row in a MySQL table.
 */
public class Row {
	/**
	 * Use this interface to process rows in a table. Pass it to one of these methods:
	 * {@link Table#processRows(Row.Processor)} or
	 * {@link Table#processRows(String, Row.Processor)}. The latter is usually the
	 * preferred choice because you can include a WHERE clause in the SQL string so that
	 * you're not processing every row of the table.
	 */
	public interface Processor {
		/**
		 * Given a row in a table, perform some operation on it. It will often make sense
		 * to perform a quick test at the beginning to determine whether the row should
		 * be processed, although it's better if you can use a SQL WHERE clause to filter
		 * the rows.
		 * @param r
		 * @throws SQLException 
		 */
		public void process(Row r) throws SQLException;
	}

	/** Which Table owns this row */
	private Table table;
	/** Map column names to column values */
	private Map<String,Object> map;

	/**
	 * The constructor is package-scoped. At least for now, database content is read-only
	 * so there is no need for rows to be created outside the package.
	 * @param table the {@link Table} to which this row belongs
	 */
	protected Row(Table table) {
		this.table = table;
		this.map = new HashMap<String,Object>();
	}
	
	@SuppressWarnings("unchecked")
	public String toString() {
		if (table.getKeyNames().size() == 0) {
			// if there is no primary key, just show all the columns
			String r = "Row(" + table.getName() + ")<";
			for (Column c : table.getColumns()) {
				if (c.getType().startsWith("varchar"))
					r += c.getName() + "=\"" + map.get(c.getName()) + "\"";
				else
					r += c.getName() + "=" + map.get(c.getName());
			}
			return r + ">";
		}
		Object keyValue = getKey();
		String keyStr = "";
		if (table.getKeyNames().size() == 1) {
			keyStr = keyValue.toString();
		} else {
			boolean first = true;
			for (Object obj : (ArrayList<Object>)keyValue) {
				if (!first) keyStr += ",";
				first = false;
				keyStr += obj;
			}
		}
		return table.getName() + "[" + keyStr + "]";
	}

	public Table getTable() {
		return table;
	}
	
	public Map<String,Object> getMap() {
		return map;
	}

	public Object get(String s) {
		return map.get(s);
	}
	
	public Object getKey() {
		List<String> keyNames = table.getKeyNames();
		if (keyNames.size() == 0)
			throw new RuntimeException(table.getName() + " has no primary key");
		if (keyNames.size() == 1)
			return get(keyNames.get(0));
		ArrayList<Object> lst = new ArrayList<Object>();
		for (String keyname : keyNames)
			lst.add(get(keyname));
		return lst;
	}

	/**
	 * This works only if the source and target tables both have EXACTLY ONE primary key.
	 */
	private class DerefRproc implements Row.Processor {
		private Table connectingTable;
		private Table targetTable;
		private String leftKey;
		private String rightKey;
		private String targetKey;
		private ArrayList<Object> rightKeyValues;
		public DerefRproc(Table connectingTable, Table targetTable,
				String leftKey, String rightKey, String targetKey) {
			this.connectingTable = connectingTable;
			this.targetTable = targetTable;
			this.leftKey = leftKey;
			this.rightKey = rightKey;
			this.targetKey = targetKey;
			rightKeyValues = new ArrayList<Object>();
		}
		@Override
		public void process(Row r) throws SQLException {
			rightKeyValues.add(r.get(rightKey));
		}
		public ArrayList<Row> getResults() throws SQLException {
			ArrayList<Row> results = new ArrayList<Row>();
			try {
				connectingTable.processRows(leftKey + "=" + getKey(), this);
				for (Object keyValue : rightKeyValues) {
					results.addAll(targetTable.getBy(targetKey, keyValue));
				}
			} catch (SQLException e) {
				throw e;
			} catch (Exception e) {
				throw new SQLException(e);
			}
			return results;
		}
	};

	/**
	 * Rows are equal if they are from the same table and have all the same
	 * column values.
	 */
	public boolean equals(Object other) {
		if (!(other instanceof Row))
			return false;
		Row rother = (Row) other;
		if (table != rother.table)
			return false;
		for (String colname : table.getColumnNames()) {
			Object colvalue = get(colname);
			Object othervalue = rother.get(colname);
			if (colvalue == null) {
				if (othervalue != null)
					return false;
			} else {
				if (!colvalue.equals(othervalue))
					return false;
			}
		}
		return true;
	}

	/**
	 * In our database schema, there are {@link Table}s whose job is to connect together
	 * {@link Row}s in two other tables. This is a convenience method to traverse that link,
	 * which can result in zero, one, or multiple results. Given a two-column connecting table
	 * called "a_to_b" with columns named "a_id" (the primary key for table "a") and
	 * "b_some_column" which maps to column "xyzzy" in table "b", you would get all the
	 * "b" rows connected to an "a" row by writing:
	 * <pre>
	 *   List<Row> bRows = aRow.dereference("a_id", a_to_b, "b_some_column", b, "xyzzy");
	 * </pre> 
	 * @param leftKey the name of the column in the connecting table that has primary key
	 *   values in the table to which this row belongs
	 * @param connectingTable
	 * @param rightKey the name of the column in the connecting table that has primary key
	 *   values in the target table ("b" in this example)
	 * @param targetTable
	 * @param targetKey a column name in the target table (need not be a primary key)
	 * @return a List of Rows
	 * @throws SQLException if anything goes wrong
	 */
	public ArrayList<Row> dereference(
			String leftKey, Table connectingTable,
			String rightKey, Table targetTable, String targetKey) throws SQLException {
		return new DerefRproc(connectingTable, targetTable,
				leftKey, rightKey, targetKey).getResults();
	}

	/**
	 * In our database schema, there are {@link Table}s whose job is to connect together
	 * {@link Row}s in two other tables. This is a convenience method to traverse that link,
	 * which can result in zero, one, or multiple results. Given a two-column connecting table
	 * called "a_to_b" with columns named "a_id" and "b_id", respectively the primary keys of
	 * rows in the "a" and "b" tables, you would get all the "b" rows connected to an "a" row
	 * by writing:
	 * <pre>
	 *   List<Row> bRows = aRow.dereference("a_id", a_to_b, "b_id", b);
	 * </pre> 
	 * @param leftKey the name of the column in the connecting table that has primary key
	 *   values in the table to which this row belongs
	 * @param connectingTable
	 * @param rightKey the name of the column in the connecting table that has primary key
	 *   values in the target table ("b" in this example)
	 * @param targetTable
	 * @return a List of Rows
	 * @throws SQLException if anything goes wrong
	 */
	public ArrayList<Row> dereference(
			String leftKey, Table connectingTable,
			String rightKey, Table targetTable) throws SQLException {
		return new DerefRproc(connectingTable, targetTable,
				leftKey, rightKey, (String)targetTable.getPrimaryKey()).getResults();
	}

	/**
	 * This is just like {@link #dereference(String, Table, String, Table)} except it is
	 * assumed that there will be exactly one result, so an exception is thrown if that's not
	 * the case. The arguments are identical. Given a two-column connecting table called "a_to_b"
	 * with columns named "a_id" and "b_id", respectively the primary keys of rows in the "a"
	 * and "b" tables, you would find the single "b" row connected to an "a" row by writing:
	 * <pre>
	 *   Row bRow = aRow.dereferenceUnique("a_id", a_to_b, "b_id", b);
	 * </pre> 
	 * @param leftKey
	 * @param connectingTable
	 * @param rightKey
	 * @param targetTable
	 * @return the single Row
	 * @throws SQLException if there are zero or multiple results, or anything goes wrong
	 */
	public Row dereferenceUnique(
			String leftKey, Table connectingTable,
			String rightKey, Table targetTable) throws SQLException {
		ArrayList<Row> results =
				new DerefRproc(connectingTable, targetTable, leftKey, rightKey,
						(String)targetTable.getPrimaryKey()).getResults();
		if (results.size() != 1)
			throw new SQLException("Not exactly one result");
		return results.get(0);
	}
}
