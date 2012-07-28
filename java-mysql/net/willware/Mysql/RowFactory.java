package net.willware.Mysql;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Map;

/**
 * Overloading the factory for Row instances enables the extension of the
 * {@link Row} class to add convenience methods.
 */
public class RowFactory {

	private static RowFactory instance = null;

	/**
	 * If you plan to extend RowFactory, you should call this method before
	 * any calls are made to {@link #getInstance()}, otherwise your factory
	 * will have no effect. To be useful, your factory must overload the
	 * {@link #makeRow(Table, ResultSet)} method.
	 * @param rf an instance of your RowFactory inheritor class 
	 */
	public static void setInstance(RowFactory rf) {
		instance = rf;
	}

	/**
	 * Get a copy of the current RowFactory. This method is used in several
	 * SQL queries. If any of these occur before a call to {@link #setInstance(RowFactory)},
	 * the setInstance call will have no effect.
	 * @return a reference to the current RowFactory; if none was set at the time of
	 * the call, use an instance of {@link RowFactory}, which will then be returned for
	 * all subsequent calls
	 */
	public static RowFactory getInstance() {
		if (instance == null) {
			instance = new RowFactory();
		}
		return instance;
	}

	/**
	 * Create a {@link Row} instance. If you want to define your own Row class, you
	 * should be sure to override this method.
	 * @param table the Table this Row comes from
	 * @param rs a result from a "SELECT * FROM table" query, used to populate the column
	 * values of the Row
	 * @return the Row instance
	 * @throws SQLException
	 */
	public Row makeRow(Table table, ResultSet rs) throws SQLException {
		Row row = new Row(table);
		Map<String, Object> map = row.getMap();
		int i = 1;
		for (Column c : table.getColumns()) {
			map.put(c.getName(), rs.getObject(i++));
		}
		return row;
	}

}
