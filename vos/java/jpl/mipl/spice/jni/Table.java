/*
 * Table.java
 *
 * Created on August 15, 2000, 8:55 AM
 */

package jpl.mipl.spice.jni;

import java.util.ArrayList;
import java.sql.SQLException;

/** An E kernel database table.
 * @author Michael Brady
 */
public class Table extends Object {

  private String    m_name    = "";
  private ArrayList m_columns = new ArrayList();
  
  /** Creates a new Table
   * @param name The name of this database table.
   */
  public Table(String name) 
  {
    setName(name);
  }

  /** Returns the name of this table.
   * @return the name of this table.
   */
  public String getName()
  {
    return m_name;
  }
  
  /** Sets the name of this table.
   * @param name the name of this table
   */
  public void setName(String name)
  {
    m_name = name;
  }
  
  /** Adds the specified column to this table.
   * @param column the column to be added
   */
  public void addColumn(Column column)
  {
    m_columns.add(column);
  }
  
  /** Returns an iterator which will iterate over all columns which have been added to this table so far.
   * @return an iterator which returns {@link Column}s
   */
  public java.util.Iterator getColumnIterator()
  {
    return m_columns.iterator();
  }
  
  /** Returns the number of column in this table
   * @return the number of columns in this table
   */
  public int getNumColumns()
  {
    return m_columns.size();
  }
  
  /**
   * Returns the specified column, or null if it does not exist.
   */
  public Column getColumn(String name)
  {
    for (int i=0; i < getNumColumns(); ++i)
    {
      if (getColumn(i).getName().equals(name))
      {
        return getColumn(i);
      }
    }
    return null;
  }
  
  /**
   *
   */
  public Column getColumn(int columnIndex)
  {
    return (Column)m_columns.get(columnIndex);
  }

  /** Returns the columns which have been added to this table so far.
   * @return the columns in this table
   */
  public Column[] getColumns()
  {
    Column[] ar = new Column[m_columns.size()];
    m_columns.toArray(ar);
    return ar;
  }
  
  
  /** Sets the columns in this table equal to those described in the specifed 
   *  result set meta data object.
   * @param md The meta data from a SQL query
   * @throws SQLException if an error occurs while accessing the meta data
   */
/*  
  public void setColumns(ResultSetMetaData md)
    throws SQLException
  {
    m_columns.clear();
    
    for (int i=1; i <= md.getColumnCount(); ++i)
    {
      Column col = new Column();
      col.setName(md.getColumnName(i));
      switch (md.getColumnType(i))
      {
        case Types.NUMERIC:
          col.setDataType(Column.INTEGER);
          break;
          
        case Types.VARCHAR:
          col.setDataType(Column.STRING);
          col.setStringLength(80);  // TODO how can I find this?
          break;
          
        case Types.DATE:   // TODO is this right?
        case (int)93:      // TODO Is this also a date?
          col.setDataType(Column.STRING);
          col.setStringLength(80);
          break;
          
        case Types.CLOB:    // TODO is this right?
          col.setDataType(Column.STRING);
          col.setStringLength(4000);
          break;
          
        case Types.DOUBLE:
          col.setDataType(Column.DOUBLE);
          break;
          
        default:
          throw new java.security.InvalidParameterException(
            "Table.setColumns: ddd " +
            "type " + String.valueOf(md.getColumnType(i)) +
            " (" + md.getColumnTypeName(i) + ") is not supported.");
          
      }
      switch (md.isNullable(i))
      {
        case ResultSetMetaData.columnNullable:
          col.setNullsOk(true);
        break;
        
        case ResultSetMetaData.columnNullableUnknown:
          col.setNullsOk(true);
          System.out.println("Table.setColumns:  Warning: column " + 
                    getName() + "." +
                    md.getColumnName(i) + "'s nullability is unknown.");
        break;          
      }
      m_columns.add(col);
    }
  }
*/
  
  /** Sets the name and columns in this table equal to those described in 
   *  the specifed 
   *  ResultSet, which should be the product of a DataBaseMetaData
   *  getColumns call.
   * @param dbDescription the result of a DataBaseMetaData.getColumns call
   * @throws SQLException if an error occurs while accessing the meta data
   */
  public void set(java.sql.ResultSet dbDescription)
    throws SQLException, BadDatabaseDescriptionException
  {
    m_columns.clear();
    
    boolean firstTime = true;
    while(dbDescription.next())
    {
      if (firstTime)
      {
        setName(dbDescription.getString("TABLE_NAME"));
        firstTime = false;
      }
      else // it's not the first time
      {
        if (!getName().equals(dbDescription.getString("TABLE_NAME")))
        {
          throw new BadDatabaseDescriptionException(
            "Table.set:  dbDescription has a description of more than " +
            "one table.");              
        }
      }
      Column column = new Column();
      column.set(dbDescription);
      m_columns.add(column);
    }
  }
}