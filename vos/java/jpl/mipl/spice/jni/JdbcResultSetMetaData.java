/*
 * ResultSetMetaData.java
 *
 * Created on October 30, 2000, 11:49 AM
 */

package jpl.mipl.spice.jni;

/**
 *
 * @author  mpb
 * @version 
 */
public class JdbcResultSetMetaData extends Object implements java.sql.ResultSetMetaData
{
  private String m_query = null;
  private int m_numColumns = -1;
  private int[] m_dataTypes = null;
  private boolean[] m_isArray = null;
  private int[] m_classes = null;
  private String[] m_tableNames = null;
  private String[] m_columnNames = null;
  
  /** Creates new ResultSetMetaData */
  JdbcResultSetMetaData(String query,int numColumns,int[] dataTypes,int[] classes,String[] tableNames,String[] columnNames) 
  {
    m_query = query;
    m_numColumns = numColumns;
    m_dataTypes = dataTypes;
    m_classes = classes;
    m_tableNames = tableNames;
    m_columnNames = columnNames;
  }

  /**
   *  Sets the flags indicating whether each column is an array.
   */
  void setArray(boolean[] isArray)
  {
    m_isArray = isArray;
  }
  
  /**
   *  Returns null.  The E kernel does not have the concept of
   *  a schema or catalog.
   */
  public String getCatalogName(int p1) 
    throws java.sql.SQLException
  {
    return null;
  }
  
  /**
   *  From the java.sql.ResultSetMetaData interface.
   */
  public int getColumnCount() 
    throws java.sql.SQLException
  {
    return m_numColumns;
  }
  
  /**
   *  From the java.sql.ResultSetMetaData interface.
   */
  public String getColumnName(int column) 
    throws java.sql.SQLException
  {
    return m_columnNames[column];
  }


  
  /**
   *  From the java.sql.ResultSetMetaData interface.
   */
  public int getColumnType(int column) 
    throws java.sql.SQLException
  {
    return m_isArray[column] ? java.sql.Types.ARRAY : m_dataTypes[column];
  }

  /**
   *  If column is an array, returns its base type.  Otherwise,
   *  returns same as getColumnType(int).
   */
  public int getColumnBaseType(int column) 
    throws java.sql.SQLException
  {
    return m_dataTypes[column];
  }  
  
  /**
   *  From the java.sql.ResultSetMetaData interface.
   */  
  public java.lang.String getTableName(int column) 
    throws java.sql.SQLException
  {
    return m_tableNames[column];
  }
  
  public boolean isCurrency(int p1) 
    throws java.sql.SQLException 
  {
    return false;
  }
  
  public boolean isDefinitelyWritable(int p1) 
    throws java.sql.SQLException 
  {
    return false;
  }
  
  public int getPrecision(int p1) throws java.sql.SQLException {
    throw new java.sql.SQLFeatureNotSupportedException();
  }
  public boolean isSigned(int p1) throws java.sql.SQLException {
    throw new java.sql.SQLFeatureNotSupportedException();
  }
  public boolean isAutoIncrement(int p1) throws java.sql.SQLException {
    throw new java.sql.SQLFeatureNotSupportedException();
  }
  public int getScale(int p1) throws java.sql.SQLException {
    throw new java.sql.SQLFeatureNotSupportedException();
  }
  public boolean isCaseSensitive(int p1) throws java.sql.SQLException {
    throw new java.sql.SQLFeatureNotSupportedException();
  }
  public int getColumnDisplaySize(int p1) throws java.sql.SQLException {
    throw new java.sql.SQLFeatureNotSupportedException();
  }
  public boolean isWritable(int p1) throws java.sql.SQLException {
    throw new java.sql.SQLFeatureNotSupportedException();
  }
  public boolean isReadOnly(int p1) throws java.sql.SQLException {
    throw new java.sql.SQLFeatureNotSupportedException();
  }
  public int isNullable(int p1) throws java.sql.SQLException {
    throw new java.sql.SQLFeatureNotSupportedException();
  }
  public boolean isSearchable(int p1) throws java.sql.SQLException {
    throw new java.sql.SQLFeatureNotSupportedException();
  }
  public java.lang.String getColumnLabel(int p1) throws java.sql.SQLException {
    throw new java.sql.SQLFeatureNotSupportedException();
  }
  public java.lang.String getSchemaName(int p1) throws java.sql.SQLException {
    throw new java.sql.SQLFeatureNotSupportedException();
  }
  public java.lang.String getColumnTypeName(int p1) throws java.sql.SQLException {
    throw new java.sql.SQLFeatureNotSupportedException();
  }
  public java.lang.String getColumnClassName(int p1) throws java.sql.SQLException {
    throw new java.sql.SQLFeatureNotSupportedException();
  }
  public boolean isWrapperFor(Class<?> iface) throws java.sql.SQLException {
    throw new java.sql.SQLFeatureNotSupportedException();
  }
  public <T> T unwrap(Class<T> iface) throws java.sql.SQLException { 
    throw new java.sql.SQLFeatureNotSupportedException();
  }
    
}
