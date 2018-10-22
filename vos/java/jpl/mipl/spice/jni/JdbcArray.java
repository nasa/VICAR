/*
 * Array.java
 *
 * Created on November 3, 2000, 8:54 AM
 */

package jpl.mipl.spice.jni;

/**
 *  An SQL array implementation for the E kernel JDBC wrapper.
 *
 * @author  Mihcael Brady
 */
public class JdbcArray extends Object implements java.sql.Array
{
  private JdbcResultSet m_resultSet = null;
  private int m_columnIndex = -1;
  private int m_baseType = -1;

  /** Creates a new Array */
  public JdbcArray(JdbcResultSet rs,int columnIndex,int baseType) 
  {
    m_resultSet   = rs;
    m_columnIndex = columnIndex;
    m_baseType = baseType;
  }

  public Object getArray(long index,int count,final java.util.Map map)
    throws java.sql.SQLException 
  {
    throw new java.sql.SQLFeatureNotSupportedException();
  }
  
  public java.sql.ResultSet getResultSet(long index,int count) 
    throws java.sql.SQLException 
  {
    throw new java.sql.SQLFeatureNotSupportedException();
  }
  public java.sql.ResultSet getResultSet(long index,int count,final java.util.Map p3) 
    throws java.sql.SQLException 
  {
    throw new java.sql.SQLFeatureNotSupportedException();
  }

  /**
   *  Inherited from java.sql.Array.
   */  
  public java.lang.Object getArray() 
    throws java.sql.SQLException 
  {
    return m_resultSet.getObjectArray(m_columnIndex);
  }

  /**
   *  Inherited from java.sql.Array.
   */
  public java.lang.Object getArray(long index,int count) 
    throws java.sql.SQLException 
  {
    if (index > (long)Integer.MAX_VALUE)
    {
        System.out.println("jpl.mipl.spice.jni.Array.getArray: WARNING:  " +
                            "index is being " +
                            "converted to int, possible loss of data.");
    }
    
    return m_resultSet.getObjectArray(m_columnIndex, (int)index, count);
  }
  
  public java.sql.ResultSet getResultSet() 
    throws java.sql.SQLException 
  {
    throw new java.sql.SQLFeatureNotSupportedException();
  }
  public java.lang.Object getArray(final java.util.Map map) 
    throws java.sql.SQLException 
  {
    throw new java.sql.SQLFeatureNotSupportedException();
  }

  /**
   *  Inherited from java.sql.Array.
   */
  public int getBaseType() 
    throws java.sql.SQLException 
  {
    return m_baseType;
  }
  
  public java.sql.ResultSet getResultSet(final java.util.Map map) 
    throws java.sql.SQLException 
  {
    throw new java.sql.SQLFeatureNotSupportedException();
  }
  public java.lang.String getBaseTypeName() 
    throws java.sql.SQLException 
  {
    throw new java.sql.SQLFeatureNotSupportedException();
  }
  
  /**
   * @since 1.6
   */
  public void free()
    throws java.sql.SQLException
  {
    throw new java.sql.SQLFeatureNotSupportedException();
  }

}
