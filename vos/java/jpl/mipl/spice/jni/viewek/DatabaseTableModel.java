/*
 * DatabaseTableModel.java
 *
 * Created on August 21, 2000, 9:49 AM
 */

package jpl.mipl.spice.jni.viewek;

import jpl.mipl.spice.jni.*;

import java.util.ArrayList;
import java.util.List;

/**
 * @author Michael Brady
 */
public class DatabaseTableModel 
  extends Object
  implements javax.swing.table.TableModel
{

  private java.sql.ResultSetMetaData m_metaData; 
  private List m_data;
  
  /** 
   *  Creates new DatabaseTableModel which holds all data in the
   *  specified result set.
   */
  public DatabaseTableModel(java.sql.ResultSet resultSet )
    throws SpiceException, ResultNotFoundException, java.sql.SQLException
  {
    m_metaData = resultSet.getMetaData();
    m_data = ((jpl.mipl.spice.jni.JdbcResultSet)resultSet).toList();
    
    if (m_metaData.getColumnCount() != m_data.size())
    {
      throw new java.security.InvalidParameterException(
        "DatabaseTableModel.DatabaseTableModel:  number of columns in " +
        "table doesn't match the number of columns in resultSet.");
    }
  }

  /** 
   *  Creates new DatabaseTableModel which holds data from
   *  the specified rows of the
   *  specified result set.
   *  @param beginRow the zero-based index of the row with which to start.
   *  @param numRows the maximum number of rows to include.
   */
  public DatabaseTableModel(java.sql.ResultSet resultSet,
                               int beginRow,
                               int numRows)
    throws SpiceException, ResultNotFoundException, java.sql.SQLException
  {
    m_metaData = resultSet.getMetaData();
    m_data = ((jpl.mipl.spice.jni.JdbcResultSet)resultSet).
                                                toList(beginRow, numRows);
    
    if (m_metaData.getColumnCount() != m_data.size())
    {
      throw new java.security.InvalidParameterException(
        "DatabaseTableModel.DatabaseTableModel:  number of columns in " +
        "table doesn't match the number of columns in resultSet.");
    }
  }
  
  public int getRowCount() 
  {
    try
    {
      return ((List)m_data.get(0)).size();
    }
    catch (Exception e)
    {
      return 0;
    }
  }
  
  public void setValueAt(final java.lang.Object p1,int p2,int p3) 
  {
  }
  
  public void removeTableModelListener(final javax.swing.event.TableModelListener p1) 
  {
  }
  
  public boolean isCellEditable(int p1,int p2) 
  {
    return false;
  }
  
  public int getColumnCount() 
  {
    try
    {
      return m_metaData.getColumnCount();
    }
    catch (java.sql.SQLException e)
    {
      return 0;
    }
  }
  
  public java.lang.Object getValueAt(int row, int column) 
  {
    List l = (List)m_data.get(column);
    Object ret = l.get(row);
    try
    {
      Object[] array = (Object[])ret;
      StringBuffer s = new StringBuffer();
      for (int i=0; i < array.length; ++i)
      {
        s.append("[" + array[i].toString() + "]");
      }
      ret = s.toString();
    }
    catch (Exception e) {};
    
    return ret;
  }
  
  public java.lang.Class getColumnClass(int columnIndex) 
  {
    if (getRowCount() < 1)
    {
      return Object.class;
    }
    return getValueAt(0, columnIndex).getClass();
  }
  
  public java.lang.String getColumnName(int columnIndex) 
  {
    try
    {
      return m_metaData.getColumnName(columnIndex);
    }
    catch (java.sql.SQLException e)
    {
      return "";
    }
  }
  
  public void addTableModelListener(final javax.swing.event.TableModelListener p1) 
  {  
  }
  
}