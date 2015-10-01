/*
 * Segment.java
 *
 * Created on August 15, 2000, 9:03 AM
 */

package jpl.mipl.spice.jni;

import java.util.ArrayList;
import java.util.List;
import java.util.Iterator;

import java.lang.reflect.Array;

import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.Types;

/** An E kernel segment.  There are one or more segments in a {@link Table }, 
 *  and one or more segments in an E kernel file.
 * @author Michael Brady
 */
public class Segment 
    extends Object 
{

  private ArrayList m_dataLists = new ArrayList();
  private Table m_table;
  
  /** Creates a new Segment.
   *  @param table the table to which this segment will belong.
   */
  public Segment(Table table) 
  {
    m_table = table;
  }

  /** Adds the specified list of data to this segment.  
   *  Each list is used as a single column of a table.

   * @param data 
   *    If the column has a fixed array length, then this 
   *    should be a list of Integers, Doubles, or Strings.
   *    For example, to add six rows of data of type Integer[5],
   *    you should a List of 30 Integers.
   *    If the column has a variable array length, then this should
   *    be a list of Lists of Integers, Doubles, or Strings.
   *          If there are any other classes in the list,
   *          behavior is undefined.
   */
  public void addData(List data)
  {
    m_dataLists.add(data);
  }
  
  /** Sets the data in this segment equal to the data contained in the specified result set.
   * @param rs The results of a SQL query.
   * @throws SQLException can be thrown by the ResultSet
   */
  public void setData(ResultSet rs)
    throws SQLException
  {
    m_dataLists.clear();
   
    ResultSetMetaData md = rs.getMetaData();
    for (int i=1; i <= md.getColumnCount(); ++i)
    {
      m_dataLists.add(new ArrayList());
    }
    
    while (rs.next())
    {
      for (int i=1; i <= md.getColumnCount(); ++i)
      {    
        ArrayList dataList = (ArrayList)m_dataLists.get(i - 1);
        switch (md.getColumnType(i))
        {
         case Types.NUMERIC:
           dataList.add(new Integer(rs.getInt(i)));
           if (rs.wasNull())
          {
            System.out.println("Integer was null: " + dataList.get(dataList.size() -1).toString());
          }
           break;

         case Types.CHAR: 
         case Types.VARCHAR:
         case Types.DATE:   // TODO Transform this to an E Kernel date.
         case 93:           
         case Types.CLOB:

           dataList.add(rs.getString(i));
           break;
          
         case Types.DOUBLE:
           dataList.add(new Double(rs.getDouble(i))); 
           break;
          
         default:        
           throw new java.security.InvalidParameterException(
              "Segment.setData:  " + 
              "type " + String.valueOf(md.getColumnType(i)) +
              " (" + md.getColumnTypeName(i) + ") is not supported.");

        }
      }
    }
  }
    
  /** Adds this segment to the specified table in the specified file.
   * @param file A file which should have been opened for writing.
   * @throws SpiceException if a SPICE toolkit error occurs
   * @throws InvalidSegmentException if this segment is invalid.  
   *         A segment is invalid for reasons including:  
   *         all data lists not being the same length, 
   *         the types of data in the lists don't match the column specification.
   */
  public void addTo(EKernel file)
    throws SpiceException, InvalidSegmentException
  {
    if (m_table.getNumColumns() != m_dataLists.size())
    {
      throw new InvalidSegmentException(
            "Segment.addTo:  the number of columns in the specified table (" +
            String.valueOf(m_table.getNumColumns()) + 
            ") is not the same as the number of columns of data (" +
            String.valueOf(m_dataLists.size())  +
            ") which " +
            "have been added to this segment.");
    }
    
    int numRows = 0;
    
    for (int i=0; i < m_dataLists.size(); ++i)
    { 
      int currNumRows = ((List)m_dataLists.get(i)).size();
      if (!m_table.getColumn(i).getArrayLengthIsVariable())
      {
          currNumRows /= m_table.getColumn(i).getArrayLength();
      }
      
      if (i == 0)
      {
          numRows = currNumRows;
      }

      if (numRows != currNumRows)
      {
        throw new InvalidSegmentException(
                "Segment.addTo:  Number of rows of data items is not the " +
                "same in all columns.");
      }
    }    
    
    if (numRows < 1)
    {
      throw new InvalidSegmentException(
            "Segment.addTo:  This segment has zero rows.");
    }

    
    
    
    
    
    int[] records = new int[numRows];

    ////////////////////////////////////////////////////////////    
    // Initialize the segment.
    ////////////////////////////////////////////////////////////    

    int segmentNum = SpiceLib.ekifld(file.getHandle(),
                                      m_table.getName(),
                                      numRows,
                                      m_table.getColumns(),
                                      records);


    ////////////////////////////////////////////////////////////                                         
    // Add the data to the segment.
    ////////////////////////////////////////////////////////////
    
    Iterator iData = m_dataLists.iterator();
    Iterator iColumn = m_table.getColumnIterator();
    while (iData.hasNext())
    {
      Column column = (Column)iColumn.next();
      List data = (List)iData.next();

      boolean[] nulls = new boolean[numRows];
      for (int i=0; i < numRows; ++i)
      {
        nulls[i] = (data.get(i) == null);
      }
      
      int[] arrayLengths = null;  // Null means column is fixed array length.
      if (column.getArrayLengthIsVariable())
      {
        arrayLengths = new int[numRows];
        List newData = new ArrayList();
        
        // The data is a list of arrays.
        // Find the length of each array.
        // Convert the list of arrays into a list of single elements.
        //   (So a list of 6 arrays of 5 integers each would become
        //    a list of 30 integers).
        
        for (int i=0; i < numRows; ++i)
        {
            List currArray = ((List)data.get(i));
            arrayLengths[i] = 
                ((currArray == null) ? 0 : currArray.size());
                   
            for (int j=0; j < arrayLengths[i]; ++j)
            {
                newData.add(currArray.get(j));
            }                    
        }
        data = newData;
      }
      
      
      switch (column.getDataType())
      {
        case Column.INTEGER:
        {            
          int[] vals = new int[data.size()];
          for (int i=0; i < data.size(); ++i)
          {
            vals[i] = ((Integer)data.get(i)).intValue();
          }
          
          
          SpiceLib.ekacli(file.getHandle(),
                           segmentNum,
                           column.getName(),
                           records,
                           vals,
                           arrayLengths,
                           nulls,
                           column.isIndexed());
        }                
        break;
          
        case Column.TIME:
        {
          double[] vals = new double[data.size()];
          for (int i=0; i < data.size(); ++i)
          {
            Object o = data.get(i);
            try
            {
              vals[i] = ((Double)o).doubleValue();
            }
            catch (ClassCastException e1)
            {
                // We don't catch an exception thrown from this next line
                // because we really have failed if this throws.
                
                vals[i] = ((Time)o).asDouble();
            }
          }
          
          
          SpiceLib.ekacld(file.getHandle(),
                           segmentNum,
                           column.getName(),
                           records,
                           vals,
                           arrayLengths,
                           nulls,
                           column.isIndexed());
        }                
        break;            
            
        case Column.DOUBLE:
        {
          double[] vals = new double[data.size()];
          for (int i=0; i < data.size(); ++i)
          {
            vals[i] = ((Double)data.get(i)).doubleValue();
          }
          
          
          SpiceLib.ekacld(file.getHandle(),
                           segmentNum,
                           column.getName(),
                           records,
                           vals,
                           arrayLengths,
                           nulls,
                           column.isIndexed());
        }                
        break;
        
        case Column.STRING:
        case Column.STRING_VAR_LENGTH:            
        {
          String[] vals = new String[data.size()];
            
          for (int i=0; i < data.size(); ++i)
          {
            vals[i] = (String)data.get(i);
            if (vals[i] == null)
            {
              vals[i] = "";
            }
          }
          
          SpiceLib.ekaclc(file.getHandle(),
                           segmentNum,
                           column.getName(),
                           records,
                           vals,
                           arrayLengths,
                           nulls,
                           column.isIndexed());
        }                
        break;          
          
        default:
            
          throw new InvalidSegmentException(
            "Segment.addTo:  column " + column.getName() + 
            " has unknown data type " + 
            String.valueOf(column.getDataType()));            
      }     
    }
    
    ////////////////////////////////////////////////////////////
    // Finalize the segment.
    ////////////////////////////////////////////////////////////
    
    SpiceLib.ekffld(file.getHandle(), segmentNum, records);
  }
}