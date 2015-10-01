package jpl.mipl.spice.jni;

import java.util.*;

/**
 * A segment of a SPICE E kernel which may be updated.  
 *
 * Note that there are two segment classes.  Use Segment when you want
 * to add a whole segment at once.  Use SegmentRW when you want to add to,
 * delete from, or modify an existing segment.
 */
public class SegmentRW
{
   /** The the file to which this segment belongs. */
   private EKernel m_file;

   /** The table to which this segment belongs. */
   private Table m_table;

   /** The number of this segment. */
   private int m_segmentNum;

   /** Creates a new SegmentRW which belongs to the specified file and table. 
    */
   public SegmentRW(EKernel file, int segmentNum, Table table)
   {
      m_file = file;
      m_segmentNum = segmentNum;
      m_table = table;
   }

   /** Appends a new record to this segment.
    *  @data the data to be placed in the new record.
    *        This is a list of lists.  Each list contains the data
    *        for one column.
    *  @return the number of the newly created record.
    */
   public int appendRecord(List data)
      throws SpiceException
   {
      int recordNum = SpiceLib.ekappr(m_file.getHandle(),
                                      m_segmentNum);

      updateRecord(recordNum, data);
      return recordNum;
   }

   /** Updates the specified STRING cell. */
   private void initCell(int recordNum, String columnName, String[] data)
      throws SpiceException
   {
      SpiceLib.ekacec(m_file.getHandle(),
                      m_segmentNum,
                      recordNum,
                      columnName,
                      data );
   }

   /** Updates the specified INTEGER cell. */
   private void initCell(int recordNum, String columnName, int[] data)
      throws SpiceException
   {
      SpiceLib.ekacei(m_file.getHandle(),
                      m_segmentNum,
                      recordNum,
                      columnName,
                      data );
   }

   /** Updates the specified DOUBLE cell. */
   private void initCell(int recordNum, 
                          String columnName, 
                          double[] data)
      throws SpiceException
   {
      SpiceLib.ekaced(m_file.getHandle(),
                      m_segmentNum,
                      recordNum,
                      columnName,
                      data );
   }


   /** Updates the specified record with the specified data.
    *  @param data each item is a list of one column's data.
    */
   public void updateRecord(int recordNum, List dataList)
      throws SpiceException
   {
      if (dataList.size() != m_table.getNumColumns())
      {
         throw new java.lang.IllegalArgumentException(
          "Attempted to add a list of " + dataList.size() + 
          " elements, but there are " + m_table.getNumColumns() + 
          " columns in the table.");
      }

      Iterator iData = dataList.iterator();
      Iterator iColumn = m_table.getColumnIterator();
      while (iData.hasNext())
      {
         Column column = (Column)iColumn.next();

         List data = (List)iData.next();
         boolean isNull = (data == null);
      
      
         switch (column.getDataType())
         {
         case Column.INTEGER:
            {            
               int[] vals = null;
               if (!isNull)
               {
                  vals  = new int[data.size()];
                  for (int i=0; i < data.size(); ++i)
                  {
                     vals[i] = ((Integer)data.get(i)).intValue();
                  }
               }
               
               initCell( recordNum,
                         column.getName(),
                         vals );
            }                
            break;
            
         case Column.TIME:
            {
               double[] vals = null;
               if (!isNull)
               {
                  vals  = new double[data.size()];
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
               }
               
               initCell( recordNum,
                         column.getName(),
                         vals );
               
            }                
            break;            
            
         case Column.DOUBLE:
            {
               double[] vals = null;
               if (!isNull)
               {
                  vals = new double[data.size()];
                  for (int i=0; i < data.size(); ++i)
                  {
                     vals[i] = ((Double)data.get(i)).doubleValue();
                  }
               }
               

               initCell( recordNum,
                           column.getName(),
                           vals );
            }                
            break;
        
         case Column.STRING:
         case Column.STRING_VAR_LENGTH:            
            {
               String[] vals = null;
               
               if (!isNull)
               {
                  vals = new String[data.size()];
                  for (int i=0; i < data.size(); ++i)
                  {
                     vals[i] = (String)data.get(i);
                  }
               }
               
               initCell( recordNum,
                         column.getName(),
                         vals );
            }                
            break;          
        
         default:
            
            throw new RuntimeException(
                "SegmentRW.add:  column " + column.getName() + 
                " has unknown data type " + 
                String.valueOf(column.getDataType()));
         }
      } 
   }

}
