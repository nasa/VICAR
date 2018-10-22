/*
 * ColumnDescriptor.java
 *
 * Created on August 14, 2000, 4:08 PM
 */

package jpl.mipl.spice.jni;

import java.sql.Types;
import java.sql.DatabaseMetaData;

/** A column in an E kernel table.
 * @author Michael Brady
 */
public class Column extends Object {

    private static final String STR_INT = "INTEGER";
    private static final String STR_DOUBLE_PRECISION = "DOUBLE PRECISION";
    private static final String STR_STRING = "CHARACTER*(";
    private static final String STR_STRING_VAR_LENGTH = "CHARACTER*(*)";
    private static final String STR_TIME = "TIME";
    
    private static final String STR_VARIABLE_ARRAY_LENGTH = "VARIABLE";
    
    private static final String STR_DATATYPE = "DATATYPE";
    private static final String STR_SIZE     = "SIZE";
    private static final String STR_INDEXED  = "INDEXED";
    private static final String STR_NULLS_OK = "NULLS_OK";
    
    /** INTEGER column data type
     */
    public static final int INTEGER           = 0;
    /** DOUBLE column data type
     */
    public static final int DOUBLE            = 1;
    /** STRING column data type
     */
    public static final int STRING            = 2;
    /** STRING_VAR_LENGTH column data type (a string of undefined (variable) length).
     */
    public static final int STRING_VAR_LENGTH = 3;
    /** TIME column data type
     */
    public static final int TIME              = 4;
  
    private String m_name = "";
    private int m_dataType = INTEGER;
    private int m_stringLength = 20;
    private int m_arrayLength = 1;
    private boolean m_arrayLengthIsVariable = false;
    private boolean m_isIndexed = false;
    private boolean m_nullsOk = false;
  
    /** Creates a new Column
     */
    public Column() 
    {
    }
    
    private static String typeCodeToString(int type, int stringLength)
    {
      switch (type)
      {
        case INTEGER: return STR_INT;
        case DOUBLE:  return STR_DOUBLE_PRECISION;
        case STRING:  return STR_STRING + String.valueOf(stringLength) + ")";
        case STRING_VAR_LENGTH:  return STR_STRING_VAR_LENGTH;
        case TIME:    return STR_TIME;
        default:
          throw new java.security.InvalidParameterException(
            "Column.typeCodeToString:  illegal type code: " + 
            String.valueOf(type));
      }
    }
    
    private static String booleanToString(boolean b)
    {
      return b ? "TRUE" : "FALSE";
    }
    
    
    
    
    // Accessor /////////////////////////////////////////////////////////
    
    
    /** Returns the name of this column
     * @return The name of this column
     */
    public String getName()
    {
      return m_name;
    }
    
    /** Returns the type of data which this column holds.
     * @return {@link #INTEGER}, {@link #DOUBLE}, {@link #STRING}, {@link #STRING_VAR_LENGTH}, or {@link #TIME}
     */
    public int getDataType()
    {
      return m_dataType;
    }
    
    /** Returns the length of string which this column holds if its data type is STRING.  If the data type is not STRING, this number is meaningless.
     * @return a string length, including one character for a trailing null.
     */
    public int getStringLength()
    {
      return m_stringLength;
    }
    
    /** Returns the number of values held in each row of this column.
     * @return the number of values held in each row of this column.
     */
    public int getArrayLength()
    {
      return m_arrayLength;
    }

    /** Returns true if the number of values held in each row of this column
     *  is variable.  If this is true, then the value of 
     *  {@link #getArrayLength()} is ignored.
     */
    public boolean getArrayLengthIsVariable()
    {
      return m_arrayLengthIsVariable;
    }    
    
    /** Returns <CODE>true</CODE> if this column is indexed.
     * @return <CODE>true</CODE> if this column is indexed.
     */
    public boolean isIndexed()
    {
      return m_isIndexed;
    }
    
    /** Returns <CODE>true</CODE> if nulls are allowed.
     * @return <CODE>true</CODE> if nulls are allowed.
     */
    public boolean nullsOk()
    {
      return m_nullsOk;
    }
    
    /** Returns a string description of this column in a format readable by the SPICE toolkit.
     * @return a description in comma-delimited KEYWORD=VALUE pairs
     */
    String getDescriptor()
    {
      return STR_DATATYPE + " = " + 
        typeCodeToString(getDataType(), getStringLength()) + ", " +
        
        STR_SIZE + " = " + 
        (getArrayLengthIsVariable() ? 
            STR_VARIABLE_ARRAY_LENGTH : String.valueOf(getArrayLength())) + 
        ", " +
        
        STR_INDEXED + " = " + booleanToString(isIndexed()) + ", " +
        
        STR_NULLS_OK + " = " + booleanToString(nullsOk());
    }
    
    // Modifiers /////////////////////////////////////////////////////////
    
    /** Sets the name of this column.
     * @param name the name of this column
     */
    public void setName(String name)
    {
      m_name = name;
    }
    
    /** Sets the type of data this column will hold.
     * @param type Can be one of: {@link #INTEGER}, {@link #DOUBLE}, {@link #STRING}, {@link #STRING_VAR_LENGTH}, or {@link #TIME}
     */
    public void setDataType(int type)
    {
        if ((type == STRING_VAR_LENGTH) && 
            (getArrayLengthIsVariable() || getArrayLength() > 1))
        {
            throw new java.security.InvalidParameterException(
                "Column.setDataType:  " + 
                "Column must have an array length of 1 to set type to " +
                "STRING_VAR_LENGTH");
        }
        
      m_dataType = type;
    }
    
    /** Sets the length of strings in this column if this column's data type is <CODE>STRING</CODE>.  Otherwise, has no effect.
     * @param length the length of Strings this column will hold.  
     *         This includes a trailing null character, so is one
     *         longer than most databases.
     */
    public void setStringLength(int length)
    {
      m_stringLength = length;
    }
    
    /** Sets the number of values held in one row of this column.
     * @param length The number of values in one row of this column.
     */
    public void setArrayLength(int length)
    {
      if (length < 1)
      {
          setArrayLengthIsVariable(true);
          length = 1;
      }

      if ((m_dataType == STRING_VAR_LENGTH) && 
          (!getArrayLengthIsVariable()) && (length > 1))
      {
          throw new java.security.InvalidParameterException(
                "Column.setArrayLength:  " + 
                "Columns of type STRING_VAR_LENGTH can only have an " +
                "array length of 1");
        }      
        m_arrayLength = length;
    }

    /** Sets whether the number of values held in one row of this column
     *  is variable.
     *  If this is set to be true, then the value set with
     *  {@link #setArrayLength(int)} is ignored.
     */
    public void setArrayLengthIsVariable(boolean isVariable)
    {
        if ((m_dataType == STRING_VAR_LENGTH) && isVariable)
        {
            throw new java.security.InvalidParameterException(
                "Column.setArrayLengthIsVariable:  " + 
                "Can not set array length to variable in " +
                "a column of type STRING_VAR_LENGTH.");
        }
        
        m_arrayLengthIsVariable = isVariable;
    }    
    
    /** Sets whether this column is indexed.
     * @param isIndexed if <CODE>true</CODE>, this column will be indexed
     */
    public void setIndexed(boolean isIndexed)
    {    
      m_isIndexed = isIndexed;
    }
    
    /** Sets whether NULL are allowed in this column.
     * @param nullsOk if <CODE>true</CODE>, NULLs are allowed.
     */
    public void setNullsOk(boolean nullsOk)
    {      
      m_nullsOk = nullsOk;
    }
    
  /** Sets this column equal to that described in 
   *  the specifed description.
   *  <P>
   *  All string columns will be declared as one longer than in dbDescription,
   *  since an E kernel database includes a trailing null character in its
   *  length, while most commercial databases do not.
   *  ResultSet, which should be the product of a DataBaseMetaData
   *  getColumns call.
   * @param dbDescription the result of a DataBaseMetaData.getColumns call
   * @throws SQLException if an error occurs while accessing the meta data
   */    
    public void set(java.sql.ResultSet dbDescription)
      throws java.sql.SQLException
    {      
      setName(dbDescription.getString("COLUMN_NAME"));
      short dataType = dbDescription.getShort("DATA_TYPE");
      switch (dataType)
      {
        case Types.NUMERIC:
        case (short)3:        // In testing, this showed up and appeared to
                              // be an integer, but it's not documentated.
          setDataType(Column.INTEGER);
          break;
          
        case Types.VARCHAR:
        case Types.CLOB:
        case (short)1111:     // In testing, this showed up and appeared to
                              // be an string, but it's not documentated.      
          setDataType(Column.STRING);
        
          // We add one to the length, because the E kernel counts
          // the trailing null character.
        
          setStringLength(dbDescription.getInt("COLUMN_SIZE") + 1);
          break;
        
        case Types.CHAR:
          setDataType(Column.STRING);
          
          // We add one to the length, because the E kernel counts
          // the trailing null character.
          
          setStringLength(2);
          
        case Types.DATE:
        case (short)93:       // In testing, this showed up and appeared to
                              // be a date, but it's not documentated.

          // TODO:  Transform this into an E kernel date instead of a string.                    
            
          setDataType(Column.STRING);
          setStringLength(80);
          break;
          
        case Types.DOUBLE:
          setDataType(Column.DOUBLE);
          break;
          
        default:
          throw new java.security.InvalidParameterException(
            "Column.set: In column " + getName() +
            ", type " + String.valueOf(dataType) +
            " (" + dbDescription.getString("TYPE_NAME") + 
            ") is not supported.");
          
      }
      switch (dbDescription.getInt("NULLABLE"))
      {
        case DatabaseMetaData.columnNullable:
          setNullsOk(true);
        break;
        
        case DatabaseMetaData.columnNullableUnknown:
          setNullsOk(true);
          System.out.println("Column.set:  Warning: column " + 
                    getName() + 
                    "'s nullability is unknown.");
        break;          
      }      
    }
    
    /** Test program.
     * @param argv parameters
     */
    public static final void main(String[] argv)
    {
      boolean allPassed = true;
      
      String d1 = "DATATYPE = INTEGER, SIZE = 1, INDEXED = FALSE, NULLS_OK = FALSE";
      String d2 = "DATATYPE = CHARACTER*(64), SIZE = 1, INDEXED = TRUE, NULLS_OK = FALSE";
      String d3 = "DATATYPE = CHARACTER*(*), SIZE = 1, INDEXED = TRUE, NULLS_OK = TRUE";
      String d4 = "DATATYPE = DOUBLE PRECISION, SIZE = 3, INDEXED = FALSE, NULLS_OK = TRUE";
      
      Column c = new Column();
      
      System.out.println("Default descriptor:");
      System.out.println(c.getDescriptor());
      if (c.getDescriptor().equals(d1))
      {
        System.out.println("OK");
      }
      else
      {
        System.out.println("Failed");
        allPassed = false;
      }
      
      System.out.println("With String of length 64, indexed");
      c.setDataType(Column.STRING);
      c.setStringLength(64);
      c.setIndexed(true);
      System.out.println(c.getDescriptor());
      if (c.getDescriptor().equals(d2))
      {
        System.out.println("OK");
      }
      else
      {
        System.out.println("Failed");
        allPassed = false;
      }      
      
      System.out.println("With String of variable length, indexed, and with nulls OK");
      c = new Column();
      c.setDataType(Column.STRING_VAR_LENGTH);
      c.setIndexed(true);
      c.setNullsOk(true);
      System.out.println(c.getDescriptor());
      if (c.getDescriptor().equals(d3))
      {
        System.out.println("OK");
      }
      else
      {
        System.out.println("Failed");
        allPassed = false;
      }

      System.out.println("With an array of 3 doubles, and with nulls OK");
      c = new Column();
      c.setDataType(Column.DOUBLE);
      c.setArrayLength(3);
      c.setIndexed(false);
      c.setNullsOk(true);
      System.out.println(c.getDescriptor());
      if (c.getDescriptor().equals(d4))
      {
        System.out.println("OK");
      }
      else
      {
        System.out.println("Failed");
        allPassed = false;
      }      
      
      System.out.println("Unit test " + (allPassed ? "passed." : "FAILED"));
    }
}