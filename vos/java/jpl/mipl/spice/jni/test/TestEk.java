package jpl.mipl.spice.jni.test;

import java.util.Iterator;
import java.util.ArrayList;

import jpl.mipl.spice.jni.*;
import java.sql.*;

/**
 *  Test class for the MIPL Java SPICE E kernel JNI wrappers.
 *  @author  Michael Brady
 *  @version
 */
public class TestEk
{
    private static java.io.File s_file = new java.io.File("test.ek");
    
    private static String[] FIRST_NAMES = new String[]{"Bob", "Jose"};
    private static String[] MIDDLE_INITIALS = new String[]{"Q", "F"};
    private static String[][] LAST_NAMES = new String[][]{
                                {"Jones", "Jr."}, 
                                {"Smith", "Duke of Wellington", "III"}};
    private static Double[] TIMES = new Double[]{new Double(-60.0), new Double(60.0)};
    private static String[] TIME_STRINGS = 
        new String[]{"JAN 01,2000  11:57:55.8160 (UTC)", 
                    "JAN 01,2000  11:59:55.8160 (UTC)"};
    private static Integer[] AGES = new Integer[]{new Integer(25), new Integer(45)};    
    
    private static int NUM_COLUMNS = 5;
    
    /**
     * @param args the command line arguments
     */
    public static void main (String args[])
    throws SpiceException, QueryException, ResultNotFoundException,
            InvalidSegmentException,
            java.io.IOException,
            java.sql.SQLException
    {
        loadLeapsecondsKernel();  
      
        if (s_file.exists())
        {
            System.out.println("Warning:  File '" + s_file.getName()
                                + "' exists.  Deleting...");
            s_file.delete();
        }
        
        testException();
        
        System.out.print("Creating '" + s_file.getName() + "'...");
        makeEk();
        System.out.println("OK.");
        
        System.out.print("Reading '" + s_file.getName() + "'...");
        readJdbc();
        System.out.println("OK.");

        s_file.delete();

        System.out.print("Creating updateable '" + s_file.getName() + "'...");
        makeUpdateableEk();
        System.out.println("OK.");

        System.out.print("Reading updateable '" + s_file.getName() + "'...");
        readJdbc();
        System.out.println("OK.");
        
        s_file.delete();
        
        System.out.println("All tests passed.");
    }
    
    static void testFailed(String reason)
    {
        System.err.println("Test failed. (" + reason + ")");
        System.exit(1);
    }
    
    static void loadLeapsecondsKernel()
        throws SpiceException
    {
        TextKernel leap = new TextKernel("/project/spice/ops/leapseconds.ker");
        if (!leap.exists())
        {
            System.out.println("Warning:  No leapseconds kernel found.");
            return;
        }
        KernelPool pool = new KernelPool();
        pool.load(leap);
    }
    
    static private void readJdbc()
    throws java.sql.SQLException, jpl.mipl.spice.jni.SpiceException
    {
        try
        {
            Class.forName("jpl.mipl.spice.jni.JdbcDriver");
        }
        catch (ClassNotFoundException e)
        {
            System.out.println("Class not found: " + e.toString());
            System.exit(1);
        }
        
        Connection con = DriverManager.getConnection("jdbc:ek:local:@" + s_file.getName());
        java.sql.Statement statement = con.createStatement();
        
        java.sql.ResultSet rs = statement.executeQuery("select * from USER_DATA");

        int row = -1;
        while (rs.next())
        {
            ++row;
            
            // Test the column count.
            
            if (rs.getMetaData().getColumnCount() != NUM_COLUMNS)
            {
                testFailed("Wrong number of columns.");
            }
                
            if (!rs.getString(0).equals(FIRST_NAMES[row]))
            {
                testFailed("FIRST_NAME in row " + row + " doesn't match. ['" + 
                            rs.getString(0) + "'!='" + FIRST_NAMES[row] + "']");
            }
            
            if (!rs.getString(1).equals(MIDDLE_INITIALS[row]))
            {
                testFailed("MIDDLE_INITAL in row " + row + " doesn't match. ['" + 
                            rs.getString(1) + "'!='" + MIDDLE_INITIALS[row] + "']");
            }            
            
            Object[] lastNames = (Object[])rs.getArray(2).getArray();
            for (int i=0; i < lastNames.length; ++i)
            {
                // Since this row is of length 15 only, 
                // we only compare the first 15 chars of the string.
                
                String target = LAST_NAMES[row][i];
                target = target.length() > 15 ? target.substring(0, 15) : target;
                
                if(!lastNames[i].toString().equals(target))
                {
                    testFailed("LAST_NAME in row " + row + " doesn't match. ['" +
                               lastNames[i] + "'!='" + target + "']");
                }
            }
            

            if (!rs.getObject(3).toString().equals(TIME_STRINGS[row]))
            {
                testFailed("TIME_STRING in row " + row + " doesn't match. ['" + 
                            rs.getString(3) + "'!='" + TIME_STRINGS[row] + "']");
            }            

            jpl.mipl.spice.jni.Time time = 
                ((jpl.mipl.spice.jni.JdbcResultSet)rs).getSpiceTime(3);
            
            if (time.asDouble() != TIMES[row].doubleValue())
            {
                testFailed("Ephemeris TIME in row " + row + 
                            " doesn't match. ['" + 
                            time.asDouble() + "'!='" + TIMES[row] + "']");                
            }
            
            if (!time.asString().equals(TIME_STRINGS[row]))
            {
                testFailed("Spice TIME_STRING in row " + row + 
                            " doesn't match. ['" + 
                            time.asString() + "'!='" + TIME_STRINGS[row] + "']");
            }             
            
            if (rs.getInt(4) != AGES[row].intValue())
            {
                testFailed("AGES in row " + row + " doesn't match. ['" + 
                            rs.getInt(4) + "'!='" + AGES[row] + "']");
            }            
        }
    }
    
    static private void testException()
    throws SpiceException
    {
        System.out.print("Testing SpiceException...");
        try
        {
            KernelPool pool = new KernelPool();
            pool.furnish(new java.io.File("doesnt_exist"));
        }
        catch (SpiceException e)
        {
           e.printStackTrace();
           System.out.println("SpiceException was thrown correctly.");
           System.out.println("SpiceException test... OK");
           return;
        }
        testFailed("No exception thrown.");
    }
    
    
   static private Table makeTable()
   {
      Table table = new Table("User_data");
      
      // A column of variable-length strings.
      
      Column c = new Column();
      c.setName("First_name");
      c.setDataType(Column.STRING_VAR_LENGTH);
      c.setArrayLength(1);
      c.setArrayLengthIsVariable(false);
      c.setNullsOk(false);
      
      table.addColumn(c);
      
      // A Column of fixed-length strings of size one.
      
      c = new Column();
      c.setName("Middle");
      c.setDataType(Column.STRING);
      c.setStringLength(1);
      
      table.addColumn(c);
      
      // A column of arrays of strings of length 15.
      // The arrays are of variable length.
      
      c = new Column();
      c.setName("Last_name");
      c.setDataType(Column.STRING);
      c.setStringLength(15);
      c.setArrayLengthIsVariable(true);
      c.setNullsOk(false);
        
      table.addColumn(c);
        
        // A column of type time.
        
      c = new Column();
      c.setName("Time");
      c.setDataType(Column.TIME);
        
      table.addColumn(c);
        
        // A column of type integer.
        
      c = new Column();
      c.setName("Ages");
      c.setDataType(Column.INTEGER);
        
      table.addColumn(c);

      return table;
   }
        
    static private void makeEk()
    throws SpiceException, InvalidSegmentException
    {
        Segment segment = new Segment(makeTable());
                
        ArrayList data = new ArrayList();
        for (int i=0; i < FIRST_NAMES.length; ++i)
            data.add(FIRST_NAMES[i]);
        
        segment.addData(data);
        
        data = new ArrayList();
        for (int i=0; i < MIDDLE_INITIALS.length; ++i)
            data.add(MIDDLE_INITIALS[i]);
        
        segment.addData(data);
        
        data = new ArrayList();
        ArrayList array1 = new ArrayList();
        for (int i=0; i < LAST_NAMES[0].length; ++i)
            array1.add(LAST_NAMES[0][i]);
        data.add(array1);
        ArrayList array2 = new ArrayList();
        for (int i=0; i < LAST_NAMES[1].length; ++i)
            array2.add(LAST_NAMES[1][i]);        
        data.add(array2);
        
        segment.addData(data);
        
        data = new ArrayList();
        for (int i=0; i < TIMES.length; ++i)
            data.add(TIMES[i]);
        
        segment.addData(data);
        
        data = new ArrayList();
        for (int i=0; i < AGES.length; ++i)
            data.add(AGES[i]);
        segment.addData(data);
        
        EKernel ek = new EKernel(s_file.getName(), s_file.getName(), 0);
        segment.addTo(ek);
        ek.close();
    }


   static private void makeUpdateableEk()
      throws SpiceException
   {
      EKernel ek = new EKernel(s_file.getName(), s_file.getName(), 0);
      SegmentRW seg = ek.createNewSegment(makeTable());
   
      for (int i=0; i < 2; ++i)
      {
         ArrayList record = new ArrayList();

         ArrayList cell = new ArrayList();
         cell.add(FIRST_NAMES[i]);
      
         record.add(cell);
      
         cell = new ArrayList();
         cell.add(MIDDLE_INITIALS[i]);
      
         record.add(cell);
        
         cell = new ArrayList();
         for (int j=0; j < LAST_NAMES[i].length; ++j)
            cell.add(LAST_NAMES[i][j]);
      
         record.add(cell);
      
         cell = new ArrayList();
         cell.add(TIMES[i]);
      
         record.add(cell);
      
         cell = new ArrayList();
         cell.add(AGES[i]);

         record.add(cell);
      
         int newRec = seg.appendRecord(record);
      }
   
      ek.close();
   }

}
