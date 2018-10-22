/*
 * Testit.java
 *
 * Created on August 15, 2000, 4:00 PM
 */

package jpl.mipl.spice.jni;

import java.util.Iterator;
import java.util.ArrayList;

import java.sql.*;

/**
 *
 * @author  mpb
 * @version 
 */
public class Testit extends Object 
{
      
    /** Creates new Testit */
    public Testit() 
    {
    }
    
    /**
     * @param args the command line arguments
     */
    public static void main (String args[])
	throws SpiceException, QueryException, ResultNotFoundException, 
                InvalidSegmentException,
	       java.io.IOException,
	       java.sql.SQLException
    {
        makeEk();
        
        //SpiceLib.ldpool("/home2/io/mpb/ek/cassini.tsc");        
        //SpiceLib.ldpool("/home2/io/mpb/ek/naif0007.tls");
        // SpacecraftClockTime(-82, "1336262957");
        //System.out.println("Sclk for -82 is " + String.valueOf(
        //    (new EphemerisTime(SpiceLib.scs2e(-82, ))).asString()));
    }

    
    static void readJdbc()
      throws java.sql.SQLException
    {
      try
      {
       Class.forName("jpl.mipl.spice.JdbcDriver");
      }
      catch (ClassNotFoundException e)
      {
        System.out.println("Class not found: " + e.toString());
        System.exit(1);
      }
      
      Connection con = DriverManager.getConnection("jdbc:ek:local:@test.ek");
      java.sql.Statement statement = con.createStatement();
      
      java.sql.ResultSet rs = statement.executeQuery("select * from USER_DATA");
      while (rs.next())
      {
        for(int i=0; i < rs.getMetaData().getColumnCount(); ++i)
        System.out.println(rs.getObject(i));
      }
    }
      
    static void readOracle()
	throws SpiceException, QueryException, ResultNotFoundException, 
	       java.io.IOException,
	       java.sql.SQLException      
    {
	try
	{
	    Class.forName( "oracle.jdbc.driver.OracleDriver" );
	}
        catch (ClassNotFoundException e)
	{
	    System.out.println("Exception loading driver: " + e.toString());
	    System.exit(1);
        }
        
	
System.out.print("Getting connection...");        
        
	Connection con = DriverManager.getConnection(
	      "jdbc:oracle:thin:@eis-dta-011.jpl.nasa.gov:1526:CSINIDEV",
	      "mbrady",
	      "mbrady" );
	
System.out.println("done.");        
        
	DatabaseMetaData md = con.getMetaData();
	java.sql.ResultSet tables = md.getTables(null, "CUPID", null, null);
	
System.out.println("Tables:");        
	
	while(tables.next())
	{
	   for (int i=1; i <= 5; ++i)
	      System.out.print(tables.getString(i) + " | ");
	   System.out.println();
	}
	
        
    }
    
    private static void testSummary()
       throws SpiceException
    {          
       EKernel ek = new EKernel("Cupid2Ek.ek", EKernel.READ);
       
       for (int i=0; i < ek.getNumSegments(); ++i)
       {
	  Table table = ek.getTable(i);
	  
	  System.out.println("Table name: " + table.getName());
	  
	  for (Iterator it = table.getColumnIterator(); it.hasNext(); )
	  {
	     Column column = (Column)it.next();
	     System.out.print( "Column " + column.getName() + ": ");
	     System.out.println( column.getDescriptor());
	     System.out.println();
	  }
       }
       ek.close();      
    }
      
      static void testQuery()
	 throws SpiceException, QueryException, ResultNotFoundException,
                  java.sql.SQLException
      {
	 //String query = "SELECT LATTRIBUTEID, LACTIVITYTYPEID, SATTRIBUTENAME, SATTRIBUTEDEFVALUE FROM APGEN_AT_ATTRIBUTE_TABLE";      
	 String query = "select ntargetid, stargetname from target_table";
	 
	 
	 EKernel ek = new EKernel("Cupid2Ek.ek");
	 KernelPool pool = new KernelPool();
	 pool.load(ek);
	 jpl.mipl.spice.jni.JdbcStatement s = pool.createStatement();
	 ResultSet rs = (ResultSet) s.executeQuery(query);
	 while (rs.next())
	 {
	    System.out.print("rs: ");
	    System.out.print(rs.getInt(0));
//        System.out.println(" " + rs.getString(1));
	 }
	 
	 Table target_table = ek.getTable("TARGET_TABLE");
	 System.out.println("Table name: " + target_table.getName());
      }
      
      static void testException()
	 throws SpiceException
      {
	 try
	 {
	    EKernel ek = new EKernel("doesnt_exist", "internal", 0);
	    System.out.println("opened");
	 }
	 catch (SpiceException e)
	 {
	    System.out.println(e);
	 }
      }

      
  static void makeEk()
      throws SpiceException, InvalidSegmentException
  {    
    Table table = new Table("User_data");
    
    Column c = new Column();
    c.setName("First_name");
    c.setDataType(Column.STRING_VAR_LENGTH);
    c.setArrayLength(1);
    c.setArrayLengthIsVariable(false);
    c.setNullsOk(false);
    
    table.addColumn(c);

    c = new Column();
    c.setName("Middle");
    c.setDataType(Column.STRING);
    c.setStringLength(1);
    
    table.addColumn(c);  
    
    c = new Column();
    c.setName("Last_name");
    c.setDataType(Column.STRING);
    c.setStringLength(15);
    c.setArrayLengthIsVariable(true);
    c.setNullsOk(false);
    
    table.addColumn(c);    
    
    c = new Column();
    c.setName("Time");
    c.setDataType(Column.TIME);
    
    table.addColumn(c);    
    
    c = new Column();
    c.setName("Ages");
    c.setDataType(Column.INTEGER);
    
    table.addColumn(c);

    Segment segment = new Segment(table);
    
    ArrayList data = new ArrayList();
    data.add("Bob");
    data.add("Jose");

    segment.addData(data);  
    
    data = new ArrayList();
    data.add("Q");
    data.add("F");        

    segment.addData(data);    
    
    data = new ArrayList();
    ArrayList array1 = new ArrayList();
    array1.add("Jones");
    array1.add("Jr.");
    data.add(array1);
    ArrayList array2 = new ArrayList();
    array2.add("Smith");
    array2.add("Duke of Wellington");
    array2.add("III");
    data.add(array2);
    
    segment.addData(data);

    data = new ArrayList();
    data.add(new Double(60.0));
    data.add(new Double(-60.0));
    
    segment.addData(data);    
    
    data = new ArrayList();
    for (int i=0; i < 2; ++i)
    {
      data.add(new Integer(i < 5 ? i : 100 - i));
    }
    segment.addData(data);
    
    EKernel ek = new EKernel("test.ek", "test.ek", 0);
    segment.addTo(ek);
    ek.close();
  }
}
