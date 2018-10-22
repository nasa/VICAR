/*
 * Database2Ek.java
 */

package jpl.mipl.spice.jni.database2ek;

import jpl.mipl.spice.jni.*;

import java.util.Properties;
import java.util.ArrayList;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;

import java.sql.*;


/**
 *  The main class for the application Database2Ek.
 *  @author  Michael Brady
 */
public class Database2Ek
{

  private static final String HELP_MSG =
	
    "Database2Ek reads tables from a database schema and writes equivalent E kernels.\n"+
    "\n" +
    "Several parameters are required, and can be specified on the command line,\n" +
    "or in a file.  Parameters on the comand line supersede those read from\n" +
    "a file.  The file name can be specified with the -f option, and is\n" +
    "Database2Ek.cfg by default.\n" +
    "\n" +
    "Parameters in a file should be ready for the Properties class load method:\n" +
    "they should be one per line, separated by '='.  The '#' character\n" + 
    "denotes a comment line. For example:\n" +
    "\n" +
    "\n" +
    "# Database2Ek.cfg\n" +
    "#\n" +
    "# Writes columns of all current CUPID tables to an E kernel.\n" +
    "#\n" +
    "DRIVER_NAME=oracle.jdbc.driver.OracleDriver\n" +
    "DRIVER_TYPE=jdbc:oracle:thin:\n" +
    "HOST=eis-dta-011.jpl.nasa.gov\n" +
    "PORT=1526\n" +
    "SID=CSINIDEV\n" +
    "USER_NAME=mbrady\n" +
    "SCHEMA=CUPID\n" +
    "# end of file\n" +
    
    "\n" +
    "or:\n" +
    "\n" +
    
    "# Example Database2Ek.cfg file\n" +
    "HOST=yahoo.com\n" +
    "PORT=5134\n" +
    "# end of file\n" +
    "\n" +      
    "  Parameters:\n" +
    "\n" +
    "    DRIVER_NAME   the JDBC driver to use to connect to the database.\n" +
    "                  example:  oracle.jdbc.driver.OracleDriver\n" +
    "    DRIVER_TYPE   the JDBC driver type. Example: jdbc:oracle:thin:\n" +
    "    HOST          the host machine which will accept the JDBC connection\n" +
    "    PORT          the port number on which to contact the host\n" +
    "    SID           the SID  Example:  CSINIDEV\n"  +
    "    USER_NAME     user name for database access\n" +
    "    PASSWORD      password for database access\n" +
    "    SCHEMA        the schema from which to read\n" +
    "    TABLEx        the tables from which to read, where x is a number\n" +
    "                  starting with 1 and increasing to n the number of tables\n" +
    "                  If no tables are listed, all will be read.\n" +
    "    TABLExCOLUMNy specifies which column 'y' to read from table 'x'.\n" +
    "                  If no columns are listed for a table, all will be read.\n" +
    "\n" +
    "  Options:\n" +
    "\n" +      
    "  -ext file_ext  appends .'file_ext' to each file, rather than \n" +
    "                 the default .ek\n" +
    "  -f file_name   reads parameters from the file 'file_name'.\n" +
    "                 Parameters specified on the command line supercede \n" +
    "                 those found in a file.\n" +
    "  -help          write this message.\n" +
    "  -o file_name   the name of the output file.  If -table is specified\n " +
    "                 the table name will be appended to 'file_name'.\n" +
    "  -table         write one E kernel file per table. \n" +
    "\n" +
    "  Examples:\n" +      
    "\n" +
    "java Database2Ek -o dbdump -table HOST altavista.com PORT 3195\n" +
    "\n";      
      
    private static final String[] REQUIRED_PROPS = {
                                                          "DRIVER_NAME",
                                                          "DRIVER_TYPE",
                                                          "HOST",
                                                          "PORT",
                                                          "SID",
                                                          "USER_NAME",
                                                          "SCHEMA"
                                                      };
                                                          
    private static final String PASSWORD = "PASSWORD";
  
    private static String s_paramFile = "Database2Ek.cfg";
    private static String s_outFileBase = "Database2Ek";
    private static String s_outFileExt = "ek";
    private static boolean s_filePerTable = false;
    
    private static Properties s_props = new Properties();
  
  
  /** Creates new Database2Ek */
  public Database2Ek() 
  {
  }

    /**
     * @param args the command line arguments
     */
    public static void main (String args[]) 
    {
      s_props = loadProperties();
      s_props = processParams(args, s_props);
      checkForRequiredProps();
      while (s_props.getProperty(PASSWORD) == null)
      {
        readPassword();
      }
      execute(); 
    }
    
    
    /**
     * Exits if the required properties have not been specified.
     */
    private static void checkForRequiredProps()
    {
      for (int i=0; i < REQUIRED_PROPS.length; ++i)
      {
        if (s_props.getProperty(REQUIRED_PROPS[i]) == null)
        {
          System.out.println("The required property " + REQUIRED_PROPS[i] +
            " is missing.  You may specify this in your configuration file" +
            " or on the command line.  " +
            "Try Database2Ek -help for more information.");
          System.exit(1);
        }
      }
    }
    
    /**
     * @param fromFile the properites which have been read from the config
     *                 file.
     * @return the properites from the command line, with the config file
     *         
     */
    private static Properties processParams(String args[], Properties fromFile)
    {
      ArrayList argsList = new ArrayList();
      for (int i=0; i < args.length; ++i)
      {
        argsList.add(args[i]);
      }
      
      if (argsList.size() > 0)
      {
        String arg = (String)argsList.get(0);
        while (arg.charAt(0) == '-')
        {
          if (arg.equals("-help"))
          {
            System.out.println(HELP_MSG);
            System.exit(0);
          }
          else if (arg.equals("-ext"))
          {
            argsList.remove(0);
            try
            {
             s_outFileExt = (String)argsList.get(0);
            }
            catch(Exception e)
            {
              System.out.println("-ext must be followed by an extension name");
              System.exit(1);
            }          
          }
          else if (arg.equals("-f"))
          {
            argsList.remove(0);
            try
            {
             s_paramFile = (String)argsList.get(0);
            }
            catch(Exception e)
            {
              System.out.println("-f must be followed by a file name");
              System.exit(1);
            }          
          }
          else if (arg.equals("-o"))
          {
            argsList.remove(0);
            try
            {
             s_outFileBase = (String)argsList.get(0);
            }
            catch(Exception e)
            {
              System.out.println("-o must be followed by a file name");
              System.exit(1);
            }  
          }
          else if (arg.equals("-table"))
          {
            s_filePerTable = true;
          }
          else 
          {
            System.out.println("Argument '" + arg + 
                    "' is not valid.  Try 'Database2Ek -help'.");
            System.exit(1);
          }
          argsList.remove(0);
          if (argsList.size() < 1)
          {
            break;
          }
          arg = (String)argsList.get(0);
        }
      }
      
      
      Properties props = new Properties(fromFile);
      
      while (argsList.size() > 1)
      {
        String key   = (String)argsList.remove(0);
        String value = (String)argsList.remove(0);
      
        props.setProperty(key, value);
      }
      
      return props;
    }
    
    public static Properties loadProperties()
    {
        Properties props = new Properties();
        try
        {
            props.load(new FileInputStream(s_paramFile));
        }
        catch (IOException e)
        {
           System.out.println("Exception opening file " + s_paramFile +
                 ": " + e.toString());
           System.exit(1);            
        }
        return props;
    }

    private static void readPassword()
    {
      
      System.out.print("Password: ");
      try
      {
        String password = new java.io.BufferedReader(
              new java.io.InputStreamReader(System.in)).readLine();
        s_props.setProperty(PASSWORD, password);
      }
      catch (java.io.IOException e)
      {
      }
    }
    
    private static String connectionString()
    {
      return s_props.getProperty("DRIVER_TYPE") + 
                 "@" +
                 s_props.getProperty("HOST") +
                  ":" +
                 s_props.getProperty("PORT") +
                 ":" +
                 s_props.getProperty("SID");     
    }

    
  /**
   *  Creates a comma-separated string of the specified names.
   */
  private static String createList(String[] names)
  {
    if (names == null)
    {
      return "*";
    }
    
    StringBuffer ret = new StringBuffer("");
    for (int i=0; i < names.length; ++i)
    {
      if (i != 0)
      {
        ret.append(", ");
      }
      ret.append(names[i]);
    }
    return ret.toString();
  }
  
  /**
   *  Returns the columns which the user has list for the specified table.
   *  If the user has not listed any columns, returns null.
   *  @param tableNum the index of the table (i.e. "3" returns all
   *  columns listed as TABLE3COLUMN*)
   */
  private static String[] columnsForTable(int tableNum)
  {
    ArrayList names = new ArrayList();
    int columnNum = 1;
    String columnName = s_props.getProperty("TABLE" + 
                                              String.valueOf(tableNum) +
                                              "COLUMN" + 
                                              String.valueOf(columnNum));
    while (columnName != null)
    { 
      names.add(columnName);
      
      ++columnNum;
      columnName = s_props.getProperty("TABLE" + 
                                              String.valueOf(tableNum) +
                                              "COLUMN" + 
                                              String.valueOf(columnNum));
    }
    
    if (names.size() < 1)
    {
      return null;
    }
    
    String[] ret = new String[names.size()];
    names.toArray(ret);
    return ret;
  }
    
  /**
   *  @param columnNames if null, all columns will be used
   */
  private static void table2ek(Connection con, 
                                  String tableName, 
                                  String[] columnNames)
    throws SQLException, SpiceException
  {
    String fullTableName = s_props.getProperty("SCHEMA") + "." +
                           tableName;
    System.out.println("Converting " + fullTableName + "...");
     
    String columnNameList = createList(columnNames);
    String query = "select " + columnNameList + " from " + fullTableName;
                          
                         
    java.sql.Statement statement = con.createStatement();
    java.sql.ResultSet results = statement.executeQuery(query);

    java.sql.ResultSetMetaData meta = results.getMetaData();        
      
    String fileName = null;
           
    if (s_filePerTable)
    {
       fileName= s_outFileBase + "_" + tableName +
                 "." + s_outFileExt;
    }
    else
    {
      fileName = s_outFileBase + "." + s_outFileExt;
    }

    EKernel ek = new EKernel(fileName);
    if (ek.exists())
    {
      ek.open(EKernel.READ_WRITE);
    }
    else
    {
      ek.create(fileName, 0);
    }

    try
    {
      if (columnNames == null)
      {
        columnNames = new String[1];
        columnNames[0] = null;
      }      
      Table table = new Table(tableName);
      for (int i=0; i < columnNames.length; ++i)
      {
        java.sql.ResultSet dbDescription = 
          con.getMetaData().getColumns(null, 
                                          s_props.getProperty("SCHEMA"), 
                                          tableName, 
                                          columnNames[i]);
        while (dbDescription.next())
        {        
          Column column = new Column();
          column.set(dbDescription);
          table.addColumn(column);
        }
      }
               
      Segment segment = new Segment(table);
      segment.setData(results);

      segment.addTo(ek);          
    } 
    catch (InvalidSegmentException e)
    {
      System.out.println("Caught: " + e.toString());
      System.out.println("Table " + tableName + " aborted.");
    }
    finally
    {
      ek.close();
    }
  }
    
    public static void execute()
    {
        try
        {
          Class.forName( s_props.getProperty("DRIVER_NAME") );
        }
        catch (ClassNotFoundException e)
        {
           System.out.println("Exception loading driver: " + e.toString());
           System.exit(1);
        }
        
        try
        {
          Connection con = DriverManager.getConnection(connectionString(),
                                            s_props.getProperty("USER_NAME"),
                                            s_props.getProperty(PASSWORD) );

          // If there are tables listed in the config file, use those....
          
          int tableNum = 1;
          String tableName = s_props.getProperty("TABLE" + 
                                                  String.valueOf(tableNum));
          while (tableName != null)
          { 
            String[] columnNames = columnsForTable(tableNum);
            table2ek(con, tableName, columnNames);

            ++tableNum;
            tableName = s_props.getProperty("TABLE" + 
                                              String.valueOf(tableNum));            
          }
          
          if (tableNum > 1)
          {
            return;
          }
          
          // ...otherwise, do all tables listed in the DB metadata.
          
          DatabaseMetaData md = con.getMetaData();
          java.sql.ResultSet tables = md.getTables(null, 
                                               s_props.getProperty("SCHEMA"), 
                                               null, 
                                               null);
          
          while(tables.next())
          {
            tableName = tables.getString(3);
            table2ek(con, tableName, null);
          }
        }
        catch (SpiceException e)
        {
           System.out.println("Caught SpiceException: " + e.toString());
           System.exit(1);          
        }
        catch (SQLException e)
        {
           System.out.println("Caught SQLException: " + e.toString());
           System.exit(1);
        }        
     }
}
