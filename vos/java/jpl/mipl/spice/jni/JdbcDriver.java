/*
 * JdbcDriver.java
 *
 * Created on November 2, 2000, 10:36 AM
 */

package jpl.mipl.spice.jni;

import java.sql.SQLFeatureNotSupportedException;
import java.util.logging.Logger;

/**
 *
 * @author  mpb
 * @version 
 */
public class JdbcDriver extends Object implements java.sql.Driver
{
  private static final String URL_PREFIX = "jdbc:ek:local:@";
  
  private static java.sql.DriverPropertyInfo[] s_propertyInfo = null;

  static
  {
    try
    {
      java.sql.DriverManager.registerDriver(new JdbcDriver());
    }
    catch (java.sql.SQLException e)
    {
      System.out.println("Exception while loading JdbcDriver: " +
          e.toString());
      e.printStackTrace(System.out);
    }
  }
  
  
    /** Creates new JdbcDriver */
    public JdbcDriver() 
    {
    }

    public int getMinorVersion() 
    {
      return 0;
    }
    
    public int getMajorVersion() 
    {
      return 1;
    }
    
    public 
    java.sql.DriverPropertyInfo[] 
    getPropertyInfo(java.lang.String p1,
                      final java.util.Properties p2) 
      throws java.sql.SQLException 
    {
      if (s_propertyInfo == null)
      {
        s_propertyInfo = new java.sql.DriverPropertyInfo[0];
      }
      return s_propertyInfo;
    }
    
    public boolean jdbcCompliant() 
    {
      return false;
    }
    
    public java.sql.Connection connect(java.lang.String url,
                                         final java.util.Properties props) 
      throws java.sql.SQLException 
    {      
      if (!acceptsURL(url))
        return null;
     
      String fileName = url.substring(URL_PREFIX.length());
      
      return new JdbcConnection (fileName, props);
    }
    
    public boolean acceptsURL(java.lang.String url) 
      throws java.sql.SQLException 
    {
      return url.startsWith(URL_PREFIX);   
    }

    public Logger getParentLogger() throws SQLFeatureNotSupportedException {
        throw new java.sql.SQLFeatureNotSupportedException();
    }
}