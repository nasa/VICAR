/*
 * JdbcConnection.java
 *
 * Created on November 2, 2000, 10:55 AM
 */

package jpl.mipl.spice.jni;

import java.sql.Blob;
import java.sql.Clob;
import java.sql.SQLClientInfoException;
import java.sql.SQLException;
import java.util.Map;
import java.util.Properties;
import java.util.concurrent.Executor;

/**
 *
 * @author  mpb
 * @version 
 */
public class JdbcConnection extends Object implements java.sql.Connection
{
    private KernelPool m_pool = new KernelPool();
    private boolean m_isClosed = false;

    /**
     *  Converts a comma-separated list of files to an array of file names.
     */
    private static String[] fileNameListToArray(String fileName)
    {
        java.util.ArrayList list = new java.util.ArrayList();
        int currIndex = 0;
        int commaIndex = fileName.indexOf(',', currIndex);
        while (commaIndex != -1)
        {
            list.add(fileName.substring(currIndex, commaIndex).trim());
            currIndex = commaIndex + 1;
            commaIndex = fileName.indexOf(',', currIndex);
        }

        // Add the last name in the list.

        list.add(fileName.substring(currIndex).trim());

        String[] ret = new String[list.size()];
        list.toArray(ret);
        return ret;    
    }

    /** Creates new JdbcConnection */
    public JdbcConnection(String fileName,final java.util.Properties props) throws java.sql.SQLException
    {
        try
        {
            String[] files = fileNameListToArray(fileName);
            for (int i=0; i < files.length; ++i)
            {
                m_pool.load(new EKernel(files[i]));
            }
        }
        catch (SpiceException e)
        {
            throw new java.sql.SQLException("SpiceException:  " + e.getMessage());
        }
    }

    public void setCatalog(java.lang.String p1) 
            throws java.sql.SQLException 
            {
        // As per the java.sql.Connection documentation,
        // we do not support catalogs, so we silently ignore this request.
            }
    public void close() 
            throws java.sql.SQLException
            {
        m_isClosed = true;
        try
        {
            m_pool.unloadAll();
        }
        catch(SpiceException e)
        {
            throw new java.sql.SQLException(e.getMessage());
        }
            }
    public void rollback() 
            throws java.sql.SQLException
            {
        throw new java.sql.SQLFeatureNotSupportedException();
            }
    public void clearWarnings() 
            throws java.sql.SQLException
            {
        throw new java.sql.SQLFeatureNotSupportedException();
            }
    public java.util.Map getTypeMap() 
            throws java.sql.SQLException
            {
        throw new java.sql.SQLFeatureNotSupportedException();
            }
    public int getTransactionIsolation() 
            throws java.sql.SQLException
            {
        throw new java.sql.SQLFeatureNotSupportedException();
            }
    public java.sql.PreparedStatement prepareStatement(java.lang.String p1) 
            throws java.sql.SQLException
            {
        throw new java.sql.SQLFeatureNotSupportedException();
            }
    public void setTransactionIsolation(int p1) 
            throws java.sql.SQLException
            {
        throw new java.sql.SQLFeatureNotSupportedException();
            }
    public boolean isClosed() 
            throws java.sql.SQLException
            {
        return m_isClosed;
            }
    public java.sql.Statement createStatement() 
            throws java.sql.SQLException
            {
        return m_pool.createStatement();
            }
    public java.sql.Statement createStatement(int p1,int p2) 
            throws java.sql.SQLException
            {
        throw new java.sql.SQLFeatureNotSupportedException();
            }
    public void setAutoCommit(boolean p1) 
            throws java.sql.SQLException
            {
        throw new java.sql.SQLFeatureNotSupportedException();
            }
    public java.sql.CallableStatement prepareCall(java.lang.String p1,int p2,int p3) 
            throws java.sql.SQLException
            {
        throw new java.sql.SQLFeatureNotSupportedException();
            }
    public void commit() 
            throws java.sql.SQLException
            {
        throw new java.sql.SQLFeatureNotSupportedException();
            }
    public java.lang.String getCatalog() 
            throws java.sql.SQLException
            {
        return null;
            }
    public java.sql.PreparedStatement prepareStatement(java.lang.String p1,int p2,int p3) 
            throws java.sql.SQLException
            {
        throw new java.sql.SQLFeatureNotSupportedException();
            }
    public boolean isReadOnly() 
            throws java.sql.SQLException
            {
        return true;
            }
    public void setReadOnly(boolean p1) 
            throws java.sql.SQLException
            {
        throw new java.sql.SQLFeatureNotSupportedException();
            }
    public java.sql.DatabaseMetaData getMetaData() 
            throws java.sql.SQLException
            {
        throw new java.sql.SQLFeatureNotSupportedException();
            }
    public boolean getAutoCommit() 
            throws java.sql.SQLException
            {
        throw new java.sql.SQLFeatureNotSupportedException();
            }
    public java.lang.String nativeSQL(java.lang.String p1) 
            throws java.sql.SQLException
            {
        throw new java.sql.SQLFeatureNotSupportedException();    
            }
    public java.sql.CallableStatement prepareCall(java.lang.String p1) 
            throws java.sql.SQLException
            {
        throw new java.sql.SQLFeatureNotSupportedException();
            }
    public java.sql.SQLWarning getWarnings() 
            throws java.sql.SQLException
            {
        throw new java.sql.SQLFeatureNotSupportedException();
            }
    public java.sql.Statement createStatement(int p1,int p2, int p3) 
            throws java.sql.SQLException
            {
        throw new java.sql.SQLFeatureNotSupportedException();
            }
    public java.sql.CallableStatement prepareCall(java.lang.String p1,int p2,int p3, int p4) 
            throws java.sql.SQLException
            {
        throw new java.sql.SQLFeatureNotSupportedException();
            }
    public java.sql.PreparedStatement prepareStatement(java.lang.String p1,java.lang.String[] p2) 
            throws java.sql.SQLException
            {
        throw new java.sql.SQLFeatureNotSupportedException();
            }
    public java.sql.PreparedStatement prepareStatement(java.lang.String p1,int[] p2) 
            throws java.sql.SQLException
            {
        throw new java.sql.SQLFeatureNotSupportedException();
            }
    public java.sql.PreparedStatement prepareStatement(java.lang.String p1,int p2) 
            throws java.sql.SQLException
            {
        throw new java.sql.SQLFeatureNotSupportedException();
            }
    public java.sql.PreparedStatement prepareStatement(java.lang.String p1,int p2,int p3, int p4) 
            throws java.sql.SQLException
            {
        throw new java.sql.SQLFeatureNotSupportedException();
            }
    public int getHoldability() 
            throws java.sql.SQLException
            {
        throw new java.sql.SQLFeatureNotSupportedException();
            }
    public void setHoldability(int p1) 
            throws java.sql.SQLException
            {
        throw new java.sql.SQLFeatureNotSupportedException();
            }
    public void rollback(java.sql.Savepoint p1) 
            throws java.sql.SQLException
            {
        throw new java.sql.SQLFeatureNotSupportedException();
            }
    public void releaseSavepoint(java.sql.Savepoint p1) 
            throws java.sql.SQLException
            {
        throw new java.sql.SQLFeatureNotSupportedException();
            }
    public java.sql.Savepoint setSavepoint() 
            throws java.sql.SQLException
            {
        throw new java.sql.SQLFeatureNotSupportedException();
            }
    public java.sql.Savepoint setSavepoint(String name) 
            throws java.sql.SQLException
            {
        throw new java.sql.SQLFeatureNotSupportedException();
            }
    public java.sql.Struct createStruct(String typeName, Object[] attributes) 
            throws java.sql.SQLException
            {
        throw new java.sql.SQLFeatureNotSupportedException();
            }
    public java.sql.Array createArrayOf(String typeName, Object[] elements) 
            throws java.sql.SQLException
            {
        throw new java.sql.SQLFeatureNotSupportedException();
            }
    public Properties getClientInfo()
            throws java.sql.SQLException
            {
        throw new java.sql.SQLFeatureNotSupportedException();
            } 

    public boolean isValid(int timeout)
            throws java.sql.SQLException
            {
        throw new java.sql.SQLFeatureNotSupportedException();
            }
    public java.sql.SQLXML createSQLXML()
            throws java.sql.SQLException
            {
        throw new java.sql.SQLFeatureNotSupportedException();
            }
    public java.sql.NClob createNClob()
            throws java.sql.SQLException
            {
        throw new java.sql.SQLFeatureNotSupportedException();
            }

    @Override
    public Blob createBlob() throws SQLException {
        throw new java.sql.SQLFeatureNotSupportedException();
    }

    @Override
    public Clob createClob() throws SQLException {
        throw new java.sql.SQLFeatureNotSupportedException();
    }

    @Override
    public void setClientInfo(Properties arg0) throws SQLClientInfoException {

    }

    @Override
    public void setTypeMap(Map<String, Class<?>> arg0) throws SQLException {
        throw new java.sql.SQLFeatureNotSupportedException();

    }

    @Override
    public boolean isWrapperFor(Class<?> arg0) throws SQLException {
        throw new java.sql.SQLFeatureNotSupportedException();
    }

    @Override
    public <T> T unwrap(Class<T> arg0) throws SQLException {
        throw new java.sql.SQLFeatureNotSupportedException();
    }

    @Override
    public String getClientInfo(String name) throws SQLException {
        throw new java.sql.SQLFeatureNotSupportedException();
    }

    @Override
    public void setClientInfo(String name, String value)
            throws SQLClientInfoException {
    }


    public void setSchema(String schema) throws SQLException {
        throw new java.sql.SQLFeatureNotSupportedException();

    }


    public String getSchema() throws SQLException {
        throw new java.sql.SQLFeatureNotSupportedException();
    }


    public void abort(Executor executor) throws SQLException {
        throw new java.sql.SQLFeatureNotSupportedException();

    }


    public void setNetworkTimeout(Executor executor, int milliseconds)
            throws SQLException {
        throw new java.sql.SQLFeatureNotSupportedException();

    }


    public int getNetworkTimeout() throws SQLException {
        throw new java.sql.SQLFeatureNotSupportedException();
    }

}
