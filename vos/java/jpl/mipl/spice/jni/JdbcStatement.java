/*
 * Statement.java
 *
 * Created on August 18, 2000, 9:41 AM
 */

package jpl.mipl.spice.jni;

import jpl.mipl.spice.jni.KernelPool;
import jpl.mipl.spice.jni.SpiceLib;

import java.sql.SQLException;
import java.util.Iterator;

/**
 * @author Michael Brady
 */
public class JdbcStatement extends Object implements java.sql.Statement
{

    static private final String SELECT_ALL = "SELECT * ";
    static private final String FROM = "FROM ";    

    private KernelPool m_pool = null;


    /** Creates new Statement */
    JdbcStatement(KernelPool pool) 
    {
        m_pool = pool;
    }

    public java.sql.ResultSet executeQuery(java.lang.String sql)
            throws java.sql.SQLException
            {
        try
        {
            if (sql.toUpperCase().startsWith(SELECT_ALL))
            {
                return executeSelectAll(sql.substring(SELECT_ALL.length()).trim());
            }

            return new jpl.mipl.spice.jni.JdbcResultSet(m_pool, sql, 
                    SpiceLib.ekfind(sql));
        }
        catch (SpiceException e)
        {
            throw new java.sql.SQLException("SpiceException: " + e.getMessage());
        }
        catch (QueryException e)
        {
            throw new java.sql.SQLException("QueryException: " + e.getMessage());
        }      
            }

    /** Executes a statment, prepending "SELECT * " to the specified sql string.
     * @param sql should begin with the word "FROM"
     * @throws SpiceException
     * @throws QueryException
     * @return
     */
    public jpl.mipl.spice.jni.JdbcResultSet executeSelectAll(String sql)
            throws java.sql.SQLException
            {
        try
        {
            if (!sql.toUpperCase().startsWith(FROM))
            {
                throw new java.security.InvalidParameterException(
                        "ResultSet.executeSelectAll: query must begin with 'FROM '.");        
            }

            int tableNameEnd = sql.indexOf(' ', FROM.length());
            if (tableNameEnd == -1)
            {
                tableNameEnd = sql.length();
            }    
            String tableName = sql.substring(FROM.length(), tableNameEnd).
                    trim();

            Table table = m_pool.getTable(tableName);
            if (table == null)
            {
                throw new java.security.InvalidParameterException(
                        "ResultSet.executeSelectAll: no table '" + tableName +
                        "' currently loaded.");        
            }

            return executeSelectAll(table, 
                    sql.substring(tableNameEnd).trim());
        }
        catch (SpiceException e)
        {
            throw new java.sql.SQLException("SpiceException:  " + e.getMessage()); 
        }
            }

    /** Executes a "SELECT * FROM table" for the specified table.
     * @param table
     * @param modifiers will be appended to the query.
     *                  May contain WHERE or ORDER BY statements.
     *                  If null, will be ignored.
     * @throws SpiceException
     * @throws QueryException
     * @return
     */
    public jpl.mipl.spice.jni.JdbcResultSet 
    executeSelectAll(jpl.mipl.spice.jni.Table table, String modifiers)
            throws java.sql.SQLException
            {
        if (table.getNumColumns() < 1)
        {
            throw new java.security.InvalidParameterException(
                    "ResultSet.executeSelectAll: table must have at least one column.");
        }

        StringBuffer query = new StringBuffer("SELECT");

        Iterator it = table.getColumnIterator();
        while (it.hasNext())
        {
            Column column = (Column)it.next();
            query.append(' ');
            query.append(column.getName());
            query.append(',');
        }
        query.setCharAt(query.length() - 1, ' ');  // Remove the trailing comma.

        query.append("FROM ");
        query.append(table.getName());

        if (modifiers != null)
        {
            query.append(" ");
            query.append(modifiers);
        }

        return (jpl.mipl.spice.jni.JdbcResultSet) executeQuery(query.toString());
            }

    public int getMaxRows() throws java.sql.SQLException {
        throw new java.sql.SQLFeatureNotSupportedException();
    }
    public java.sql.Connection getConnection() throws java.sql.SQLException {
        throw new java.sql.SQLFeatureNotSupportedException();
    }
    public int getResultSetType() throws java.sql.SQLException {
        throw new java.sql.SQLFeatureNotSupportedException();
    }
    public int executeUpdate(java.lang.String p1) throws java.sql.SQLException {
        throw new java.sql.SQLFeatureNotSupportedException();
    }
    public void setFetchDirection(int p1) throws java.sql.SQLException {
        throw new java.sql.SQLFeatureNotSupportedException();
    }
    public void setCursorName(java.lang.String p1) throws java.sql.SQLException {
        throw new java.sql.SQLFeatureNotSupportedException();
    }
    public void setMaxRows(int p1) throws java.sql.SQLException {
        throw new java.sql.SQLFeatureNotSupportedException();
    }
    public void setFetchSize(int p1) throws java.sql.SQLException {
        throw new java.sql.SQLFeatureNotSupportedException();
    }
    public int getFetchSize() throws java.sql.SQLException {
        throw new java.sql.SQLFeatureNotSupportedException();
    }
    public void setMaxFieldSize(int p1) throws java.sql.SQLException {
        throw new java.sql.SQLFeatureNotSupportedException();
    }
    public void clearBatch() throws java.sql.SQLException {
        throw new java.sql.SQLFeatureNotSupportedException();
    }
    public int[] executeBatch() throws java.sql.SQLException {
        throw new java.sql.SQLFeatureNotSupportedException();
    }
    public boolean execute(java.lang.String p1) throws java.sql.SQLException {
        throw new java.sql.SQLFeatureNotSupportedException();
    }
    public int getQueryTimeout() throws java.sql.SQLException {
        throw new java.sql.SQLFeatureNotSupportedException();
    }
    public int getResultSetConcurrency() throws java.sql.SQLException {
        throw new java.sql.SQLFeatureNotSupportedException();
    }
    public void cancel() throws java.sql.SQLException {
        throw new java.sql.SQLFeatureNotSupportedException();
    }
    public int getUpdateCount() throws java.sql.SQLException {
        throw new java.sql.SQLFeatureNotSupportedException();
    }
    public void addBatch(java.lang.String p1) throws java.sql.SQLException {
        throw new java.sql.SQLFeatureNotSupportedException();
    }
    public int getMaxFieldSize() throws java.sql.SQLException {
        throw new java.sql.SQLFeatureNotSupportedException();
    }
    public void setEscapeProcessing(boolean p1) throws java.sql.SQLException {
        throw new java.sql.SQLFeatureNotSupportedException();
    }
    public boolean getMoreResults() throws java.sql.SQLException {
        throw new java.sql.SQLFeatureNotSupportedException();
    }
    public java.sql.ResultSet getResultSet() throws java.sql.SQLException {
        throw new java.sql.SQLFeatureNotSupportedException();
    }
    public java.sql.SQLWarning getWarnings() throws java.sql.SQLException {
        throw new java.sql.SQLFeatureNotSupportedException();
    }
    public void clearWarnings() throws java.sql.SQLException {
        throw new java.sql.SQLFeatureNotSupportedException();
    }
    public void setQueryTimeout(int p1) throws java.sql.SQLException {
        throw new java.sql.SQLFeatureNotSupportedException();
    }
    public int getFetchDirection() throws java.sql.SQLException {
        throw new java.sql.SQLFeatureNotSupportedException();
    }
    public void close() throws java.sql.SQLException {
        throw new java.sql.SQLFeatureNotSupportedException();
    }
    public int executeUpdate(java.lang.String[] p1) throws java.sql.SQLException {
        throw new java.sql.SQLFeatureNotSupportedException();
    }
    public int executeUpdate(java.lang.String p1, java.lang.String[] p2) throws java.sql.SQLException {
        throw new java.sql.SQLFeatureNotSupportedException();
    }
    public int executeUpdate(java.lang.String p1, int[] p2) throws java.sql.SQLException {
        throw new java.sql.SQLFeatureNotSupportedException();
    }
    public int executeUpdate(java.lang.String p1, int p2) throws java.sql.SQLException {
        throw new java.sql.SQLFeatureNotSupportedException();
    }
    public int getResultSetHoldability() throws java.sql.SQLException {
        throw new java.sql.SQLFeatureNotSupportedException();
    }
    public boolean execute(java.lang.String p1, int p2) throws java.sql.SQLException {
        throw new java.sql.SQLFeatureNotSupportedException();
    }
    public boolean execute(java.lang.String p1, int[] p2) throws java.sql.SQLException {
        throw new java.sql.SQLFeatureNotSupportedException();
    }
    public boolean execute(java.lang.String p1, java.lang.String[] p2) throws java.sql.SQLException {
        throw new java.sql.SQLFeatureNotSupportedException();
    }
    public java.sql.ResultSet getGeneratedKeys() throws java.sql.SQLException {
        throw new java.sql.SQLFeatureNotSupportedException();
    }
    public boolean getMoreResults(int current) throws java.sql.SQLException {
        throw new java.sql.SQLFeatureNotSupportedException();
    }
    public boolean isPoolable() throws java.sql.SQLException {
        throw new java.sql.SQLFeatureNotSupportedException();
    }
    public void setPoolable(boolean poolable) throws java.sql.SQLException {
        throw new java.sql.SQLFeatureNotSupportedException();
    }
    public boolean isClosed() throws java.sql.SQLException {
        throw new java.sql.SQLFeatureNotSupportedException();
    }
    public <T> T unwrap(Class<T> iface) throws java.sql.SQLException {
        throw new java.sql.SQLFeatureNotSupportedException();
    }
    public boolean isWrapperFor(Class<?> iface) throws java.sql.SQLException {
        throw new java.sql.SQLFeatureNotSupportedException();
    }


    public void closeOnCompletion() throws SQLException {
        throw new java.sql.SQLFeatureNotSupportedException();

    }


    public boolean isCloseOnCompletion() throws SQLException {
        throw new java.sql.SQLFeatureNotSupportedException();
    }

}
