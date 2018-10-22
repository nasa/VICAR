/*
 * QueryException.java
 *
 * Created on August 18, 2000, 10:01 AM
 */

package jpl.mipl.spice.jni;

/**
 *
 * @author  mpb
 * @version 
 */
public class QueryException extends Exception {

    /**
     * Creates new <code>QueryException</code> without detail message.
     */
    public QueryException() {
    }


    /**
     * Constructs an <code>QueryException</code> with the specified detail message.
     * @param msg the detail message.
     */
    public QueryException(String msg) {
        super(msg);
    }
}

