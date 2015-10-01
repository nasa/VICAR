/*
 * BadDatabaseDescriptionException.java
 *
 * Created on August 23, 2000, 9:30 AM
 */

package jpl.mipl.spice.jni;

/**
 *
 * @author  mpb
 * @version 
 */
public class BadDatabaseDescriptionException extends Exception {

    /**
     * Creates new <code>BadDatabaseDescriptionException</code> without detail message.
     */
    public BadDatabaseDescriptionException() {
    }


    /**
     * Constructs an <code>BadDatabaseDescriptionException</code> with the specified detail message.
     * @param msg the detail message.
     */
    public BadDatabaseDescriptionException(String msg) {
        super(msg);
    }
}

