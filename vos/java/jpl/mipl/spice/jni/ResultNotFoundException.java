/*
 * ResultNotFoundException.java
 *
 * Created on August 18, 2000, 9:45 AM
 */

package jpl.mipl.spice.jni;

/**
 *
 * @author  mpb
 * @version 
 */
public class ResultNotFoundException extends Exception {

    /**
     * Creates new <code>ResultNotFoundException</code> without detail message.
     */
    public ResultNotFoundException() {
    }


    /**
     * Constructs an <code>ResultNotFoundException</code> with the specified detail message.
     * @param msg the detail message.
     */
    public ResultNotFoundException(String msg) {
        super(msg);
    }
}

