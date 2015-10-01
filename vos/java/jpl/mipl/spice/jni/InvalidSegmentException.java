/*
 * InvalidSegment.java
 *
 * Created on August 15, 2000, 9:10 AM
 */

package jpl.mipl.spice.jni;

/** An exception throws when trying to add a bad segment to an E kernel.
 * @author Michael Brady
 */
public class InvalidSegmentException extends Exception {

    /**
     * Creates new <code>InvalidSegment</code> without detail message.
     */
    public InvalidSegmentException() {
    }


    /**
     * Constructs an <code>InvalidSegment</code> with the specified detail message.
     * @param msg the detail message.
     */
    public InvalidSegmentException(String msg) {
        super(msg);
    }
}

