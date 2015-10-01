package jpl.mipl.io.vicar;

import java.io.*;

/**
 * The size of a <code>VicarBinaryLabel</code> does not match the size of
 * the header or prefix of the file it is being written to.
 */

public class BinarySizeMismatchException extends IOException
{

/***********************************************************************
 * Detailed message
 * @param s detailed message
 */
    public BinarySizeMismatchException(String s)
    {
	super(s);
    }

}

