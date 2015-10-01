package jpl.mipl.io.vicar;

import java.io.*;

/**
 * The format (host data representation) of a <code>VicarBinaryLabel</code>
 * does not match the BINTFMT/BREALFMT of the file it is being written to.
 */

public class BinaryFormatMismatchException extends IOException
{

/***********************************************************************
 * Detailed message
 * @param s detailed message
 */
    public BinaryFormatMismatchException(String s)
    {
	super(s);
    }

}

