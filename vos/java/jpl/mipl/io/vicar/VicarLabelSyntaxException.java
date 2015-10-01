package jpl.mipl.io.vicar;

import java.io.*;

/**
 * An error occurred while parsing the VICAR label.
 */

public class VicarLabelSyntaxException extends IOException
{

/***********************************************************************
 * Detailed message
 * @param s detailed message
 */
    public VicarLabelSyntaxException(String s)
    {
	super(s);
    }

}

