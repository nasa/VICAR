package jpl.mipl.io.vicar;

import java.io.*;

/**
 * An attempt was made to access a sequential-only file in a non-sequential
 * manner.
 */

public class NonSequentialAccessException extends IOException
{

/***********************************************************************
 * Detailed message
 * @param s detailed message
 */
    public NonSequentialAccessException(String s)
    {
	super(s);
    }

}

