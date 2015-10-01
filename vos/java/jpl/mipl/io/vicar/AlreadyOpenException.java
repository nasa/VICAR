package jpl.mipl.io.vicar;

import java.io.*;

/**
 * Attempt was made to set a property that has already been set, and cannot
 * be set more than once.  For example, setting the <code>SystemLabel</code>
 * on a <code>VicarOutput</code> when it already has a system label would
 * throw this exception.
 */

public class AlreadyOpenException extends IOException
{

/***********************************************************************
 * Detailed message
 * @param s detailed message
 */
    public AlreadyOpenException(String s)
    {
	super(s);
    }

}

