package jpl.mipl.io.vicar;

import java.io.*;
import com.sun.media.jai.codec.*;

/**
 * This class is a convenient place to collect static functions related to
 * the VICAR I/O system.
 */

public class VicarIO
{

    /** True if the codec classes are available at runtime */
    protected static boolean _isCodecAvailable;
    static {		// See if Codec package is available
	try {
	    Class.forName("com.sun.media.jai.codec.SeekableStream");
	    _isCodecAvailable = true;
	}
	catch (Exception e) {
	    _isCodecAvailable = false;
	}
    }

    /** True if the JAI classes are available at runtime */
    protected static boolean _isJAIAvailable;
    static {		// See if JAI package is available
	try {
	    // Don't use JAI class... way too expensive to load and req's codec
	    Class.forName("javax.media.jai.ImageJAI");
	    _isJAIAvailable = true;
	}
	catch (Exception e) {
	    _isJAIAvailable = false;
	}
    }


/***********************************************************************
 * Reports whether the Codec classes are available at runtime
 */
    public static boolean isCodecAvailable()
    {
	return _isCodecAvailable;
    }

/***********************************************************************
 * Reports whether the JAI classes are available at runtime
 */
    public static boolean isJAIAvailable()
    {
	return _isJAIAvailable;
    }
}

