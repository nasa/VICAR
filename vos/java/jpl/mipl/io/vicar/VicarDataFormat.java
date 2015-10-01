package jpl.mipl.io.vicar;

import java.io.*;
import jpl.mipl.io.streams.*;

/***********************************************************************
 * This class handles all aspects of data in different host representation.
 * It "knows" the data formats for each host type, and has factory methods
 * for creating filter streams that translate pixels between Java internal
 * representations, and the format required for a particular host.
 * <p>
 * The supported integer data representations are: <ul>
 * <li> HIGH - high byte first.  Used by Java, Sun, SGI, etc.
 * <li> LOW - low byte first.  Used by Alpha and Intel architectures.
 * </ul><p>
 * The supported floating-point data representations are: <ul>
 * <li> IEEE - IEEE floating-point format, high byte first. Java, Sun, SGI, etc
 * <li> RIEEE - Reverse IEEE (low byte first).  Intel and Alpha Unix.
 * <li> VAX - VMS D/F floating point formats.
 * </ul><p>
 * Both integer and floating-point types may alternatively be specified using
 * the strings NATIVE or LOCAL, which are equivalent.  Both mean to use the
 * "native" host representation for the machine on which you are running, which
 * for Java is always HIGH/IEEE.  If you Set using one of these, you'll Get
 * either HIGH or IEEE, never NATIVE or LOCAL.
 * <p>
 * The supported data types are: <ul>
 * <li> BYTE - 8-bit unsigned data (Java byte, interpreted as unsigned)
 * <li> HALF - 16-bit signed data (Java short)
 * <li> FULL - 32-bit signed data (Java int)
 * <li> REAL - 32-bit floating point data (Java float)
 * <li> DOUB - 64-bit floating point data (Java double)
 * <li> COMP - Complex data (two consecutive REAL's)
 * <li> USHORT - 16-bit unsigned data (Java short, interpreted as unsigned) - not legal for VICAR but other formats use it.
 * </ul><p>
 * Valid host types are: <ul>
 * <li> JAVA
 * <li> SUN-SOLR
 * <li> SGI
 * <li> X86-LINUX
 * <li> HP-700
 * <li> VAX-VMS
 * <li> AXP-VMS
 * <li> AXP-UNIX
 * <li> AXP-LINUX
 * <li> SUN-3
 * <li> SUN-4
 * <li> X86-SOLR
 * <li> ALLIANT
 * <li> DECSTATN
 * <li> MAC-AUX
 * <li> MAC-MPW
 * <li> TEK
 * <li> NATIVE
 * <li> LOCAL
 * </ul> <p>
 */

public class VicarDataFormat
{
    protected String _hostTypeString;

    protected int    _intFmtCode;
    protected String _intFmtString;

    protected int    _realFmtCode;
    protected String _realFmtString;
  

    /** indicates that for integers, the high byte is first */
    public static final int INT_FMT_HIGH            = 1;

    /** indicates that for integers, the low byte is first */
    public static final int INT_FMT_LOW             = 2;

    /** indicates that the IEEE format is used to represent reals */
    public static final int REAL_FMT_IEEE           = 3;

    /** indicates that the reverse IEEE format is used to represent reals */
    public static final int REAL_FMT_RIEEE          = 4;

    /** indicates that the VMS D/F format is used to represent reals */
    public static final int REAL_FMT_VAX            = 5;
    
    // A table to map a host type to the int and real representations it uses.

    // NATIVE and LOCAL use the same representations as Java; they are included
    // to match the C-lanugage RTL code

    protected static final String _hostTable[][] = {
      {"JAVA",     "HIGH",        "IEEE"         },
      {"SUN-SOLR", "HIGH",        "IEEE"         },
      {"SGI",      "HIGH",        "IEEE"         },
      {"X86-LINUX","LOW",         "RIEEE"        },
      {"HP-700",   "HIGH",        "IEEE"         },
      {"VAX-VMS",  "LOW",         "VAX"          },
      {"AXP-VMS",  "LOW",         "VAX"          },
      {"AXP-UNIX", "LOW",         "RIEEE"        },
      {"AXP-LINUX","LOW",         "RIEEE"        },
      {"SUN-3",    "HIGH",        "IEEE"         },
      {"SUN-4",    "HIGH",        "IEEE"         },
      {"X86-SOLR", "LOW",         "RIEEE"        },
      {"ALLIANT",  "HIGH",        "IEEE"         },
      {"DECSTATN", "LOW",         "RIEEE"        },
      //      {"CRAY",     "???",         "???"          },
      {"MAC-AUX",  "HIGH",        "IEEE"         },
      {"MAC-MPW",  "HIGH",        "IEEE"         },
      {"TEK",      "HIGH",        "IEEE"         },
      {"NATIVE",   "HIGH",        "IEEE"         },
      {"LOCAL",    "HIGH",        "IEEE"         },   
    };


/***********************************************************************
 * Initialize using a hostname only.  intFmt and realFmt are looked up.
 */
    public VicarDataFormat(String hostType)
    {
      // set the property fields
      
      setHostType(hostType);

      setIntFormat(getIntFormatFromHost(hostType));
      setRealFormat(getRealFormatFromHost(hostType));
    }

/***********************************************************************
 * Initialize using all three strings.  Consistency of the strings is
 * not checked, only validity of the intFmt/realFmt.  This should be called
 * by objects reading a label (since consistency in the label is not
 * guaranteed).  Also, host name can be anything.
 */
    public VicarDataFormat(String hostType, String intFmt, String realFmt)
    {
      // set the property fields
      
      setHostType(hostType);
      setIntFormat(intFmt);
      setRealFormat(realFmt);
    }

/***********************************************************************
 * Returns the integer format corresponding to the given hostType or null
 * if the hostType is not found.
 */
  public String getIntFormatFromHost(String s)
  {
    String hostType = s.toUpperCase();
    
    for (int i=0 ; i < _hostTable.length; i++) {
      if (hostType.equals(_hostTable[i][0]))
        return _hostTable[i][1];
    }

    return null;  // not found -- return null and let the caller handle it
  }

/***********************************************************************
 * Returns The real format corresponding the given hostType or null if
 * the hostType is not found.
 */
  public String getRealFormatFromHost(String s)
  {
    String hostType = s.toUpperCase();
    
    for (int i=0 ; i < _hostTable.length; i++) {
      if (hostType.equals(_hostTable[i][0]))
        return _hostTable[i][2];
    }
    
    return null;  // not found -- return null and let the caller handle it
  }


/***********************************************************************
 * get the host type property
 */
    public String getHostType()
    {
      return _hostTypeString;
    }

/***********************************************************************
 * set the host type property
 */
    public void setHostType(String hostType)
    {
      _hostTypeString = hostType.toUpperCase();
    }

/***********************************************************************
 * get the int format property
 */  
    public String getIntFormatString()
    {
      return _intFmtString;
    }
    
/***********************************************************************
 * get the int format property as an integer code corresponding to the
 * public static final constants in this class.
 */  
    public int getIntFormatCode()
    {
        return _intFmtCode;
    }

/***********************************************************************
 * set the int format property
 */  
    public void setIntFormat(String intFmt)
    {
      _intFmtCode = INT_FMT_HIGH;     // default to java's int representation
      _intFmtString = "HIGH";

      if (intFmt == null)
        return;

      String fmt = intFmt.toUpperCase();

      if (fmt.equals("LOW")) {
        _intFmtCode = INT_FMT_LOW;
        _intFmtString = "LOW";
      }
    }
  
/***********************************************************************
 * get the real format property
 */
    public String getRealFormatString()
    {
      return _realFmtString;
    }
  
/***********************************************************************
 * get the real format property as an integer code corresponding to the
 * public static final constants in this class.
 */  
    public int getRealFormatCode()
    {
        return _realFmtCode;
    }
  
/***********************************************************************
 * set the real format property
 */
    public void setRealFormat(String realFmt)
    {
      _realFmtCode = REAL_FMT_IEEE;   // default to java's real representation
      _realFmtString = "IEEE";
      
      if (realFmt == null)
        return;

      String fmt = realFmt.toUpperCase();

      if (fmt.equals("RIEEE")) {
        _realFmtCode = REAL_FMT_RIEEE;
        _realFmtString = "RIEEE";
      }
      if (fmt.equals("VAX")) {
        _realFmtCode = REAL_FMT_VAX;
        _realFmtString = "VAX";
      }
    }

/***********************************************************************
 * Returns the size (in bytes) of a pixel, given the integer and real
 * representations.
 *<p>
 * Currently, the number of bytes per pixel is the same regardless of the
 * int/real representations -- however, this may change (e.g., "HALF" may
 * not always imply two bytes per pixel), so we take intFmt and realFmt as
 * args in case they become necessary in the future.
 */
   public static int getPixelSize(String dataType,String intFmt,String realFmt)
   {

     if (dataType.equals("BYTE"))
       return 1;
     if (dataType.equals("HALF"))
       return 2;
     if (dataType.equals("FULL"))
       return 4;
     if (dataType.equals("REAL"))
       return 4;
     if (dataType.equals("DOUB"))
       return 8;
     if (dataType.equals("COMP"))
       return 8;
     if (dataType.equals("USHORT"))
       return 2;

     return 1;
   }

/***********************************************************************
 * Returns the size in pixels of the given data type for the host.  Unlike
 * Java, the size cannot be assumed, although they match in all current cases.
 */
    public int getPixelSize(String dataType)
    {
      // get the formats that are used to call the other getPixelSize()
      return getPixelSize(dataType, getIntFormatString(), getRealFormatString());
    }

/***********************************************************************
 * Indicate whether or not translation is needed for a given data type.
 * If not, the caller may save time by avoiding the translation step
 * altogether.
 */
    public boolean isTranslationNeeded(String dataType)
    {
      // you never need to convert if the int and real formats being used
      // match those used by Java:

      if ((_intFmtCode == INT_FMT_HIGH) && (_realFmtCode == REAL_FMT_IEEE))
        return false;

      // we "fall through" *unless* we find a type that needs conversion
      
      if ( (_intFmtCode != INT_FMT_HIGH) &&
           (dataType.equals("HALF") ||
            dataType.equals("FULL")) )
        return true;
      else if ( (_intFmtCode != REAL_FMT_IEEE) &&
                (dataType.equals("REAL") ||
                 dataType.equals("DOUB") ||
                 dataType.equals("COMP")) )
        return true;
      
      return false;
    }

/***********************************************************************
 * Return a string representation of the VICAR data format object.
 */
    public String toString()
    {
      StringBuffer buf = new StringBuffer(80);

      buf.append("hostType = ");
      buf.append(_hostTypeString);
      
      buf.append(" intFmt = ");
      buf.append(_intFmtString);
      
      buf.append(" realFmt = ");
      buf.append(_realFmtString);

      return buf.toString();
    }

/***********************************************************************
 * These "factory" methods will create and return a stream object that
 * implements the <code>DataInput</code> or <code>DataOutput</code> interfaces.
 * These streams may be private classes, but they are set up to do the
 * correct data type translations given the current setup of this
 * <code>VicarDataFormat</code>.
 * <p>
 * It is quite possible that the factories may simply return the original
 * stream, if it already implements <code>DataInput</code> or
 * <code>DataOutput</code> and the data is already in Java format.
 * <p>
 * There is no guarantee that the returned stream will support any given
 * random-access or seek operations.  For this reason, the original stream
 * should be retained by the caller, and used to reposition within the
 * file (if needed).  The returned stream (if not simply the original) will
 * only use the read() or write() functions of the given stream, so it will
 * be able to make use of any repositionings done on the original stream.
 * @throws IllegalArgumentException if the provided object is not a subclass
 * of <code>InputStream</code> or <code>DataInput</code>.
 */
    public DataInput getDataInputWrapper(Object s)
    { 
      if ((_intFmtCode == INT_FMT_HIGH) &&
	  (_realFmtCode == REAL_FMT_IEEE)) {
	
	// Special case:  for RAFIS, we're wrapping a RandomAccessFile, which
	// already implements DataInput.  So just return it, rather than
	// going through 2 extra layers of wrappers!

	if (s instanceof DataInput)
	  return (DataInput) s;
	else if (s instanceof RandomAccessFileInputStream)
	  return (DataInput) ((RandomAccessFileInputStream)s).getFile();
	else if (s instanceof InputStream)
	  return new DataInputStream((InputStream)s);
	else
	  throw new IllegalArgumentException("need InputStream or DataInput");
      }

      else if ((_intFmtCode == INT_FMT_LOW) &&
	       (_realFmtCode == REAL_FMT_RIEEE)) {
	if (s instanceof InputStream)
	    return new VicarSwapDataInputStream((InputStream)s,
						_intFmtCode, _realFmtCode);
	else
	    return new VicarSwapDataInputDIWrapper((DataInput)s,
						_intFmtCode, _realFmtCode);
      }
      
      else if ((_intFmtCode == INT_FMT_LOW) && 
	       (_realFmtCode == REAL_FMT_VAX)) {
	if (s instanceof InputStream)
	    return new VicarVaxDataInputStream((InputStream)s,
						_intFmtCode, _realFmtCode);
	else
	    return new VicarVaxDataInputDIWrapper((DataInput)s,
						_intFmtCode, _realFmtCode);
      }
      else {
	if (s instanceof InputStream)
	    return new VicarDataInputStream((InputStream)s,
						_intFmtCode, _realFmtCode);
	else
	    return new VicarDataInputDIWrapper((DataInput)s,
						_intFmtCode, _realFmtCode);
      }
    }

/***********************************************************************
 * Factory to return a DataOutput object.
 * @see #getDataInputWrapper
 * @throws IllegalArgumentException if the provided object is not a subclass
 * of <code>InputStream</code> or <code>DataInput</code>.
 */
    public DataOutput getDataOutputWrapper(Object s)
    {
      if ((_intFmtCode == INT_FMT_HIGH) &&
	  (_realFmtCode == REAL_FMT_IEEE)) {

	// Special case:  for RAFOS, we're wrapping a RandomAccessFile, which
	// already implements DataOutput.  So just return it, rather than
	// going through 2 extra layers of wrappers!

	if (s instanceof DataOutput)
	  return (DataOutput) s;
	else if (s instanceof RandomAccessFileOutputStream)
	  return (DataOutput) ((RandomAccessFileOutputStream)s).getFile();
	else if (s instanceof OutputStream)
	  return new DataOutputStream((OutputStream)s);
	else
	  throw new IllegalArgumentException("need OutputStream or DataOutput");
      }

      else if ((_intFmtCode == INT_FMT_LOW) &&
	       (_realFmtCode == REAL_FMT_RIEEE)) {
	if (s instanceof OutputStream)
	    return new VicarSwapDataOutputStream((OutputStream)s,
						_intFmtCode, _realFmtCode);
	else
	    return new VicarSwapDataOutputDOWrapper((DataOutput)s,
						_intFmtCode, _realFmtCode);
      }
      else if ((_intFmtCode == INT_FMT_LOW) &&
	       (_realFmtCode == REAL_FMT_VAX)) {
	if (s instanceof OutputStream)
	    return new VicarVaxDataOutputStream((OutputStream)s,
						_intFmtCode, _realFmtCode);
	else
	    return new VicarVaxDataOutputDOWrapper((DataOutput)s,
						_intFmtCode, _realFmtCode);
      }
      else {
	if (s instanceof OutputStream)
	    return new VicarDataOutputStream((OutputStream)s,
						_intFmtCode, _realFmtCode);
	else
	    return new VicarDataOutputDOWrapper((DataOutput)s,
						_intFmtCode, _realFmtCode);
      }
    }

/***********************************************************************
 * "equals" is defined as having the same intFmtCode and realFmtCode.
 * The host type is not considered when checking for equality.  "equals"
 * thus means the data types are interchangeable with each other.
 */
    public boolean equals(Object obj)
    {
	if (! (obj instanceof VicarDataFormat))
	    return false;
	VicarDataFormat fmt = (VicarDataFormat)obj;

	if (_intFmtCode != fmt._intFmtCode || _realFmtCode != fmt._realFmtCode)
	    return false;

	return true;
    }

}

