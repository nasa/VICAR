package jpl.mipl.io.vicar;

import java.util.*;
import java.beans.*;
import java.lang.reflect.*;

/**
 * This class is a bean that maintains all VICAR system label information.
 * Note that no value checking is performed, since there are no easy facilities
 * for validation in the beans system, without going through the VetoableChange
 * mechanism.  Besides, we want to accept whatever is in a label, and do the
 * best we can with it.
 * <p>
 * N1, N2, N3, and NS, NL, NB are kept up-to-date with each other via the
 * current Org setting.  Changes to one set will change the corresponding item
 * in the other set.  Note that Org (Organization) must be set first, because
 * the mapping between N1/2/3 and NS/L/B depends on the organziation and is
 * not reversible once set.
 */

public class SystemLabel implements Cloneable
{
    // The fields
    // Note: LBLSIZE is not maintained here; it is managed by the label system.

    protected String _format;
    protected String _type;
    protected int _bufsiz;
    protected int _dim;
    protected int _eol;
    protected int _recsize;
    protected String _org;
    protected int _nl;
    protected int _ns;
    protected int _nb;
    protected int _n1;
    protected int _n2;
    protected int _n3;
    protected int _n4;
    protected int _nbb;
    protected int _nlb;
    protected String _host;
    protected String _intfmt;
    protected String _realfmt;
    protected String _bhost;
    protected String _bintfmt;
    protected String _brealfmt;
    protected String _bltype;

    /** This is a completely synthetic property, derived from format,
    * intfmt, and realfmt labels.
    */
    protected int _pixel_size;

    /** These valid fields are needed because input and output files have
    * different defaults.  Inputs default to VAX/VMS since that's all that
    * existed before these labels came along.  Outputs default to the
    * native format, Java in this case.
    */
    protected boolean _host_valid;
    protected boolean _intfmt_valid;
    protected boolean _realfmt_valid;

    /** This vector holds label items which weren't recognized */
    protected List _extraItems;

    /** Codes for Org, used for efficiency when we need to compare a lot */

    protected int _org_code;
    public static final int ORG_BSQ = 0;
    public static final int ORG_BIL = 1;
    public static final int ORG_BIP = 2;

    /** Codes for Data Type, used for efficiency when we need to compare a lot*/

    protected int _format_code;
    public static final int TYPE_BYTE = 0;
    public static final int TYPE_HALF = 1;
    public static final int TYPE_FULL = 2;
    public static final int TYPE_REAL = 3;
    public static final int TYPE_DOUB = 4;
    public static final int TYPE_COMP = 5;
    public static final int TYPE_USHORT = 6;	// not a real VICAR type

    /** Table of system label entries.  Note that non-required items
     *  don't need a Valid flag since we have defaults.  Also, BUFSIZ
     *  is not required, although it is in the C RTL.  */
    public static final LabelEntry _systemList[] = {
	new LabelEntry("FORMAT",  LabelEntry.TYPE_STRING,"format",  null,true),
	new LabelEntry("TYPE",    LabelEntry.TYPE_STRING,"type",    null,false),
	new LabelEntry("BUFSIZ",  LabelEntry.TYPE_INT,   "bufsiz",  null,false),
	new LabelEntry("DIM",     LabelEntry.TYPE_INT,   "dim",     null,false),
	new LabelEntry("EOL",     LabelEntry.TYPE_INT,   "EOL",     null,false),
	new LabelEntry("RECSIZE", LabelEntry.TYPE_INT,   "recsize", null,true),
	new LabelEntry("ORG",     LabelEntry.TYPE_STRING,"org",     null,false),
	new LabelEntry("NL",      LabelEntry.TYPE_INT,   "NL",      null,true),
	new LabelEntry("NS",      LabelEntry.TYPE_INT,   "NS",      null,true),
	new LabelEntry("NB",      LabelEntry.TYPE_INT,   "NB",      null,true),
	new LabelEntry("N1",      LabelEntry.TYPE_INT,   "n1",      null,false),
	new LabelEntry("N2",      LabelEntry.TYPE_INT,   "n2",      null,false),
	new LabelEntry("N3",      LabelEntry.TYPE_INT,   "n3",      null,false),
	new LabelEntry("N4",      LabelEntry.TYPE_INT,   "n4",      null,false),
	new LabelEntry("NBB",     LabelEntry.TYPE_INT,   "NBB",     null,false),
	new LabelEntry("NLB",     LabelEntry.TYPE_INT,   "NLB",     null,false),
	new LabelEntry("HOST",    LabelEntry.TYPE_STRING,"host",
							     "hostValid",false),
	new LabelEntry("INTFMT",  LabelEntry.TYPE_STRING,"intFmt",
							   "intFmtValid",false),
	new LabelEntry("REALFMT", LabelEntry.TYPE_STRING,"realFmt",
							  "realFmtValid",false),
	new LabelEntry("BHOST",   LabelEntry.TYPE_STRING,"BHost",   null,false),
	new LabelEntry("BINTFMT", LabelEntry.TYPE_STRING,"BIntFmt", null,false),
	new LabelEntry("BREALFMT",LabelEntry.TYPE_STRING,"BRealFmt",null,false),
	new LabelEntry("BLTYPE",  LabelEntry.TYPE_STRING,"BLType",  null,false)
    };

/***********************************************************************
 * Create an empty (default) SystemLabel.
 */
    public SystemLabel()
    {
	setDefaults();
	_extraItems = new ArrayList();
    }

/***********************************************************************
 * Create a SystemLabel and fill it up.
 */
    public SystemLabel(VicarLabelSet set)
	throws IntrospectionException, InvocationTargetException,
	       IllegalAccessException
    {
	this();
	readLabel(set);
    }

/***********************************************************************
 * Fill up the SystemLabel object from the VicarLabelSet.
 */
    public void readLabel(VicarLabelSet set)
	throws IntrospectionException, InvocationTargetException,
	       IllegalAccessException
    {
	LabelToBean.readLabel(set, this, _systemList, _extraItems);
    }

/***********************************************************************
 * Write the SystemLabel object to the VicarLabelSet.
 */
    public void writeLabel(VicarLabelSet set)
	throws IntrospectionException, InvocationTargetException,
	       IllegalAccessException
    {
//!!!!	LabelToBean.writeLabel(set, this, _systemList, _extraItems);
	LabelToBean.writeLabel(set, this, _systemList, null);
    }

/***********************************************************************
 * Initialize the label to the default values.
 */
    public void setDefaults()
    {
	_format = "BYTE";
	_format_code = TYPE_BYTE;
	_type = "IMAGE";
	_bufsiz = 0;
	_dim = 3;
	_eol = 0;
	_recsize = 0;
	_org = "BSQ";
	_org_code = ORG_BSQ;
	_nl = 0;
	_ns = 0;
	_nb = 0;
	_n1 = 0;
	_n2 = 0;
	_n3 = 0;
	_n4 = 0;
	_nbb = 0;
	_nlb = 0;
	_host = "JAVA";		// for output.  Input defaults to vax-vms.
	_intfmt = "HIGH";	// ditto
	_realfmt = "IEEE";	// ditto
	_bhost = "VAX-VMS";	// yes, really.  Historical reasons.
	_bintfmt = "LOW";
	_brealfmt = "VAX";
	_bltype = "";

	_host_valid = false;
	_intfmt_valid = false;
	_realfmt_valid = false;

	calcPixelSize();
	calcRecsize();
    }

/***********************************************************************
 * Retrieve the values which didn't match anything
 */
    public List getExtraItems()
    {
	return _extraItems;
    }

/***********************************************************************
 * Return a string representation of the system label.  To make this easy,
 * we convert back to a <code>VicarLabelSet</code> and use its toString().
 */
    public String toString()
    {
	VicarLabelSet set = new VicarLabelSet(VicarLabelSet.LABEL_SYSTEM);
	try {
	    writeLabel(set);
	} catch (Exception e) {
	    return e.toString();
	}
	return set.toString();
    }

/***********************************************************************
 * Property <code>Format</code>: data type of pixels in the image.  Valid
 * values are:<ul>
 * <li>BYTE: one byte unsigned integer, range 0 to 255.
 * <li>HALF: two byte signed integer, range -32768 to 32767.
 * <li>FULL: four byte signed integer.
 * <li>REAL: single-precision floating point number.
 * <li>DOUB: double-precision floating point number.
 * <li>COMP: complex number, composed of two REALs in the order (real,imaginary)
 * </ul>
 * The following value is accepted, although it is not a valid VICAR data type.
 * However, other file formats which use this package can find it useful.
 * <ul>
 * <li>USHORT: two byte unsigned integer, range 0 to 65535.
 * </ul>
 * The following values are obsolete, but may appear in some older images:<ul>
 * <li>WORD: same as HALF
 * <li>LONG: same as FULL
 * <li>COMPLEX: same as COMP
 * </ul>
 * The Set method will convert the three obsolete types to their counterparts.
 */
    public String getFormat() {		return _format; }
    public int getFormatCode() {	return _format_code; }
    public void setFormat(String s) {
	_format = s.toUpperCase();
	if (_format.equals("WORD")) _format = "HALF";
	if (_format.equals("LONG")) _format = "FULL";
	if (_format.equals("COMPLEX")) _format = "COMP";

	_format_code = TYPE_BYTE;
	if (_format.equals("HALF")) _format_code = TYPE_HALF;
	if (_format.equals("FULL")) _format_code = TYPE_FULL;
	if (_format.equals("REAL")) _format_code = TYPE_REAL;
	if (_format.equals("DOUB")) _format_code = TYPE_DOUB;
	if (_format.equals("COMP")) _format_code = TYPE_COMP;
	if (_format.equals("USHORT")) _format_code = TYPE_USHORT;

	calcPixelSize();
    }
    public void setFormatCode(int c) {
	switch (c) {
	    case TYPE_HALF:	_format = "HALF"; break;
	    case TYPE_FULL:	_format = "FULL"; break;
	    case TYPE_REAL:	_format = "REAL"; break;
	    case TYPE_DOUB:	_format = "DOUB"; break;
	    case TYPE_COMP:	_format = "COMP"; break;
	    case TYPE_USHORT:	_format = "USHORT"; break;
	    default:		_format = "BYTE"; c=TYPE_BYTE; break;
	}
	_format_code = c;
	calcPixelSize();
    }

/***********************************************************************
 * Property <code>Type</code>: The kind of file this is. TYPE defaults to
 * IMAGE. The valid values may very well be expanded in the future, but
 * currently they are:<ul>
 * <li>IMAGE: standard VICAR image file.
 * <li>PARMS: very old-style parameter file. 
 * <li>PARM: old-style parameter file.
 * <li>PARAM: current parameter file, used to hold input parameters for one
 *     VICAR program that were generated by another.  Not useful in Java.
 * <li>GRAPH1: IBIS Graphics-1 file.
 * <li>GRAPH2: IBIS Graphics-2 file.
 * <li>GRAPH3: IBIS Graphics-3 file.
 * <li>TABULAR: IBIS Tabular file.
 * </ul>
 * This field is basically ignored.
 */
    public String getType() {		return _type; }
    public void setType(String s) {	_type = s.toUpperCase(); }

/***********************************************************************
 * Property <code>Bufsiz</code>: This label item is obsolete. It formerly
 * defined the size of the internal buffer to use when reading the image,
 * but it is no longer used. It still must be present for historical reasons,
 * however.  BUFSIZ is automatically set the same as RECSIZE, whenever
 * RECSIZE changes.  If you want a certain value for this field for some
 * reason, set it last.  Unlike the C RTL, the Java version does not require
 * this field... but it must always be present for VICAR compatibility.
 * <p>
 * Note the spelling: no "e".  This should be a reminder that this field is
 * not used anywhere.
 */
    public int getBufsiz() {		return _bufsiz; }
    public void setBufsiz(int i) {	_bufsiz = i; }

/***********************************************************************
 * Property <code>Dim</code>: The number of dimensions in the file, which
 * is always equal to 3.  Some older images may have a DIM of 2, in which
 * case some labels will not be present.  Note that the dimension is 3 even
 * if N3=1 (e.g. there is only one band in a BSQ file).
 */
    public int getDim() {		return _dim; }
    public void setDim(int i)	 {	_dim = i; }

/***********************************************************************
 * Property <code>EOL</code>: A flag indicating the existence of EOL labels.
 * If EOL=1, the labels are present.  If EOL=0 or is absent, no EOL labels
 * are present, and the entire label string is at the front of the file.
 * <p>
 * This flag is automatically maintained by the <code>VicarLabel*</code>
 * classes.
 */
    public int getEOL() {		return _eol; }
    public void setEOL(int i) {		_eol = i; }
 
/***********************************************************************
 * Property <code>Recsize</code>: The size in bytes of each record in the
 * VICAR file.  It may be calculated with the formula NBB + N1 * pixel_size,
 * where pixel_size is the size of each pixel computed using FORMAT (for the
 * pixel type) and the INTFMT or REALFMT (for the host representation) labels.
 */
    public int getRecsize() {		return _recsize; }
    public void setRecsize(int i) {	_recsize= i;  _bufsiz = _recsize; }
 /** This routine recalculates the recsize based on the other values */
    public void calcRecsize() {
	_recsize = getNBB() + getN1() * getPixelSize();
	_bufsiz = _recsize;
    }

/***********************************************************************
 * Property <code>Org</code>: The organization of the file.  While N1 is
 * always the fastest-varying dimension, and N3 is the slowest, the terms
 * Samples, Lines, and Bands may be interpreted in different ways.  ORG
 * specifies which interpretation to use, and defaults to BSQ.  The valid
 * values are:<ul>
 * <li>BSQ: Band SeQuential.  The file is a sequence of bands.  Each band
 *     is made up of lines, which are in turn made up of samples.
 *     N1=Samples, N2=Lines, and N3=Bands.  This is the most common case.
 * <li>BIL: Band Interleaved by Line.  The file is a sequence of lines.
 *     Each line is made up of bands, which are in turn made up of samples.
 *     N1=Samples, N2=Bands, N3=Lines.
 * <li>BIP: Band Interleaved by Pixel.  The file is a sequence of lines.
 *     Each line is made up of samples, which are in turn made up of bands.
 *     N1=Bands, N2=Samples, N3=Lines.
 * </ul>
 * Org must be set before setting file-size parameters such as N1/N2/N3 or
 * NS/NL/NB.
 */
    public String getOrg() {		return _org; }
    public int getOrgCode() {		return _org_code; }
    public void setOrg(String s) {
	_org = s.toUpperCase();
	_org_code = ORG_BSQ;		// compute code for efficiency
	if (_org.equals("BIL"))
	    _org_code = ORG_BIL;
	if (_org.equals("BIP"))
	    _org_code = ORG_BIP;
    }

/***********************************************************************
 * Property <code>NL</code>: The number of lines in the image (same as
 * N2 for BSQ or N3 for BIL and BIP).
 * <p>
 * Setting this property will affect <code>N1</code>, <code>N2</code>, or
 * <code>N3</code>, depending on the current <code>Org</code> setting.
 * <code>Org</code> must be set first.
 */
    public int getNL() {		return _nl; }
    public void setNL(int i) {
	_nl = i;
	switch (_org_code) {
	    case ORG_BSQ:	_n2 = _nl;	break;
	    case ORG_BIL:	_n3 = _nl;	break;
	    case ORG_BIP:	_n3 = _nl;	break;
	}
    }

/***********************************************************************
 * Property <code>NS</code>: The number of samples in the image (same as
 * N1 for BSQ and BIL or N2 for BIP).
 * <p>
 * Setting this property will affect <code>N1</code>, <code>N2</code>, or
 * <code>N3</code>, depending on the current <code>Org</code> setting.
 * <code>Org</code> must be set first.
 */
    public int getNS() {		return _ns; }
    public void setNS(int i) {
	_ns = i;
	switch (_org_code) {
	    case ORG_BSQ:	_n1 = _ns;	calcRecsize();	break;
	    case ORG_BIL:	_n1 = _ns;	calcRecsize();	break;
	    case ORG_BIP:	_n2 = _ns;			break;
	}
    }

/***********************************************************************
 * Property <code>NB</code>: The number of bands in the image (same as
 * N3 for BSQ, N2 for BIL, or N1 for BIP).
 * <p>
 * Setting this property will affect <code>N1</code>, <code>N2</code>, or
 * <code>N3</code>, depending on the current <code>Org</code> setting.
 * <code>Org</code> must be set first.
 */
    public int getNB() {		return _nb; }
    public void setNB(int i) {
	_nb = i;
	switch (_org_code) {
	    case ORG_BSQ:	_n3 = _nb;			break;
	    case ORG_BIL:	_n2 = _nb;			break;
	    case ORG_BIP:	_n1 = _nb;	calcRecsize();	break;
	}
    }

/***********************************************************************
 * Property <code>N1</code>: The size (in pixels) of the first (fastest-varying)
 * dimension.  Defaults to <code>NS</code> or <code>NB</code> if not present
 * or 0.
 * 
 * <p>
 * Setting this property to a non-0 value will affect <code>NS</code>,
 * <code>NL</code>, or <code>NB</code>, depending on the current
 * <code>Org</code> setting.  <code>Org</code> must be set first.
 */
    public int getN1() {
	if (_n1 != 0)
	    return _n1;
	switch (_org_code) {
	    case ORG_BSQ:	return _ns;
	    case ORG_BIL:	return _ns;
	    case ORG_BIP:	return _nb;
	}
	return 0;
    }

    public void setN1(int i) {
	_n1 = i;
	if (_n1 == 0) return;
	switch (_org_code) {
	    case ORG_BSQ:	_ns = _n1;	break;
	    case ORG_BIL:	_ns = _n1;	break;
	    case ORG_BIP:	_nb = _n1;	break;
	}
	calcRecsize();
    }

/***********************************************************************
 * Property <code>N2</code>: The size of the second dimension.  Defaults to
 * <code>NS</code>, <code>NL</code>, or <code>NB</code> if not present or 0.
 * <p>
 * Setting this property to a non-0 value will affect <code>NS</code>,
 * <code>NL</code>, or <code>NB</code>, depending on the current
 * <code>Org</code> setting.  <code>Org</code> must be set first.
 */
    public int getN2() {
	if (_n2 != 0)
	    return _n2;
	switch (_org_code) {
	    case ORG_BSQ:	return _nl;
	    case ORG_BIL:	return _nb;
	    case ORG_BIP:	return _ns;
	}
	return 0;
    }
	
    public void setN2(int i) {
	_n2 = i;
	if (_n2 == 0) return;
	switch (_org_code) {
	    case ORG_BSQ:	_nl = _n2;	break;
	    case ORG_BIL:	_nb = _n2;	break;
	    case ORG_BIP:	_ns = _n2;	break;
	}
    }

/***********************************************************************
 * Property <code>N3</code>: The size of the third (slowst-varying)
 * dimension.  Defaults to <code>NL</code> or <code>NB</code> if not
 * present or 0.
 * <p>
 * Setting this property to a non-0 value will affect <code>NS</code>,
 * <code>NL</code>, or <code>NB</code>, depending on the current
 * <code>Org</code> setting.  <code>Org</code> must be set first.
 */
    public int getN3() {
	if (_n3 != 0)
	    return _n3;
	switch (_org_code) {
	    case ORG_BSQ:	return _nb;
	    case ORG_BIL:	return _nl;
	    case ORG_BIP:	return _nl;
	}
	return 0;
    }

    public void setN3(int i) {
	_n3 = i;
	if (_n3 == 0) return;
	switch (_org_code) {
	    case ORG_BSQ:	_nb = _n3;	break;
	    case ORG_BIL:	_nl = _n3;	break;
	    case ORG_BIP:	_nl = _n3;	break;
	}
    }

/***********************************************************************
 * Property <code>N4</code>: This item was to have been used for
 * four-dimensional files, but this has not been implemented.  Defaults to 0.
 */
    public int getN4() {		return _n4; }
    public void setN4(int i) {		_n4 = i; }

/***********************************************************************
 * Property <code>NBB</code>: The number of bytes of binary prefix before
 * each record.  Each and every record consists of the pixels of the fastest-
 * varying dimension, optionally preceded by a binary prefix.  The size (in
 * <em>bytes</em>, not pixels) of this binary prefix is given by NBB, which
 * defaults to 0.  The binary prefix and the binary header (see NLB) together
 * make up the binary label.  The format of data in the binary label is
 * application-defined.  The BLTYPE label is intended to identify the format
 * of the binary label.  Generally, the binary label should be ignored unless
 * the format of the data is known beforehand.
 */
    public int getNBB() {		return _nbb; }
    public void setNBB(int i) {		_nbb = i;	calcRecsize(); }

/***********************************************************************
 * Property <code>NLB</code>: The number of lines (records) of binary header
 * at the top of the file.  The optional binary header occurs once in the
 * file, between the main labels and the image data.  It is not repeated per
 * third dimension.  The size of the binary header in bytes is given by
 * NLB * RECSIZE, since NLB is a line count.  NLB defaults to 0.  Note that
 * the binary header also includes space reserved for the binary prefix (NBB),
 * since NBB goes into RECSIZE.  The binary header and the binary prefix (see
 * NBB) together make up the binary label.
 * @see #getNBB()
 */
    public int getNLB() {		return _nlb; }
    public void setNLB(int i) {		_nlb = i; }

/***********************************************************************
 * Property <code>Host</code>: The type of computer used to generate the
 * image.  It is used only for documentation; the INTFMT and REALFMT labels
 * are used to determine the format of the pixels.  Nevertheless, it should
 * be kept consistent with INTFMT and REALFMT.  HOST defaults to VAX-VMS.
 * The value may be anything, as new computer types are occasionally added,
 * but as of this writing, the possible values are:<ul>
 * <li>ALLIANT*: Alliant FX series computer
 * <li>AXP-LINUX*: DIGITAL Alpha running Linux
 * <li>AXP-UNIX*: DIGITAL Alpha running Unix (OSF/1)
 * <li>AXP-VMS: DIGITAL Alpha running VMS
 * <li>CRAY*: Cray (port is incomplete)
 * <li>DECSTATN*: DECstation (any DEC MIPS-based RISC machine) running Ultrix
 * <li>HP-700: HP 9000 Series 700 workstation
 * <li>JAVA: Any Java platform, regardless of underlying platform type
 * <li>MAC-AUX*: Macintosh running A/UX
 * <li>MAC-MPW*: Macintosh running native mode with Mac Programmers Workbench
 * <li>SGI: Silicon Graphics workstation
 * <li>SUN-SOLR: Sun SPARC machine running Solaris 2
 * <li>SUN-3*: Sun 3, and model
 * <li>SUN-4+: Sun 4, SPARCstation, or Sun clone running SunOS
 * <li>TEK*: Tektronix workstation
 * <li>VAX-VMS+: DIGITAL VAX running VMS
 * <li>X86-LINUX: Intel x86 machine running Linux
 * <li>X86-SOLR*: Intel x86 machine running Solaris 2
 * </ul>
 * * Host machine is not officially supported<p>
 * + No longer officially supported
 * <p>
 * Note that <em>ALL</em> Java platforms use the host name "JAVA", regardless
 * of the underlying architecture.
 */
    public String getHost() {		return _host; }
    public void setHost(String s) {   _host=s.toUpperCase(); _host_valid=true; }
    public boolean isHostValid() {	return _host_valid; }
    public void setHostValid(boolean b) { _host_valid = b; }

/***********************************************************************
 * Property <code>IntFmt</code>: The format used to represent integer pixels
 * (BYTE, HALF, and FULL) in the file.  Defaults to LOW.  Note that INTFMT
 * should be present even if the pixels are a floating-point type.  The
 * valid values are:<ul>
 * <li>LOW: Low byte first, "little endian".  Used for hosts VAX-VMS, AXP-VMS,
 *     X86-SOLR, DECSTATN, AXP-UNIX, AXP-LINUX, X86-LINUX.
 * <li>HIGH: High byte first, "big endian".  Used for all other hosts,
 *     including JAVA.
 * </ul>
 */
    public String getIntFmt() {		return _intfmt; }
    public void setIntFmt(String s)
    {
	_intfmt = s.toUpperCase();
	calcPixelSize();
	_intfmt_valid = true;
    }
    public boolean isIntFmtValid() {	return _intfmt_valid; }
    public void setIntFmtValid(boolean b) { _intfmt_valid = b; }

/***********************************************************************
 * Property <code>RealFmt</code>: The format used to represent floating-point
 * pixels (REAL, DOUB, and COMP) in the file.  Defaults to VAX.  Note that
 * REALFMT should be present even if the pixels are an integral type.  The
 * valid values are:<ul>
 * <li>VAX: VAX format.  Single precision is VAX F format, double precision
 *     is VAX D format.  Used on hosts VAX-VMS and AXP-VMS only.
 * <li>RIEEE: Reverse IEEE format.  Just like IEEE, except the bytes are
 *     reversed, with the exponent last.  Used on hosts X86-SOLR, DECSTATN,
 *     AXP-UNIX, AXP-LINUX, X86-LINUX only.
 * <li>IEEE: IEEE 754 format, with the high-order bytes (containing the
 *     exponent) first.  Used for all other hosts, including JAVA.
 * </ul>
 */
    public String getRealFmt() {	return _realfmt; }
    public void setRealFmt(String s)
    {
	_realfmt = s.toUpperCase();
	calcPixelSize();
	_realfmt_valid = true;
    }
    public boolean isRealFmtValid() {	return _realfmt_valid; }
    public void setRealFmtValid(boolean b) { _realfmt_valid = b; }

/***********************************************************************
 * Property <code>BHost</code>: The type of computer used to generate the
 * binary label.  It can take the same values with the same meanings as HOST.
 * The reason BHOST is separate is that the data in the binary label may be
 * in a different host representation than the pixels.
 * @see #getHost()
 */
    public String getBHost() {		return _bhost; }
    public void setBHost(String s) {	_bhost = s.toUpperCase(); }

/***********************************************************************
 * Property <code>BIntFmt</code>: The format used to represent integers in
 * the binary label.  It can take the same values with the same meanings as
 * INTFMT.  The reason BINTFMT is separate is that the data in the binary label
 * may be in a different host representation than the pixels.
 * @see #getIntFmt()
 */
    public String getBIntFmt() {	return _bintfmt; }
    public void setBIntFmt(String s) {	_bintfmt = s.toUpperCase(); }

/***********************************************************************
 * Property <code>BRealFmt</code>: The format used to represent floating-point
 * data in the binary label.  It can take the same values with the same
 * meanings as REALFMT.  The reason BINTFMT is separate is that the data in
 * the binary label may be in a different host representation than the pixels.
 * @see #getRealFmt()
 */
    public String getBRealFmt() {	return _brealfmt; }
    public void setBRealFmt(String s) {	_brealfmt = s.toUpperCase(); }

/***********************************************************************
 * Property <code>BLType</code>: The type of the binary label.  This is not
 * a data type, but is a string identifying the kind of binary label in the
 * file.  It is used for documentation, and so application programs can
 * process the binary label correctly, without having to be told what kind it
 * is.  It defaults to a null string.  The valid values are to be maintained
 * in a name registry, which will document the actual data layout for each
 * BLTYPE.
 */
    public String getBLType() {		return _bltype; }
    public void setBLType(String s) {	_bltype = s.toUpperCase(); }

/***********************************************************************
 * Pixel size is a completely synthetic (get-only) property.  It is affected
 * by <code>Format</code>, <code>IntFmt</code>, and <code>RealFmt</code>
 * properties, and is updated whenever one of those changes.
 */
    public int getPixelSize() {		return _pixel_size; }
    protected void calcPixelSize()
    {
	_pixel_size = VicarDataFormat.getPixelSize(_format, _intfmt, _realfmt);
	calcRecsize();
    }

/***********************************************************************
 * Creates a deep copy of the label bean.  Nothing can be done to the returned
 * copy that in any way affects the original.  This allows the bean to be
 * returned to the application from the I/O system, without worrying about
 * the application messing up some internal state.
 */
    public Object clone()
    {
	SystemLabel lbl;
	try {
	    lbl = (SystemLabel)super.clone();
	} catch (Exception e) {
	    return null;
	}

	// _extraItems is the only one that requires a deep copy.
	// This is a *very* deep copy because the elements are VicarLabelItem's.

	lbl._extraItems = (ArrayList)((ArrayList)(lbl._extraItems)).clone();
	for (int i=0; i < lbl._extraItems.size(); i++) {
	    lbl._extraItems.set(i,
			((VicarLabelItem)(lbl._extraItems.get(i))).clone());
	}

	return lbl;
    }

}

