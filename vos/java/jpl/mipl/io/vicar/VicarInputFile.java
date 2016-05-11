package jpl.mipl.io.vicar;

import jpl.mipl.io.streams.*;

import java.io.*;
import com.sun.media.jai.codec.*;
import java.lang.reflect.*;
import java.beans.*;
import java.awt.image.*;

import javax.media.jai.*;
import javax.imageio.stream.*;
import java.nio.ByteOrder;
import java.awt.Rectangle;
import java.awt.Dimension;
import java.awt.Point;
import javax.imageio.ImageReadParam;

import jpl.mipl.io.util.DOMutils;

/**
 * This class manages a single VICAR input image file.
 * <p>
 * All accesses to the VICAR file are thread-safe, assuming that nobody else
 * tries to access the underlying stream directly.  Thus, multiple threads
 * can issue simultaneous <code>readRecord()</code> or <code>readTile()</code>
 * requests, although each request is handled one at a time via synchronization
 * on the <code>VicarInputFile</code> object.  However, if you have a
 * sequential-only stream, all accesses must still be strictly sequential...
 * meaning that the use of multiple threads with sequential streams will not
 * work (the request order would be non-deterministic).  For random-hard
 * streams, threads will work but could cause performance hits depending on
 * the ordering of the requests.  Random-easy streams should be fine.
 * <p>
 * @see VicarInputImage
 * @see VicarInput
 */

public class VicarInputFile implements VicarInput
{
    /** The input stream object.  Must be a <code>InputStream</code> or
     *  <code>DataInput</code>, but specific subclasses may be handled
     *  specially.  See the <code>stream_</code>*() functions.
     */
    protected Object _input_stream;
    
    String filename = null; 
    // the filename, for use by native code which needs a filename instead of an
    // ImageInputStream

    /** Wrapper around <code>_input_stream</code> that implements
     *  <code>ImageInputStreamStride</code> and handles any required
     *  byte-swapping etc.
     */
    protected ImageInputStreamStride _input_stream_wrap;

    protected VicarLabel _label;
    protected SystemLabel _system;
    protected int _lblsize_front;
    protected int _record_size;
    protected int _lblsize_eol;

    /** Random access mode flags */
    protected boolean _random_allowed;
    protected boolean _random_easy;

    protected long _image_size_bytes;

    protected long _current_file_pos;	// Current position for seq files

    /** Amount to skip at beginning of file */
    protected long _file_offset;

    protected boolean _file_opened;

    protected VicarDataFormat _data_format;
    protected VicarDataFormat _binary_data_format;

    // Internal data buffers

    protected int _int_buffer[];
    protected int _int_bufsize;
    protected float _float_buffer[];
    protected int _float_bufsize;
    protected double _double_buffer[];
    protected int _double_bufsize;
    
    // new *********
	VicarBinaryLinePrefix _vicarBinaryLinePrefix = null;
	VicarBinaryHeader _vicarBinaryHeader = null;
   
   
   // boolean debug = true; 
   boolean debug_tile = false;
   // boolean debug_tile = true;
   
   boolean debug = false;
   
   // new ImageReadParam values to handle subsampling, cropping and band select
   // these could either be null if unused or the values set to whole image
   protected ImageReadParam _param ;
   protected Rectangle _sourceRegion;
   protected int[] _sourceBands;
   protected int _sourceBandsLen = 0;
   protected int[] _destBands;
   protected int _sourceXSubsample = 1;
   protected int _sourceYSubsample = 1;
   protected int _subsamplingXOffset = 0;
   protected int _subsamplingYOffset = 0;
   protected Rectangle _originalRegion;
   protected Rectangle _destinationRegion;
   protected Dimension originalDimension;
   protected Point sourceOrigin;
   protected int maxXTile, maxYTile;
   
   
   // add getters and setters for these
////////////////////////////////////////////////////////////////////////

/***********************************************************************
 * Yucky static initializer that determines if we're on 1.3 or later.
 * 1.3 can't handle double/float in things like BandedSampleModel but
 * 1.4 can.  Also determines JAI 1.1.1 or later... 1.1.2 can handle
 * JDK 1.4's DataBufferFloat but 1.1.1 can't (note both JDK 1.4 and JAI
 * have a DataBufferFloat class).
 */
    protected static boolean jdk_version_1_3;
    protected static boolean jai_version_1_1_1;
    static {
	jdk_version_1_3 = false;	// assume we're okay if things bust
	try {				// ignore any security manager issues
	    String ver = System.getProperty("java.specification.version");
	    if (ver != null) {
		if (ver.startsWith("1.3"))
		    jdk_version_1_3 = true;
	    }
	} catch (Exception e) { }

	jai_version_1_1_1 = false;	// assume we're okay if things bust
	try {
	    Package pkg = Class.forName("javax.media.jai.JAI").getPackage();
	    if (pkg != null) {
		String ver = pkg.getSpecificationVersion();
		if (ver.equals("1.1"))	// 1.1.2 has "1.1-mr" or "1.1-mrd".
		    jai_version_1_1_1 = true;
	    }
	} catch (Exception e) { }
    }

/***********************************************************************
 * Dummy constructor (for now). Need to add good ones that call open().
 */
    public VicarInputFile()
    {
	_file_opened = false;
	_input_stream = null;
	_label = null;
	_system = null;
	_lblsize_front = 0;
	_lblsize_eol = 0;
	_random_allowed = false;
	_random_easy = false;
	_image_size_bytes = 0;
	_current_file_pos = 0;
	_file_offset = 0;
	_data_format = null;
	_binary_data_format = null;
	_int_buffer = null;
	_int_bufsize = -1;
	_float_buffer = null;
	_float_bufsize = -1;
	_double_buffer = null;
	_double_bufsize = -1;
	// set defaults or null for imageReadParams derived values
	ImageReadParam _param = null;
	Rectangle _sourceRegion = null; // default is whole image, set once gheader is read
	// int[] sourceBands = [0]; // can;t set this until we have read the header
	_sourceXSubsample = 1;
	_sourceYSubsample = 1;
	_subsamplingXOffset = 0;
	_subsamplingYOffset = 0;
    }

/***********************************************************************
 * Constructor that calls <code>open(String)</code>.
 * @throws IOException
 */
    public VicarInputFile(String fn) throws IOException
    {
	this();
	open(fn);
    }

/***********************************************************************
 * Sets the file offset to the given value.  The file offset is the
 * amount to skip at the beginning of the file, before the VICAR label
 * even starts.  The purpose of it is to handle dual PDS/VICAR files;
 * the PDS file describes the offset where the VICAR file starts.  The
 * offset is 0-based.
 * <p>
 * The offset <em>must</em> be set before the file is opened, or an
 * <code>IllegalStateException</code> is thrown.
 * @throws IllegalStateException if the file is already open.
 */
    public void setFileOffset(long offset)
    {
	if (_file_opened)
	    throw new IllegalStateException("Cannot setFileOffset on a file that is already open");
	_file_offset = offset;
    }

/***********************************************************************
 * Returns the value of the file offset.  For information only; apps should
 * not need to depend on this.
 */
    public long getFileOffset()
    {
	return _file_offset;
    }

/***********************************************************************
 * Opens a file given an <code>InputStream</code>.  Random access is
 * assumed to be needed, so the stream is wrapped if necessary.
 * @throws IOException
 */
    public void open(InputStream is) throws IOException
    {
	open(is, false);
    }

/***********************************************************************
 * Opens a file given an <code>InputStream</code>.  The
 * <code>sequential_only</code> flag is used to determine whether random
 * access is needed (thus stream wrappers), or if strict sequential is okay.
 * @throws IOException
 */
    public synchronized void open(InputStream is, boolean sequential_only)
				throws IOException
    {
	_input_stream = SeekableStream.wrapInputStream(is, !sequential_only);

	if (sequential_only) {
	    _random_allowed = false;
	    _random_easy = false;
	}
	else {
	    _random_allowed = true;
	    _random_easy = (_input_stream instanceof FileSeekableStream ||
			    _input_stream instanceof ByteArraySeekableStream);
	}

	openInternal();
    }

/***********************************************************************
 * Opens a file given a <code>RandomAccessFile</code>.
 * @throws IOException
 */
    public synchronized void open(RandomAccessFile raf) throws IOException
    {
	_input_stream = new FileSeekableStream(raf);

	_random_allowed = true;
	_random_easy = true;

	openInternal();
    }

/***********************************************************************
 * Opens a file given a <code>SeekableStream</code>.
 * @throws IOException
 */
    public synchronized void open(SeekableStream ss) throws IOException
    {
	_input_stream = ss;

	_random_allowed = ss.canSeekBackwards();
	_random_easy = false;
	if (_random_allowed)
	    _random_easy = (_input_stream instanceof FileSeekableStream ||
			    _input_stream instanceof ByteArraySeekableStream);

	openInternal();
    }

/***********************************************************************
 * Opens a file given a filename.
 * What about URL's?
 * @throws IOException
 */
    public synchronized void open(String fn) throws IOException
    {
    filename = fn;
	open(new RandomAccessFile(fn, "r"));
	
    }

/***********************************************************************
 * Opens a file given an <code>ImageInputStream</code>.
 */
    public synchronized void open(ImageInputStream iis) throws IOException
    {
	_input_stream = iis;

	_random_allowed = true;
	_random_easy = (_input_stream instanceof FileImageInputStream);

	openInternal();
    }

/***********************************************************************
 * Opens a file given a generic <code>Object</code>.  The appropriate
 * <code>open()</code> routine is called based on whether the object is
 * a <code>String</code> (filename), <code>SeekableStream</code>,
 * <code>ImageInputStream</code>, <code>RandomAccessFile</code>, or
 * generic <code>InputStream</code>.
 * @throws UnsupportedOperationException if it's an unrecognized type.
 */

    public void open(Object obj) throws IOException
    {
	if (obj instanceof String) {
	    open((String) obj);
	    return;
	}
	if (obj instanceof SeekableStream) {
	    open((SeekableStream) obj);
	    return;
	}
	if (obj instanceof ImageInputStream) {
	    open((ImageInputStream) obj);
	    return;
	}
	if (obj instanceof RandomAccessFile) {
	    open((RandomAccessFile) obj);
	    return;
	}
	if (obj instanceof InputStream) {
	    open((InputStream) obj);
	    return;
	}

	throw new UnsupportedOperationException(
			"Unrecognized object type in VicarInputFile.open");
    }

/***********************************************************************
 * Does the actual work of opening the file.  Reads in the first part
 * of the label, and sets up the SystemLabel object.
 * @throws IOException
 */
    protected void openInternal() throws IOException
    {
	// Make sure we're at the beginning of the file/stream.
	// Only does anything if random-access.
	seekToLocation(0);

	_lblsize_front = 0;
	_lblsize_eol = 0;
	
	

	setupLabels();

	// Get the host and data formats and set up the VicarDataFormat object.

	_data_format = new VicarDataFormat(_system.getHost(),
			_system.getIntFmt(), _system.getRealFmt());

	// This is a bit messy but avoids having the io.streams package
	// depend on the io.vicar package...

	int int_order = ImageInputStreamStride.HIGH_ORDER;
	if (_data_format.getIntFormatCode() == VicarDataFormat.INT_FMT_LOW)
	    int_order = ImageInputStreamStride.LOW_ORDER;

	int float_order = ImageInputStreamStride.HIGH_ORDER;
	// if (_data_format.getIntFormatCode() == VicarDataFormat.REAL_FMT_RIEEE)
	if (_data_format.getRealFormatCode() == VicarDataFormat.REAL_FMT_RIEEE)
	    float_order = ImageInputStreamStride.LOW_ORDER;
	// if (_data_format.getIntFormatCode() == VicarDataFormat.REAL_FMT_VAX)
	if (_data_format.getRealFormatCode() == VicarDataFormat.REAL_FMT_VAX)
	    float_order = ImageInputStreamStride.VAX_ORDER;

	// If an ImageInputStream, set the byte ordering there too.  If
	// it's VAX or inconsistent, leave it BIG and the Stride class
	// will take care of it (it re-verifies the order is right).
	
	if (debug) {
		System.out.println("VicarInputFile.openInternal()");
		System.out.println("_input_stream = "+_input_stream);
		System.out.println("_system "+_system.toString());
		System.out.println("_data_format="+_data_format);
		// Get the host and data formats and set up the VicarDataFormat object.
		System.out.println("_system.getHost() = "+_system.getHost());
		System.out.println("_system.getIntFmt() = "+_system.getIntFmt());
		System.out.println("_system.getRealFmt() = "+_system.getRealFmt());
		
			System.out.println("VicarInputFile.openInternal()");
			System.out.println("ImageInputStreamStride.LOW_ORDER = "+ImageInputStreamStride.LOW_ORDER);
			System.out.println("ImageInputStreamStride.HIGH_ORDER = "+ImageInputStreamStride.HIGH_ORDER);
			System.out.println("_data_format.getRealFormatCode() ="+_data_format.getRealFormatCode());
			System.out.println("VicarDataFormat.REAL_FMT_RIEEE ="+VicarDataFormat.REAL_FMT_RIEEE);
			System.out.println("VicarDataFormat.REAL_FMT_IEEE ="+VicarDataFormat.REAL_FMT_IEEE);
			System.out.println("int_order="+int_order+", float_order="+float_order);
	}
	
	

	if (_input_stream instanceof ImageInputStream) {
	    if (int_order == ImageInputStreamStride.LOW_ORDER &&
		float_order == ImageInputStreamStride.LOW_ORDER) {
	    if (debug) System.out.println("VicarInputFile.openInternal()  ByteOrder.LITTLE_ENDIAN");
		((ImageInputStream)_input_stream).setByteOrder(
						ByteOrder.LITTLE_ENDIAN);
	    }
	    else {
	    if (debug) System.out.println("VicarInputFile.openInternal()  ByteOrder.BIG_ENDIAN");
	    
		((ImageInputStream)_input_stream).setByteOrder(
						ByteOrder.BIG_ENDIAN);
	    }
	}
	
	

	_input_stream_wrap = new ImageInputStreamStride(_input_stream,
					int_order, float_order);

	_file_opened = true;
	// gives subclassers a hook to do something else here
	
	
	openInternalLast();
    }
    
	/**
	 * a call to readBinaryHeaderData will read the Binary header 
	 * and add the Object.
	 * This call allows the VicarBinaryHeader to be set from some where else
	 * **/
    public void setVicarBinaryHeader(VicarBinaryHeader vbh) {
		_vicarBinaryHeader = vbh ;
		
    }
    
	public VicarBinaryHeader getVicarBinaryHeader() {
			return _vicarBinaryHeader ;
		
	}
		
    /** 
     * forces the reading of the Binary header. A VicarBinaryHeader Object is created 
     * and the data is stored in it. The VicarBinaryHeader should be added
     * to the VicarMetadata Object.
     * Most Vicar files do NOT contain a binary header.
     *
     */
	protected void readBinaryHeaderData() {
		
		int nlb = _system.getNLB(); // number of records in the header
		int recsize = _system.getRecsize(); // record size 
		try {
		
			if (nlb == 0 ) {
				return ;
			}
			else {
							
				_vicarBinaryHeader = new VicarBinaryHeader(recsize, nlb);
						// read the data which is nlb * recsize bytes
						// bltype, bhost, bintfmt and brealfmt are all needed by application programs to
						// properly interpret the data in a binary prefix or label
					String bltype = _system.getBLType();
					_vicarBinaryHeader.set_bltype(bltype);
	
					String brealfmt = _system.getBRealFmt();
						_vicarBinaryHeader.set_brealfmt(brealfmt);
		
					String bintfmt = _system.getBIntFmt();
						_vicarBinaryHeader.set_bintfmt(bintfmt);
		
					String bhost = _system.getBHost();
						_vicarBinaryHeader.set_bhost(bhost);
	
					if (debug) { System.out.println("Created VicarBinaryHeader nlb="+nlb+
							" recsize="+recsize); }
						
				VicarBinaryLabel vbl = getBinaryHeader();
				byte[] bdata = vbl.getBuffer();
				// put the data into the Object
				_vicarBinaryHeader.setData(bdata);
				// this will be aded to the metadata Object
			}
		  
		}
		catch (IOException ioe) {
			System.out.println("IOException "+ioe);
			ioe.printStackTrace();
			// clean up/ null out the prefix
			return; // or throw ioe;
		}
      
	}
    
    /**
     * 
     * @return
     */
	public VicarBinaryLinePrefix getVicarBinaryLinePrefix() {
		 return _vicarBinaryLinePrefix ;
	}
	
	public void setVicarBinaryLinePrefix ( VicarBinaryLinePrefix vblp) {
		_vicarBinaryLinePrefix = vblp;
	}
	
    /*******************************************************************
     * readLinePrefixData
     *
     * Determine if there is any prefix data in this file. If so get the 
     * Object to hold it and read it in.
     * This is called after the header has been read.
     */
	protected void readLinePrefixData(int nbb) {
		 int bandList[] = null; // if a bandList is set we should only read the prefixs of the 
		// readPrefixData( null);
		readLinePrefixData(nbb,  bandList);
	}
	
   protected void readLinePrefixData(int nbb, int bandList[]) {
   	
   	if (debug) {
   	
	System.out.println("*******************************");
	System.out.println("**");
	System.out.println("** readLinePrefixData");
	System.out.println("**");
	System.out.println("** nbb="+nbb);
	System.out.println("*******************************");
   	}
   	
   	boolean useBobs = true; // use Bob Deens getBinaryPrefix
	// bands requested
	int i;
	// int nbb = _system.getNBB();
	if (nbb == 0) {
		return;
	}
		int nb = _system.getNB();
		int nl = _system.getNL();
	   	_vicarBinaryLinePrefix = new VicarBinaryLinePrefix(nb, nl, nbb);
	   	// read the data
	   	// bltype, bhost, bintfmt and brealfmt are all needed by application programs to
	   	// properly interpret the data in a binary prefix or label
	String bltype = _system.getBLType();
	_vicarBinaryLinePrefix.set_bltype(bltype);
	
	String brealfmt = _system.getBRealFmt();
		_vicarBinaryLinePrefix.set_brealfmt(brealfmt);
		
	String bintfmt = _system.getBIntFmt();
		_vicarBinaryLinePrefix.set_bintfmt(bintfmt);
		
	String bhost = _system.getBHost();
		_vicarBinaryLinePrefix.set_bhost(bhost);
	
	if (debug) { System.out.println("Created VicarBinaryLinePrefix nl="+nl+" nb="+
		nb+" nbb="+nbb+" bltype="+bltype); }
	
	
	   	
	   	
		// VicarBinaryLabel getBinaryPrefix(int n2, int n3)
		
		int line;
		int band, band_ind;
		boolean bandListGiven;
			// Array pointers that are filled in from UnpackedImageData.getXxxData()
		byte bdata[] = new byte[nbb];

		int num_bands = _system.getNB(); // sm.getNumBands();
		if (bandList != null && num_bands > bandList.length)
				num_bands = bandList.length;

			bandListGiven = (bandList != null);	// flag if it was given
			if (!bandListGiven) {			// make one for convenience
				bandList = new int[num_bands];
				for (i=0; i<num_bands; i++)
				bandList[i] = i;
			}

	    int org_code = _system.getOrgCode();
		if (debug) {
		// if (true) {
				System.out.println("VicarInputFile.readLinePrefixData nb="+nb+"  nl="+nl+"  nbb="+nbb+" +_system.getNBB()="+_system.getNBB());
				System.out.println(" _system.getNS() "+_system.getNS()+"  _system.getNL() "+_system.getNL()+"  _system.getNB() "+_system.getNB());
				System.out.println(" _system.getN1() "+_system.getN1()+"  _system.getN2() "+_system.getN2()+"  _system.getN3() "+_system.getN3());
				System.out.println(" num_bands="+num_bands+" bandList ");
				
				  for (i=0; i<num_bands; i++) {
					System.out.println("    bandList["+i+"] = " +bandList[i] );
				  }
			System.out.println(" org_code="+org_code +"  SystemLabel.ORG_BSQ="+SystemLabel.ORG_BSQ);	
			System.out.println(" ----------------------------------------------------------------");
		}
		
	

		switch (org_code) {

			case SystemLabel.ORG_BSQ:
			
			// this seems VERY inefficient
			VicarDataFormat vdf = new VicarDataFormat(_system.getHost()) ; // "NATIVE"
			if (debug) {System.out.println("VicarDataFormat="+vdf+" HOST="+_system.getHost() );}
			
			VicarBinaryLabel vbl = new VicarBinaryLabel(nbb, vdf);
			if (debug) {System.out.println("ORG_BSQ nbb="+nbb+"  ****************************");}

			for (band_ind=0; band_ind < num_bands; band_ind++) {
				band = bandList[band_ind];
				if (debug) {System.out.println("band_list loop band_ind="+band_ind );}
				// _vicarBinaryLinePrefix.setPrefix(int band, int line, byte[] b, int size)
						// read the data
	   	
	   	
				// VicarBinaryLabel getBinaryLinePrefix(int n2, int n3)
				
				// case SystemLabel.TYPE_BYTE:
				try {
					
					for (line=0; line < nl; line++) {
						// n2 is line, n3 is band
						// System.out.println("readLinePrefix line="+line+" band="+band);
						
					   if (useBobs == true) {
						
						vbl = getBinaryPrefix(line, band);
						_vicarBinaryLinePrefix.setPrefix(band, line, vbl.getBuffer());
						}
						else {
							System.out.println("use new data reader ****************** ");
							bdata = getBinaryPrefix(line, band, bdata);
							_vicarBinaryLinePrefix.setPrefix(band, line, bdata);
						}
					// readRecordNS(bdata, x, w, offset, data.pixelStride, line+y,band);
					// offset += data.lineStride;
					} 
				 }
				 catch (IOException ioe) {
						System.out.println("IOException "+ioe);
						ioe.printStackTrace();
						// clean up/ null out the prefix
					return; // or throw ioe;
				 }
				}
				
			    if (debug) {
			    	_vicarBinaryLinePrefix.print();
			    }
				break;

/**************
			case SystemLabel.ORG_BIL:

			for (line=0; line < h; line++) {

				// case SystemLabel.TYPE_BYTE:
				for (band_ind=0; band_ind < num_bands; band_ind++) {
					band = bandList[band_ind];
					bdata = data.getByteData(band);
					offset = data.getOffset(band) + line * data.lineStride;
					readRecordNS(bdata, x, w, offset, data.pixelStride, band, line+y);
					}
			}
			break;
			
			case SystemLabel.ORG_BIP:

				
					// Is band_offsets[] needed?  Not sure HotSpot is good enough
					// to optimize data.getOffset(band_ind) inside the pixel loop...
					int band_offsets[] = new int[num_bands];
					for (band_ind=0; band_ind<num_bands; band_ind++) {
						band_offsets[band_ind] = data.getOffset(band_ind);
					}

					// get starting band and # of bands to read from file
					// Note that band_ind indexes into the SM whild band (or
					// bandList[band_ind]) indexes into the file.

					int sb = 0;
					int nb = num_bands;
					if (bandListGiven) {		// get min/max of bandList
						int min = bandList[0];
						int max = bandList[0];
						for (i=1; i<num_bands; i++) {
						if (bandList[i] < min)
							min = bandList[i];
						if (bandList[i] > max)
							max = bandList[i];
						}
						sb = min;
						nb = max - min + 1;
					}

					
						// case SystemLabel.TYPE_BYTE:
						bbdata = data.getByteData();
						byte bbuf[] = new byte[nb];
						for (line=0; line < h; line++) {
							for (samp=0; samp < w; samp++) {
							readRecordNS(bbuf, sb, nb, 0, 1, samp+x,line+y);
							for (band_ind=0; band_ind < num_bands;
											band_ind++) {
								band = bandList[band_ind];
								bbdata[band_ind][band_offsets[band_ind] +
										 samp * data.pixelStride +
										 line * data.lineStride]
								= bbuf[band-sb];
							}
							}
						}
						break;
						**************/
			
		} // end of switch
			
			
   }
/***********************************************************************
 * openInternalLast * 
 * called at the end of openInternal. Can be used by subclasses to do any 
 * extra work once openInternal has completed. Created so 
 * PDSInputFile and ISISInputFile  oculd have the oportunity to modify 
 * _input_stream_wrap.  */
protected void openInternalLast() {
	
	// provided for subclasses to put good stuff here
	if (debug) {
		System.out.println("VicarInputFile.openInternalLast()");
	}
	// get any binary prefix
	int nlb = _system.getNLB();
	readBinaryHeaderData(); // this is a Vicar only operation
	
	int nbb = _system.getNBB();
	readLinePrefixData(nbb); // this is a Vicar only operation
}

/***********************************************************************
 * Sets up the label information for the file.  This is intended to be
 * overridden by subclasses for different label formats.  The method must
 * set up: _label (can be null), _lblsize_front, _system (cannot be null),
 * _image_size_bytes, and _current_file_pos.
 */
    protected void setupLabels() throws IOException
    {
	_label = new VicarLabel();
	_lblsize_front = _label.readLabelChunk(_input_stream);

	// Extract System label items

	try {
	    _system = new SystemLabel(_label.getSystem());
	} catch (IntrospectionException e) {
	    throw new IOException(
			"Error creating System label: " + e.getMessage());
	} catch (InvocationTargetException e) {
	    throw new IOException(
			"Error creating System label: " + e.getMessage());
	} catch (IllegalAccessException e) {
	    throw new IOException(
			"Error creating System label: " + e.getMessage());
	}

	// Set up defaults for missing label items.
	// Note that the host format defaults for input files are VAX, because
	// that was the only kind of file in existence before the host type
	// labels were added.  Output files default to Java.

	if (!_system.isHostValid()) {
	    _system.setHost("VAX-VMS");
	}
	if (!_system.isIntFmtValid()) {
	    _system.setIntFmt("LOW");
	}
	if (!_system.isRealFmtValid()) {
	    _system.setRealFmt("VAX");
	}
	_image_size_bytes = (_system.getNLB() +
				((long)_system.getN2() * (long)_system.getN3()))
			* _system.getRecsize();

	
	_record_size = _system.getRecsize();
	// If random access is easy, and there are EOL labels, read the
	// entire label now

/* Disabled - no need to process them all if nobody asks for them.  Deferred
   execution, and all that...
   !!!!If this is ever re-enabled, take _file_offset into account...
	if (_random_easy && _system.getEOL() != 0) {
	    stream_seek(_lblsize_front + _image_size_bytes);
	    _lblsize_eol = _label.readLabelChunk(_input_stream);
	    stream_seek(_lblsize_front);
	    _label.setReadComplete(true);
	}
Disabled */

	// If there are no EOL labels, the label is done in any case

	if (_system.getEOL() == 0)
	    _label.setReadComplete(true);

	_current_file_pos = _lblsize_front + _file_offset;

    }

/***********************************************************************
 * Closes the file or stream.
 */
    public synchronized void close() throws IOException
    {
	stream_close();
	_file_opened = false;
    }

////////////////////////////////////////////////////////////////////////

/***********************************************************************
 * Returns a deep copy of the <code>VicarLabel</code> object for this file.
 * This routine should <em>not</em> be used to retrieve the system label
 * bean; use <code>getSystemLabel()</code> instead.
 * <p>
 * Note that requesting the label via this routine will cause the EOL labels
 * to be read if needed.  On a sequential-only stream, this could get you
 * into trouble and thus this should be done only after the image data has
 * been read.  If random access is possible but not easy, this can be done
 * before reading the data but you could take a significant performance hit.
 * If it is easy, then this can be called any time.
 * <p>
 * @see #getSystemLabel()
 * @throws IOException if an EOL read is required, and fails.  Will not occur
 * for random-easy streams.
 */
    public synchronized VicarLabel getVicarLabel() throws IOException
    {
	// The second condition is redundant, but just in case...
	if (!_label.isReadComplete() && _system.getEOL() != 0) {
	    seekToLocation(_lblsize_front + _image_size_bytes);
	    _lblsize_eol = _label.readLabelChunk(_input_stream);

	    // We do not seek back because of sequential or random-hard files.
	    // The next file access will go wherever it needs to; let's
	    // minimize work here.
	}
	_label.setReadComplete(true);

	return (VicarLabel)_label.clone();
    }

/***********************************************************************
 * Indicates whether or not the <code>VicarLabel</code> has been completely
 * read.  This can be used with sequential or random-hard streams to determine
 * whether or not <code>getVicarLabel()</code> will do bad things.  The label
 * will always be completely read for random-easy streams, or for any stream
 * if there are no EOL labels.
 * @see #getVicarLabel()
 */
    public synchronized boolean isLabelComplete()
    {
	return _label.isReadComplete();
    }

/***********************************************************************
 * Retrieves a deep copy of the <code>SystemLabel</code> object associated
 * with this image.  This should not be confused with
 * <code>VicarLabel.getSystem()</code>, which returns a
 * <code>VicarLabelSet</code> rather than a <code>SystemLabel</code> object.
 * The <code>SystemLabel</code> object returned by this function need not
 * contain the same information as the System part of the
 * <code>VicarLabel</code> object (e.g. a synthetic image made up of multiple
 * files).
 */
    public synchronized SystemLabel getSystemLabel()
    {
	return (SystemLabel)_system.clone();
    }

/***********************************************************************
 * Return true if random access to this file is <em>possible</em>.  It
 * might be expensive, however.
 * @see #isRandomAccessEasy()
 */
    public boolean isRandomAccessAllowed()
    {
	return _random_allowed;
    }

/***********************************************************************
 * Return true if random access to this file is <em>easy</em>, meaning
 * that the underlying stream/file supports seekable random access, as
 * opposed to drastic means like mark/reset, contents caching, or re-opening.
 */
    public boolean isRandomAccessEasy()
    {
	return _random_easy;
    }

////////////////////////////////////////////////////////////////////////

/***********************************************************************
 * Low-level read function, returns a byte array for the given record.
 * The most normal call, to read an entire record, would be
 * <code>x.readRecord(data, 0, 0, 0, 1, line, band);</code>
 * @param data The array to read into
 * @param start Starting position in the record.  Normally 0 to read the whole
 *  record.
 * @param length Number of pixels to read.  If 0, the rest of the record is
 *  read.
 * @param offset Offset into the data array at which to put the data.
 * @param pixelStride Offset from one pixel to the next in the array.  For
 *  example, a stride of 1 has all pixels contiguous while a stride of 3
 *  allows band interleaved pixels in the array (pixels read from the file
 *  will be put 3 elements apart in the array).  This has nothing to do with
 *  what is read from the file - only where the data is put in the array.
 *  Note that this value is measured in pixels or array elements - not bytes.
 * @param n2 "Line number" to read (for BSQ organization).  (For BIL: band
 *  number, for BIP: sample number).  The second dimension's index.
 * @param n3 "Band number" to read (for BSQ organization).  (For BIL or BIP:
 *  line number).  The third dimension's index.
 * @throws NonSequentialAccessException (subclass of IOException)
 * @throws IOException
 */
    public synchronized void readRecord(byte[] data,
			int start, int length, int offset, int pixelStride,
			int n2, int n3) throws IOException
    {
	readRecordNS(data, start, length, offset, pixelStride, n2, n3);
    }

/** Non-synchronized version */
    protected void readRecordNS(byte[] data,
			int start, int length, int offset, int pixelStride,
			int n2, int n3) throws IOException
    {
	long pos = calcFilePos(start, n2, n3);
	
	seekToLocation(pos);
	if (length == 0)
	    length = _system.getN1() - start;
	// System.out.println("VicarInputFile.readRecordNS BYTE pos="+pos+" start="+start+" offset="+offset+
	// 		" n2="+n2+" n3="+n3+" length="+length+" stride="+pixelStride);
			
	if (_system.getFormatCode() == SystemLabel.TYPE_BYTE) {
            _input_stream_wrap.readBytes(data, offset, length, pixelStride);
	    _current_file_pos += length * _system.getPixelSize();
	    // System.out.println("  _current_file_pos "+_current_file_pos);
	}
	else 		//!!!! OTHER CASES: MAYBE DONE IN WRAPPER????!!!!
	    throw new UnsupportedOperationException("Data type conversions not implemented yet!!!!");
    }

/***********************************************************************
 * Low-level read function, returns a short array for the given record.
 * Follow the See Also link for parameters
 * @see #readRecord(byte[], int, int, int, int, int, int)
 */
    public synchronized void readRecord(short[] data,
			int start, int length, int offset,
			int pixelStride, int n2, int n3) throws IOException
    {
	readRecordNS(data, start, length, offset, pixelStride, n2, n3);
    }

/** Non-synchronized version */
    protected void readRecordNS(short[] data,
			int start, int length, int offset,
			int pixelStride, int n2, int n3) throws IOException
    {
	long pos = calcFilePos(start, n2, n3);
	seekToLocation(pos);
	if (length == 0)
	    length = _system.getN1() - start;
	if (_system.getFormatCode() == SystemLabel.TYPE_HALF) {
            _input_stream_wrap.readShorts(data, offset, length, pixelStride);
	    _current_file_pos += length * _system.getPixelSize();
	}
	else if (_system.getFormatCode() == SystemLabel.TYPE_USHORT) {
            _input_stream_wrap.readUShorts(data, offset, length, pixelStride);
	    _current_file_pos += length * _system.getPixelSize();
	}
	else 		//!!!! OTHER CASES: MAYBE DONE IN WRAPPER????!!!!
	    throw new UnsupportedOperationException("Data type conversions not implemented yet!!!!");
    }

/***********************************************************************
 * Low-level read function, returns an unsigned short array for the given
 * record.  Although unsigned short is not a VICAR data type, it is useful
 * for some other formats which use this library.  Note that a short array
 * must be used, although the data should be interpreted as unsigned.
 * For this reason, a different method name is needed.
 * <p>
 * Follow the See Also link for parameters
 * @see #readRecord(byte[], int, int, int, int, int, int)
 */
    public synchronized void readRecordUshort(short[] data,
			int start, int length, int offset,
			int pixelStride, int n2, int n3) throws IOException
    {
	readRecordUshortNS(data, start, length, offset, pixelStride, n2, n3);
    }

/** Non-synchronized version */
    protected void readRecordUshortNS(short[] data,
			int start, int length, int offset,
			int pixelStride, int n2, int n3) throws IOException
    {
	long pos = calcFilePos(start, n2, n3);
	seekToLocation(pos);
	if (length == 0)
	    length = _system.getN1() - start;
	if (_system.getFormatCode() == SystemLabel.TYPE_USHORT) {
            _input_stream_wrap.readUShorts(data, offset, length, pixelStride);
	    _current_file_pos += length * _system.getPixelSize();
	}
	else if (_system.getFormatCode() == SystemLabel.TYPE_HALF) {
            _input_stream_wrap.readShorts(data, offset, length, pixelStride);
	    _current_file_pos += length * _system.getPixelSize();
	}
	else 		//!!!! OTHER CASES: MAYBE DONE IN WRAPPER????!!!!
	    throw new UnsupportedOperationException("Data type conversions not implemented yet!!!!");
    }

/***********************************************************************
 * Low-level read function, returns an int array for the given record.
 * Follow the See Also link for parameters
 * @see #readRecord(byte[], int, int, int, int, int, int)
 */
    public synchronized void readRecord(int[] data,
			int start, int length, int offset, int pixelStride,
			int n2, int n3) throws IOException
    {
	readRecordNS(data, start, length, offset, pixelStride, n2, n3);
    }

/** Non-synchronized version */
    protected void readRecordNS(int[] data,
			int start, int length, int offset, int pixelStride,
			int n2, int n3) throws IOException
    {
	long pos = calcFilePos(start, n2, n3);
	seekToLocation(pos);
	if (length == 0)
	    length = _system.getN1() - start;
	if (_system.getFormatCode() == SystemLabel.TYPE_FULL) {
	    _input_stream_wrap.readInts(data, offset, length, pixelStride);
	    _current_file_pos += length * _system.getPixelSize();
	}
	else		 //!!!! OTHER CASES: MAYBE DONE IN WRAPPER????!!!!
	    throw new UnsupportedOperationException("Data type conversions not implemented yet!!!!");
    }

/***********************************************************************
 * Low-level read function, returns a float array for the given record.
 * Follow the See Also link for parameters
 * @see #readRecord(byte[], int, int, int, int, int, int)
 */
    public synchronized void readRecord(float[] data,
			int start, int length, int offset,
			int pixelStride, int n2, int n3) throws IOException
    {
	readRecordNS(data, start, length, offset, pixelStride, n2, n3);
    }

/** Non-synchronized version */
    protected void readRecordNS(float[] data,
			int start, int length, int offset,
			int pixelStride, int n2, int n3) throws IOException
    {
	long pos = calcFilePos(start, n2, n3);
	seekToLocation(pos);
	if (length == 0)
	    length = _system.getN1() - start;
	if (_system.getFormatCode() == SystemLabel.TYPE_REAL) {
	    _input_stream_wrap.readFloats(data, offset, length, pixelStride);
	    _current_file_pos += length * _system.getPixelSize();
	}
	else 		//!!!! OTHER CASES: MAYBE DONE IN WRAPPER????!!!!
	    throw new UnsupportedOperationException("Data type conversions not implemented yet!!!!");
    }

/***********************************************************************
 * Low-level read function, returns a double array for the given record.
 * Follow the See Also link for parameters
 * @see #readRecord(byte[], int, int, int, int, int, int)
 */
    public synchronized void readRecord(double[] data,
			int start, int length, int offset,
			int pixelStride, int n2, int n3) throws IOException
    {
	readRecordNS(data, start, length, offset, pixelStride, n2, n3);
    }

/** Non-synchronized version */
    protected void readRecordNS(double[] data,
			int start, int length, int offset,
			int pixelStride, int n2, int n3) throws IOException
    {
	long pos = calcFilePos(start, n2, n3);
	seekToLocation(pos);
	if (length == 0)
	    length = _system.getN1() - start;
	if (_system.getFormatCode() == SystemLabel.TYPE_DOUB) {
	    _input_stream_wrap.readDoubles(data, offset, length, pixelStride);
	    _current_file_pos += length * _system.getPixelSize();
	}
	else 		//!!!! OTHER CASES: MAYBE DONE IN WRAPPER????!!!!
	    throw new UnsupportedOperationException("Data type conversions not implemented yet!!!!");
    }

/***********************************************************************
 * Low-level read function, returns complex data in a float array for the
 * given record.  Each complex value is stored in two consecutive float
 * elements in the array, so it must be twice as long as you would normally
 * expect, and <code>offset</code> must be multiplied by 2.  <code>length</code>
 * is still measured in <em>pixels</em>, so there are 2*<code>length</code>
 * values filled in the array.  pixelStride also applies to <em>pixels</em>,
 * so there are always two consecutive floats for real/imaginary, and
 * pixelStride gives the offset between one real and the next (also between
 * one imaginary and the next).
 * <p>
 * TBD: is this the right way to handle complex??!!
 * <p>
 * Follow the See Also link for parameters
 * @see #readRecord(byte[], int, int, int, int, int, int)
 */
    public synchronized void readRecordComp(float[] data,
			int start, int length, int offset,
			int pixelStride, int n2, int n3) throws IOException
    {
	readRecordCompNS(data, start, length, offset, pixelStride, n2, n3);
    }

/** Non-synchronized version */
    protected void readRecordCompNS(float[] data,
			int start, int length, int offset,
			int pixelStride, int n2, int n3) throws IOException
    {
	long pos = calcFilePos(start, n2, n3);
	seekToLocation(pos);
	if (length == 0)
	    length = _system.getN1() - start;
	if (_system.getFormatCode() == SystemLabel.TYPE_COMP) {
	    // Yeah this is inefficient... but complex is not well supported
	    // in any case.
	    for (int i=0; i<length; i++) {			// simple read
		_input_stream_wrap.readFloats(data, offset+i*pixelStride*2,
							1,1);		// real
		_input_stream_wrap.readFloats(data, offset+i*pixelStride*2+1,
							1,1);		// imag
	    }
	    _current_file_pos += length * _system.getPixelSize();
	}
	else 		//!!!! OTHER CASES: MAYBE DONE IN WRAPPER????!!!!
	    throw new UnsupportedOperationException("Data type conversions not implemented yet!!!!");
    }

////////////////////////////////////////////////////////////////////////

/***********************************************************************
 * Provides access to the binary header, which is NLB records at the beginning
 * of the file.
 * Follow the See Also link for complete description
 * @see VicarInput#getBinaryHeader()
 */
    public synchronized VicarBinaryLabel getBinaryHeader() throws IOException
    {
	if (_system.getNLB() == 0)	// no header!
	    return null;

	long pos = _lblsize_front;	// header starts right after labels
	seekToLocation(pos);

	if (_binary_data_format == null)
	    _binary_data_format = new VicarDataFormat(_system.getBHost(),
				_system.getBIntFmt(), _system.getBRealFmt());

	int size = _system.getNLB() * _system.getRecsize();

	if (debug) {
		System.out.println("VicarInputFile.getBinaryHeader() size="+size+", nlb="+_system.getNLB()+" recsize="+_system.getRecsize());
	}
	VicarBinaryLabel vbl = new VicarBinaryLabel(size, _binary_data_format);

	// Read the entire header

	stream_read(vbl.getBuffer());
	_current_file_pos += size;

	return vbl;
    }

/***********************************************************************
 * Provides access to the binary prefix, which is NBB bytes at the beginning
 * of each record.
 * Follow the See Also link for complete description
 * @see VicarInput#getBinaryPrefix(int,int) 
 */
    public synchronized VicarBinaryLabel getBinaryPrefix(int n2, int n3)
							throws IOException
    {
	if (_system.getNBB() == 0)	// no header!
	    return null;

	long pos = calcFilePos(0, n2, n3);
	pos -= _system.getNBB();	// calcFilePos skips past prefix
	seekToLocation(pos);

	if (_binary_data_format == null)
	    _binary_data_format = new VicarDataFormat(_system.getBHost(),
				_system.getBIntFmt(), _system.getBRealFmt());

	int size = _system.getNBB();

	VicarBinaryLabel vbl = new VicarBinaryLabel(size, _binary_data_format);

	// System.out.println("getLinePrefix pos="+pos+" size="+size+"  n2="+n2+" n3="+n3);
	// Read the entire header
	stream_read(vbl.getBuffer());
	
	_current_file_pos += size;

	return vbl;
    }


    /********************************************
     * returns the vicar recsize value fron the system label
     * This would be useful in creating a PDS  detached label
     * @return
     */
    public int getRecord_size() {
    	
    	return _record_size;
    }
    
    /**
     * returns the number of bytes of the vicar front label
     * This would be useful in creating a PDS  detached label
     * to set skip records
     * @return
     */
    public int getLblsize_front() {
    	return _lblsize_front;
    }
    
    public int getFileRecordCount() {
    	
    	return (int) (_lblsize_front + _image_size_bytes +_lblsize_eol)
			/ _record_size;
    }
    
	/***********************************************************************
	 * Provides access to the binary prefix, which is NBB bytes at the beginning
	 * of each record.
	 * Follow the See Also link for complete description
	 * @see VicarInput#getBinaryPrefix(int,int) 
	 */
		public synchronized byte[] getBinaryPrefix(int n2, int n3, byte buf[])
								throws IOException
		{
		if (_system.getNBB() == 0)	{ // no header!
			/// zero out the buffer
			for (int i=0 ; i < buf.length ; i++) {
				buf[i] = 0;
			}
			return buf;
		}

		long pos = calcFilePos(0, n2, n3);
		pos -= _system.getNBB();	// calcFilePos skips past prefix
		seekToLocation(pos);

		int size = _system.getNBB();
	
		// System.out.println("getLinePrefix pos="+pos+" size="+size+"  n2="+n2+" n3="+n3);
		
		stream_read(buf); // it knows how big the buffer is !!!!
	
		_current_file_pos += size;

		return buf;
		}
////////////////////////////////////////////////////////////////////////

/***********************************************************************
 * Creates a SampleModel that is most compatible with the input image.
 * Follow the See Also link for complete description
 * @see VicarIOBase#createSampleModel()
 */
    public SampleModel createSampleModel()
    {
	int width, height;

	width = VICAR_TILE_WIDTH;
	if (!isRandomAccessEasy())	// non-random files use entire width
	    width = _system.getNS();
	if (width > _system.getNS())
	    width = _system.getNS();
	height = VICAR_TILE_HEIGHT;
	if (height > _system.getNL())
	    height = _system.getNL();

	return createSampleModel(width, height);
    }

/***********************************************************************
 * Creates a SampleModel that is most compatible with the input image.
 * Follow the See Also link for complete description
 * @see VicarIOBase#createSampleModel(int,int)
 */
    public SampleModel createSampleModel(int tileWidth, int tileHeight)
    {
	int i;
	int pixel_stride;
	int scanline_stride;
	int num_bands;
	boolean float_type;
	int data_buffer_type = DataBuffer.TYPE_BYTE;

	num_bands = _system.getNB();
	
	_sourceBandsLen = 0;
	if (this._sourceBands != null) {
		_sourceBandsLen = this._sourceBands.length ;
		num_bands = _sourceBandsLen;
	}
	
	if (debug) {
		System.out.println("VicarInputFile.createSampleModel() num_bands = "+num_bands+"  _sourceBandsLen = "+ _sourceBandsLen);
	}
	
	
	if (_system.getFormatCode() == SystemLabel.TYPE_COMP)
	    num_bands *= 2;

	if (tileWidth == 0)
	    tileWidth = _system.getNS();
	if (tileHeight == 0)
	    tileHeight = _system.getNL();

	int[] band_offsets = new int[num_bands];
	int[] bank_indices = new int[num_bands];

	float_type = false;

	switch (_system.getFormatCode()) {
	    case SystemLabel.TYPE_BYTE:
		data_buffer_type = DataBuffer.TYPE_BYTE;
		break;
	    case SystemLabel.TYPE_HALF:
		data_buffer_type = DataBuffer.TYPE_SHORT;
		// data_buffer_type = DataBuffer.TYPE_USHORT;
		break;
	    case SystemLabel.TYPE_USHORT:
		data_buffer_type = DataBuffer.TYPE_USHORT;
		break;
	    case SystemLabel.TYPE_FULL:
		data_buffer_type = DataBuffer.TYPE_INT;
		break;
	    case SystemLabel.TYPE_REAL:
		data_buffer_type = DataBuffer.TYPE_FLOAT;
		float_type = true;
		break;
	    case SystemLabel.TYPE_DOUB:
		data_buffer_type = DataBuffer.TYPE_DOUBLE;
		float_type = true;
		break;
	    case SystemLabel.TYPE_COMP:
		data_buffer_type = DataBuffer.TYPE_FLOAT;
		float_type = true;
		break;
	}

	// float_type is used only to select ComponentSampleModelJAI.  JDK 1.3
	// requires this for float/double types.  But 1.4 supports float/double
	// using the standard sample models.  So if we're not in 1.3, simply
	// reset the flag and use the standard sample models.

	// Except that PixelAccessor in JAI 1.1.1 doesn't work with 1.4's
	// DataBufferFloat/Double... only JAI's.  1.1.2 is better.

	if (!jdk_version_1_3 && !jai_version_1_1_1)
	    float_type = false;

	// Compute offsets, etc... then create the SM
	
	
	switch (_system.getOrgCode()) {
	    case SystemLabel.ORG_BSQ:
		// One bank per band of data
		pixel_stride = 1;
		scanline_stride = tileWidth;
		if (_sourceBandsLen != 0 && _sourceBandsLen <= num_bands ) {
			for (i=0; i < _sourceBandsLen; i++) {
				band_offsets[i] = 0;
				bank_indices[i] = _sourceBands[i];
			}			
		} else {		
			for (i=0; i < num_bands; i++) {
				band_offsets[i] = 0;
				bank_indices[i] = i;
			}
		}
		
		
		if (float_type)
		    return new ComponentSampleModelJAI(data_buffer_type,
				tileWidth, tileHeight,
				pixel_stride, scanline_stride,
				bank_indices, band_offsets);

		/**/
		if (num_bands == 1)
		    return new PixelInterleavedSampleModel(data_buffer_type,
				tileWidth, tileHeight,
				pixel_stride, scanline_stride,
				band_offsets);
		else
		/***/
		    return new BandedSampleModel(data_buffer_type,
				tileWidth, tileHeight,
				scanline_stride,
				bank_indices, band_offsets);

	    case SystemLabel.ORG_BIL:
		// One bank for all bands of data
		pixel_stride = 1;
		scanline_stride = tileWidth * num_bands;
		// modify for _sourceBands
		for (i=0; i < num_bands; i++) {
		    band_offsets[i] = tileWidth * i;
		    bank_indices[i] = 0;
		}
		if (float_type)
		    return new ComponentSampleModelJAI(data_buffer_type,
				tileWidth, tileHeight,
				pixel_stride, scanline_stride,
				bank_indices, band_offsets);

		return new ComponentSampleModel(data_buffer_type,
				tileWidth, tileHeight,
				pixel_stride, scanline_stride,
				bank_indices, band_offsets);

	    case SystemLabel.ORG_BIP:
		// One bank for all bands of data
	    // modify for _sourceBands
		pixel_stride = num_bands;
		scanline_stride = tileWidth * num_bands;
		for (i=0; i < num_bands; i++) {
		    band_offsets[i] = i;
		    bank_indices[i] = 0;
		}
		if (float_type)
		    return new ComponentSampleModelJAI(data_buffer_type,
				tileWidth, tileHeight,
				pixel_stride, scanline_stride,
				bank_indices, band_offsets);

		return new PixelInterleavedSampleModel(data_buffer_type,
				tileWidth, tileHeight,
				pixel_stride, scanline_stride,
				band_offsets);
	}

	return null;		// whoops!!  Shouldn't happen!
    }

/***********************************************************************
 * High-level read function.
 * Follow the See Also link for complete description
 * @see VicarInput#readTile(int,int,SampleModel,DataBuffer)
 */
    public void readTile(int x, int y, SampleModel m, DataBuffer b)
			throws IOException
    {
	readTile(x, y, m.getWidth(), m.getHeight(), 0, 0, null, m, b);
    }

/***********************************************************************
 * High-level read function.
 * Follow the See Also link for complete description
 * @see VicarInput#readTile(int,int,int,int,int,int,SampleModel,DataBuffer)
 */
    public void readTile(int x, int y, int w, int h, int x_off, int y_off,
			SampleModel m, DataBuffer b)
			throws IOException
    {
	readTile(x, y, w, h, x_off, y_off, null, m, b);
    }

/***********************************************************************
 * High-level read function.
 * Follow the See Also link for complete description
 * @see VicarInput#readTile(int,int,int,int,int,int,int[],SampleModel,DataBuffer)
 */
    public synchronized void readTile(int x, int y, int w, int h,
			int x_off, int y_off,
			int bandList[],
			SampleModel sm, DataBuffer db)
			throws IOException
    {
	int i;
	int buffer_needed;
	int line, samp;
	int band, band_ind, srcBand, destBand;
	int offset, us_offset;
	boolean bandListGiven;
	// Array pointers that are filled in from UnpackedImageData.getXxxData()
	byte bdata[], bbdata[][];
	short sdata[], ssdata[][];
	int idata[], iidata[][];
	float fdata[], ffdata[][];
	double ddata[], dddata[][];
	
	byte us_bdata[], us_bbdata[][];
	short us_sdata[], us_ssdata[][];
	int us_idata[], us_iidata[][];
	float us_fdata[], us_ffdata[][];
	double us_ddata[], us_dddata[][];
	
	// check if _sourceBands is null or not. try just using that if it is set
	// make sure size is <= bands infile
	// does 
	
	
	
	// sampleModel was already adjusted to only use _sourceBands
	int num_bands = sm.getNumBands();
	if (bandList != null && num_bands > bandList.length)
	    num_bands = bandList.length;

	// dest bands are in order. data will be read from the band specified in _sourceBands and placed 
	// into the _destBand
	_destBands = new int[num_bands];
	for (i=0; i<num_bands; i++) {
		_destBands[i] = i;
	}
	bandListGiven = (bandList != null);	// flag if it was given
	
	if (!bandListGiven) {			
		if (_sourceBandsLen != 0 && _sourceBandsLen <= num_bands ) {
			num_bands = _sourceBandsLen;
			bandList = new int[_sourceBandsLen];
			for (i=0; i < _sourceBandsLen; i++) {
				bandList[i]  = _sourceBands[i];
			}	
		} else {	
			// make one for convenience
			bandList = new int[num_bands];
			for (i=0; i<num_bands; i++) {
				bandList[i] = i;
			}
		}
	}


	// tile size was adjusted in computeTiles. it is the desired output size
	// we need to determine the size of the tile to read to subsample to the output size
	
	int out_h = h ;
	int out_w = w ;
	
	int out_x = x ;
	int out_y = y ;
	
	int in_h = h * _sourceYSubsample;	
	int in_w = w * _sourceXSubsample;
	
	// special case subsample and crop
	// maybe only if sourceRegion.x != 0
	// _sourceRegion.x or _sourceRegion.y 
	// other wise we jump too far into the image
	
	int in_x = x * _sourceXSubsample;
	
	int in_x_crop = in_x;
	if (_sourceXSubsample == 1) {
		in_x_crop = in_x;
	} else {
		in_x_crop = (x * _sourceXSubsample) - _sourceRegion.x;
	}
	// int in_x = x ;
	// int in_y = y ;
	int in_y = y * _sourceYSubsample;
	int in_y_crop = in_y;
	if (_sourceYSubsample == 1) {
		in_y_crop = in_y;
	} else {
		in_y_crop = (y * _sourceYSubsample) - _sourceRegion.y ;
	}
	
	in_x = in_x_crop;
	in_y = in_y_crop;
	
	
	// is this always 0 ?? seems to be
	int in_x_off = x_off * _sourceXSubsample;
	int in_y_off = y_off * _sourceYSubsample;
		
	// is there or could there be a ss_x_off and ss_y_off which are different than x_off andf y_off??
	// looks like we always get here thru readTile(int x, int y, SampleModel m, DataBuffer b) 
	// which always sets x_off and y_off to 0
	
	// System.out.println("VicarInputFile.readTile "+x+","+y+" "+w+"x"+h+"   out_h "+out_h+"  in_h "+in_h+"  sourceYSubsampling = "+sourceYSubsampling+" ");
	// Throwable t = new Throwable("Check where we were called from");
	// t.printStackTrace();
	
	if (debug_tile) {
		System.out.println("VicarInputFile.readTile  --- "+x+","+y+" ---------------------------------------------------");
		System.out.println(" _sourceBands =" +_sourceBands+"   _sourceBandsLen =" +_sourceBandsLen+"  sampleModel num_bands = "+num_bands );
		if (_sourceBands != null) {
			System.out.println(" _sourceBands.length = "+_sourceBands.length );
			for (int j=0 ; j< _sourceBands.length ; j++) {
				System.out.println(" _sourceBands["+j+"] = "+_sourceBands[j] );
			}				
		}
		for (i=0; i<num_bands; i++) {
			System.out.println(" bandList["+i+"] = "+bandList[i] );
			System.out.println(" _destBands["+i+"] = "+_destBands[i] );
		}
		// Throwable t = new Throwable("Check where we were called from");
		// t.printStackTrace();
		System.out.println("VicarInputFile.readTile  ---- "+x+","+y+" "+w+"x"+h+"  x_off="+x_off+"  y_off="+y_off);
		
		System.out.println("VicarInputFile.readTile   in_ "+in_x+","+in_y+" "+in_w+"x"+in_h+"  x_off="+x_off+"  y_off="+y_off);
		System.out.println("VicarInputFile.readTile in_cr "+in_x_crop+","+in_y_crop+" "+in_w+"x"+in_h+"  x_off="+x_off+"  y_off="+y_off);
		System.out.println("VicarInputFile.readTile  out_ "+out_x+","+out_y+" "+out_w+"x"+out_h+"  x_off="+x_off+"  y_off="+y_off);
		System.out.println(" _sourceXSubsample = "+_sourceXSubsample+"  _sourceYSubsample = "+_sourceYSubsample+" ");
		System.out.println("     _system.getNS() "+_system.getNS()+"    _system.getNL() "+_system.getNL()+" ");
		System.out.println(" _sourceRegion.width "+_sourceRegion.width+"  _sourceRegion.height "+_sourceRegion.height+" "+_sourceRegion.x+","+_sourceRegion.y);
		System.out.println(" _destinationRegion.width "+_destinationRegion.width+"  _destinationRegion.height "+_destinationRegion.height+" "+_destinationRegion.x+","+_destinationRegion.y);
		// System.out.println(" _system.getNS() "+_system.getNS()+"  _system.getNL() "+_system.getNL()+"  _system.getNB() "+_system.getNB());
		// System.out.println(" _system.getN1() "+_system.getN1()+"  _system.getN2() "+_system.getN2()+"  _system.getN3() "+_system.getN3());
		System.out.println("w= " + w + ", x_off=" + x_off + ", ("+(w+x_off)+") sm width=" + sm.getWidth());
		System.out.println("h= " + h + ", y_off=" + x_off + ", ("+(h+y_off)+") sm height=" + sm.getHeight());
		System.out.println(" num_bands="+num_bands+" bandList ");
		// print the readParams values
		// printImageReadParams()
		// add all subsample, crop values here
		for (i=0; i<num_bands; i++) {
			System.out.print("  "+i+" > " +bandList[i] );
		}
		System.out.println(" ");
		System.out.println("x="+x+" w= " + w + ", (x+w)="+(x+w)+" x_off=" + x_off + ", _destinationRegion.x="+_destinationRegion.x+", _destinationRegion.width="+_destinationRegion.width+"  DW");
		System.out.println("y="+y+" h= " + h + ", (y+h)="+(y+h)+" , y_off=" + y_off + ", _destinationRegion.y="+_destinationRegion.y+", _destinationRegion.height="+_destinationRegion.height+" DW");
		
		int wd = w;
		int maxXd = (_destinationRegion.width + _destinationRegion.x);
		if (x + w > _destinationRegion.width + _destinationRegion.x) {
			// last tile may be incomplete
			wd = (_destinationRegion.width + _destinationRegion.x ) - x;
		}
		System.out.println("x="+x+" w= " + w + "  wd="+wd+"   _destinationRegion.width + _destinationRegion.x = "+maxXd+" DW");
		
		
		System.out.println("x="+x+" w= " + w + ", (x+w)="+(x+w)+" x_off=" + x_off + ", _sourceRegion.x="+_sourceRegion.x+", _sourceRegion.width="+_sourceRegion.width+"  DW");
		System.out.println("y="+y+" h= " + h + ", (y+h)="+(y+h)+" , y_off=" + y_off + ", _sourceRegion.y="+_sourceRegion.y+", _sourceRegion.height="+_sourceRegion.height+" DW");
		int ws = w;
		int maxXs = (_sourceRegion.width + _sourceRegion.x);
		if (x + w > _sourceRegion.width + _sourceRegion.x) {
			// last tile may be incomplete
			ws = (_sourceRegion.width + _sourceRegion.x ) - x;
		}
		System.out.println("x="+x+" w= " + w + "  ws="+ws+"   _sourceRegion.width + _sourceRegion.x = "+maxXs+" DW ");
		
    }
	
	// last tile may be incomplete
	if (_sourceRegion != null) {
		if (x + w > _sourceRegion.width + _sourceRegion.x){		
			w = (_sourceRegion.width + _sourceRegion.x ) - x;
		}
		if (y + h > _sourceRegion.height + _sourceRegion.y){
			h = (_sourceRegion.height + _sourceRegion.y ) - y;
		}
	}
	
	
	
	// sourceRegion- this is data read before we subsample
	// does this also need to take 
	if (in_x + in_w > _system.getNS())		// last tile may be incomplete
		in_w = _system.getNS() - in_x;
	if (in_y + in_h > _system.getNL())
		in_h = _system.getNL() - in_y;
	
	if (debug_tile) {
		System.out.print(" XX in_x="+in_x+" in_y="+in_y+" in_w="+in_w+" in_h="+in_h+" in_x_off="+in_x_off+" in_y_off="+in_y_off);
		System.out.println(" _system.getNS()="+_system.getNS()+" getNL()="+_system.getNL()+" ");
		System.out.print(" XX    x="+x+"    y="+y+"    w="+w+"     h="+h+"  x_off="+x_off+" y_off="+y_off+"  ");
		System.out.println(" _destinationRegion.width="+_destinationRegion.width+" height="+_destinationRegion.height+"  DW");
		System.out.println("                                                        DW ");
		}

	

// Exception e = new Exception("VicarInputFile.readTile");
// e.printStackTrace();
	// this sm is the one for the output tile
	if (x_off + w > sm.getWidth())
	    throw new ArrayIndexOutOfBoundsException(
		"Illegal width in VICAR readTile: " + w + ", x_off=" + x_off +
			", width=" + sm.getWidth());
	if (y_off + h > sm.getHeight())
	    throw new ArrayIndexOutOfBoundsException(
		"Illegal height in VICAR readTile: " + h + ", y_off=" + y_off +
			", height=" + sm.getHeight());

	// is this where the output data is written?? make one for the subsampled data? new db ??
	// The input sm etc is what needs to be filled for the output tile
	// The data is first read from the file and then subsampled to tile
	PixelAccessor pa = new PixelAccessor(sm, null);
	// out_h instead of h
	// Rectangle area = new Rectangle(x_off, y_off, w, out_h);
	Rectangle area = new Rectangle(x_off, y_off, w, h);	
	UnpackedImageData data = pa.getPixels(
			Raster.createWritableRaster(sm,db,null),
			area, db.getDataType(), true);

	
	
	// this is the tile and associated data buffer for the full tile (unsampled)
	// it is always the same size ??
	
	SampleModel unsampledTileSampleModel = createSampleModel(in_w, in_h);
    Point unsampledTileOrg = new Point(0,0);
	WritableRaster  unsampledTile = Raster.createWritableRaster(unsampledTileSampleModel, unsampledTileOrg);     
    Rectangle fullTileRect = unsampledTile.getBounds();    
    DataBuffer db_unsampled = unsampledTile.getDataBuffer(); 
    /***
	* what is db ??? DataBuffer db passed in. It is the data buffer of the inputTile
	* we need a dataBuffer (db) which we will use to read the input data
	* we will need a tile and databuffer for the tile we read before it is subsampled
	* can't use db here, must create a buffer sized to hold the data read directly from the file
	* it will then be subsampled into the output buffer which is db **/
	PixelAccessor inPa = new PixelAccessor(unsampledTileSampleModel, null);
	Rectangle inputArea = new Rectangle(in_x_off, in_y_off, in_w, in_h);
	UnpackedImageData unsubsampledData = inPa.getPixels(
			Raster.createWritableRaster(unsampledTileSampleModel,db_unsampled,null),
			inputArea, db_unsampled.getDataType(), true);
	
	int data_type = _system.getFormatCode();
	int org_code = _system.getOrgCode();
	
	int bytesPer = bytesForDataType(data_type);
	int inBytes = bytesPer * w;
	int outBytes = bytesPer * out_w;
	boolean doSS = false; // are we subsampling the data??
	if (_sourceXSubsample> 1 || _sourceXSubsample> 1) doSS = true;
	
	if (debug_tile) {
		System.out.println(" _sourceXSubsample = "+_sourceXSubsample+" in_w "+in_w+"   out_w = "+out_w+"  w="+w+" ");
		System.out.println(" _sourceYSubsample = "+_sourceYSubsample+" in_h "+in_h+"   out_h = "+out_h+"  h="+h);
		System.out.print(" "+x+","+y+" "+w+"x"+h+"  in_h "+in_h+"   out_h="+out_h+" bytesPer "+bytesPer+" inBytes="+inBytes+" outBytes="+outBytes);
		// how big a buffer would I need to read one line?
		System.out.println("  x_off "+x_off+"   y_off "+y_off+" data_type "+ data_type+"  TYPE_BYTE "+SystemLabel.TYPE_BYTE);
		System.out.print("  TYPE_BYTE="+SystemLabel.TYPE_BYTE+" TYPE_HALF="+SystemLabel.TYPE_HALF +" TYPE_FULL="+SystemLabel.TYPE_FULL);
		System.out.println("  TYPE_REAL="+SystemLabel.TYPE_REAL+" TYPE_DOUB="+SystemLabel.TYPE_DOUB +" ");
		System.out.println(" area = "+area+"  db.getSize()="+db.getSize()+"  data "+data.toString()+" pa="+pa.toString());	
		
		System.out.println(" inputArea = "+inputArea+"  db_unsampled.getSize()="+db_unsampled.getSize()+"  db_unsampled "+db_unsampled.toString()+" inPa="+inPa.toString());			
	}
	
	
	if (doSS && debug) {
		System.out.println("VicarInputFile.readTile  should call readSubsampled() ########## "+_sourceXSubsample+" "+_sourceYSubsample+" ################");
	} else if (debug ){
		System.out.println("VicarInputFile.readTile  should NOT call readSubsampled() ########## "+_sourceXSubsample+" "+_sourceYSubsample+" ################");
	}
	

	switch (org_code) {

	    case SystemLabel.ORG_BSQ:
	    	

		for (band_ind=0; band_ind < num_bands; band_ind++) {
		    band = bandList[band_ind];
		    srcBand = bandList[band_ind]; // band data is read from
		    destBand = _destBands[band_ind]; // band data is written to

		    switch (data_type) {
			case SystemLabel.TYPE_BYTE:
				if (doSS) {
					// destBand is where the data is placed after it has been read
					// us_  unsampled values
					// us_bdata = unsubsampledData.getByteData(band);
					us_bdata = unsubsampledData.getByteData(destBand);
					// should this always be 0 ?? might be the offset specified with subsampling ??
					// maybe we won't support it. always use 0,0
					// us_offset = unsubsampledData.getOffset(band);
					us_offset = unsubsampledData.getOffset(destBand);
					
					// bdata = data.getByteData(band);
					// offset = data.getOffset(band);
					bdata = data.getByteData(destBand);
					offset = data.getOffset(destBand);
					// int data_offset = offset; 
					if (debug_tile) {
					System.out.println("TYPE_BYTE h="+h+" x="+x+" w="+w+" offset="+offset+" y="+y+" band="+band+" srcBand="+srcBand+" destBand="+destBand+" data.pixelStride="+data.pixelStride+" doSS="+doSS);
					System.out.println("   in_h="+in_h+" in_x="+in_x+" in_w="+in_w+" us_offset="+us_offset+" in_y="+in_y+" band="+band+" unsubsampledData.pixelStride="+unsubsampledData.pixelStride+" doSS="+doSS);
					System.out.println("   bdata.length = "+bdata.length +"  us_bdata.length = "+us_bdata.length +" h="+h+" "+(bdata.length/w)+"  in_h="+in_h+" "+(us_bdata.length/in_w)+"  ");
					}
					/* read the input file data at full resolution */
			    	for (line=0; line < in_h; line++) {			    	
			    		// readRecordNS(us_bdata, in_x, in_w, us_offset, unsubsampledData.pixelStride, line+in_y,band);
			    		readRecordNS(us_bdata, in_x, in_w, us_offset, unsubsampledData.pixelStride, line+in_y,srcBand);	
			    		us_offset += unsubsampledData.lineStride;
			    	}
			    	
			    	// copy the data to the output buffer, subsampling as we do it
			    	int us_bdata_i = 0; // index into the us_bdata array
			    	
			    	int bdata_i = offset; 
			    	boolean exit_loop = false;
			    	for (line=0; line < in_h && !exit_loop ; line += _sourceYSubsample ) {	
			    		us_bdata_i = line * in_w; // * unsubsampledData.lineStride
			    		// System.out.println("line="+line+" bdata_i="+bdata_i+" us_bdata_i="+us_bdata_i+"  "+in_w+"  "+in_h );  
			    		for (int in_i=0; in_i < in_w; in_i = in_i + _sourceXSubsample ) {
			    			int us_bdata_ii = us_bdata_i + in_i;
			    			if (us_bdata_ii >= us_bdata.length || bdata_i >= bdata.length) {
			    				System.out.println("line="+line+" bdata_i="+bdata_i+" us_bdata_i="+us_bdata_i+"  us_bdata_ii="+us_bdata_ii+" "+in_w+" in_i="+in_i+"  break");
			    				exit_loop = true;
			    				break;
			    			}
			    			bdata[bdata_i++] = us_bdata[us_bdata_ii];	
			    		}			    	
			    	}
			    	if (debug_tile) {
			    		System.out.println("line="+line+" bdata_i="+bdata_i+" us_bdata_i="+us_bdata_i+"  "+in_w+"  "+in_h+" XXB" );  
			    	}
			    	
			    } else {
			    	// bdata = data.getByteData(band);
					// offset = data.getOffset(band);
					bdata = data.getByteData(destBand);
					offset = data.getOffset(destBand);
					if (debug_tile) {
						System.out.println("TYPE_BYTE h="+h+" x="+x+" w="+w+" offset="+offset+" y="+y+" band="+band+" data.pixelStride="+data.pixelStride+" doSS="+doSS);
					}
			    
			    	for (line=0; line < h; line++) {			    	
				    	// readRecordNS(bdata, x, w, offset, data.pixelStride, line+y,band);		
				    	readRecordNS(bdata, x, w, offset, data.pixelStride, line+y,srcBand);		
				    	// always increment the offset even if the data isn't read
				    	offset += data.lineStride;
				    }
			    }
			    
			    
			    break;
			case SystemLabel.TYPE_HALF:
				if (doSS) {
					// destBand is where the data is placed after it has been read
					// us_  unsampled values
					us_sdata = unsubsampledData.getShortData(destBand);
					// should this always be 0 ?? might be the offset specified with subsampling ??
					// maybe we won't support it. always use 0,0
					us_offset = unsubsampledData.getOffset(destBand);
										
					sdata = data.getShortData(band);
					offset = data.getOffset(destBand);
					// int data_offset = offset; 
					if (debug_tile) {
					System.out.println("TYPE_HALF h="+h+" x="+x+" w="+w+" offset="+offset+" y="+y+" band="+band+" srcBand="+srcBand+" destBand="+destBand+" data.pixelStride="+data.pixelStride+" doSS="+doSS);
					System.out.println("   in_h="+in_h+" in_x="+in_x+" in_w="+in_w+" us_offset="+us_offset+" in_y="+in_y+" band="+band+" unsubsampledData.pixelStride="+unsubsampledData.pixelStride+" doSS="+doSS);
					System.out.println("   sdata.length = "+sdata.length +"  us_bdata.length = "+us_sdata.length +" h="+h+" "+(sdata.length/w)+"  in_h="+in_h+" "+(us_sdata.length/in_w)+"  ");
					}
					/* read the input file data at full resolution */
			    	for (line=0; line < in_h; line++) {			    	
			    		// readRecordNS(us_bdata, in_x, in_w, us_offset, unsubsampledData.pixelStride, line+in_y,band);
			    		readRecordNS(us_sdata, in_x, in_w, us_offset, unsubsampledData.pixelStride, line+in_y,srcBand);	
			    		us_offset += unsubsampledData.lineStride;
			    	}
			    	
			    	// copy the data to the output buffer, subsampling as we do it
			    	int us_sdata_i = 0; // index into the us_bdata array
			    	
			    	int sdata_i = offset; 
			    	boolean exit_loop = false;
			    	for (line=0; line < in_h && !exit_loop ; line += _sourceYSubsample ) {	
			    		us_sdata_i = line * in_w; // * unsubsampledData.lineStride
			    		// System.out.println("line="+line+" bdata_i="+bdata_i+" us_bdata_i="+us_bdata_i+"  "+in_w+"  "+in_h );  
			    		for (int in_i=0; in_i < in_w; in_i = in_i + _sourceXSubsample ) {
			    			int us_sdata_ii = us_sdata_i + in_i;
			    			if (us_sdata_ii >= us_sdata.length || sdata_i >= sdata.length) {
			    				System.out.println("line="+line+" sdata_i="+sdata_i+" us_sdata_i="+us_sdata_i+"  us_bdata_ii="+us_sdata_ii+" "+in_w+" in_i="+in_i+"  break");
			    				exit_loop = true;
			    				break;
			    			}
			    			sdata[sdata_i++] = us_sdata[us_sdata_ii];	
			    		}			    	
			    	}
			    	if (debug_tile) {
			    		System.out.println("line="+line+" sdata_i="+sdata_i+" us_bdata_i="+us_sdata_i+"  "+in_w+"  "+in_h+" XXH" );  
			    	}

				} else {
					sdata = data.getShortData(band);
					offset = data.getOffset(band);
					for (line=0; line < h; line++) {
						readRecordNS(sdata, x, w, offset, data.pixelStride, line+y,band);
						offset += data.lineStride;
			    	}
				}
			    break;
			case SystemLabel.TYPE_USHORT:
				if (doSS) {
					// destBand is where the data is placed after it has been read
					// us_  unsampled values
					us_sdata = unsubsampledData.getShortData(destBand);
					// should this always be 0 ?? might be the offset specified with subsampling ??
					// maybe we won't support it. always use 0,0
					us_offset = unsubsampledData.getOffset(destBand);
										
					sdata = data.getShortData(band);
					offset = data.getOffset(destBand);
					// int data_offset = offset; 
					if (debug_tile) {
					System.out.println("TYPE_USHORT h="+h+" x="+x+" w="+w+" offset="+offset+" y="+y+" band="+band+" srcBand="+srcBand+" destBand="+destBand+" data.pixelStride="+data.pixelStride+" doSS="+doSS);
					System.out.println("   in_h="+in_h+" in_x="+in_x+" in_w="+in_w+" us_offset="+us_offset+" in_y="+in_y+" band="+band+" unsubsampledData.pixelStride="+unsubsampledData.pixelStride+" doSS="+doSS);
					System.out.println("   sdata.length = "+sdata.length +"  us_bdata.length = "+us_sdata.length +" h="+h+" "+(sdata.length/w)+"  in_h="+in_h+" "+(us_sdata.length/in_w)+"  ");
					}
					/* read the input file data at full resolution */
			    	for (line=0; line < in_h; line++) {			    	
			    		// readRecordNS(us_bdata, in_x, in_w, us_offset, unsubsampledData.pixelStride, line+in_y,band);
			    		readRecordNS(us_sdata, in_x, in_w, us_offset, unsubsampledData.pixelStride, line+in_y,srcBand);	
			    		us_offset += unsubsampledData.lineStride;
			    	}
			    	
			    	// copy the data to the output buffer, subsampling as we do it
			    	int us_sdata_i = 0; // index into the us_bdata array
			    	
			    	int sdata_i = offset; 
			    	boolean exit_loop = false;
			    	for (line=0; line < in_h && !exit_loop ; line += _sourceYSubsample ) {	
			    		us_sdata_i = line * in_w; // * unsubsampledData.lineStride
			    		// System.out.println("line="+line+" bdata_i="+bdata_i+" us_bdata_i="+us_bdata_i+"  "+in_w+"  "+in_h );  
			    		for (int in_i=0; in_i < in_w; in_i = in_i + _sourceXSubsample ) {
			    			int us_sdata_ii = us_sdata_i + in_i;
			    			if (us_sdata_ii >= us_sdata.length || sdata_i >= sdata.length) {
			    				System.out.println("line="+line+" sdata_i="+sdata_i+" us_sdata_i="+us_sdata_i+"  us_bdata_ii="+us_sdata_ii+" "+in_w+" in_i="+in_i+"  break");
			    				exit_loop = true;
			    				break;
			    			}
			    			sdata[sdata_i++] = us_sdata[us_sdata_ii];	
			    		}			    	
			    	}
			    	if (debug_tile) {
			    		System.out.println("line="+line+" sdata_i="+sdata_i+" us_sdata_i="+us_sdata_i+"  "+in_w+"  "+in_h+" XXUS" );  
			    	}

				} else {
					sdata = data.getShortData(band);
					offset = data.getOffset(band);
					for (line=0; line < h; line++) {
						readRecordUshortNS(sdata, x, w, offset, data.pixelStride, line+y,band);
						offset += data.lineStride;
					}
				}
			    break;
			case SystemLabel.TYPE_FULL:
				if (doSS) {
					// destBand is where the data is placed after it has been read
					// us_  unsampled values
					us_idata = unsubsampledData.getIntData(destBand);
					// should this always be 0 ?? might be the offset specified with subsampling ??
					// maybe we won't support it. always use 0,0
					// us_offset = unsubsampledData.getOffset(band);
					us_offset = unsubsampledData.getOffset(destBand);
					
					// bdata = data.getByteData(band);
					// offset = data.getOffset(band);
					idata = data.getIntData(destBand);
					offset = data.getOffset(destBand);
					// int data_offset = offset; 
					if (debug_tile) {
					System.out.println("TYPE_FULL h="+h+" x="+x+" w="+w+" offset="+offset+" y="+y+" band="+band+" srcBand="+srcBand+" destBand="+destBand+" data.pixelStride="+data.pixelStride+" doSS="+doSS);
					System.out.println("   in_h="+in_h+" in_x="+in_x+" in_w="+in_w+" us_offset="+us_offset+" in_y="+in_y+" band="+band+" unsubsampledData.pixelStride="+unsubsampledData.pixelStride+" doSS="+doSS);
					System.out.println("   idata.length = "+idata.length +"  us_bdata.length = "+us_idata.length +" h="+h+" "+(idata.length/w)+"  in_h="+in_h+" "+(us_idata.length/in_w)+"  ");
					}
					/* read the input file data at full resolution */
			    	for (line=0; line < in_h; line++) {			    	
			    		// readRecordNS(us_bdata, in_x, in_w, us_offset, unsubsampledData.pixelStride, line+in_y,band);
			    		readRecordNS(us_idata, in_x, in_w, us_offset, unsubsampledData.pixelStride, line+in_y,srcBand);	
			    		us_offset += unsubsampledData.lineStride;
			    	}
			    	
			    	// copy the data to the output buffer, subsampling as we do it
			    	int us_idata_i = 0; // index into the us_idata array
			    	
			    	int idata_i = offset; 
			    	boolean exit_loop = false;
			    	for (line=0; line < in_h && !exit_loop ; line += _sourceYSubsample ) {	
			    		us_idata_i = line * in_w; // * unsubsampledData.lineStride
			    		// System.out.println("line="+line+" bdata_i="+bdata_i+" us_bdata_i="+us_bdata_i+"  "+in_w+"  "+in_h );  
			    		for (int in_i=0; in_i < in_w; in_i = in_i + _sourceXSubsample ) {
			    			int us_idata_ii = us_idata_i + in_i;
			    			if (us_idata_ii >= us_idata.length || idata_i >= idata.length) {
			    				System.out.println("line="+line+" idata_i="+idata_i+" us_idata_i="+us_idata_i+"  us_idata_ii="+us_idata_ii+" "+in_w+" in_i="+in_i+"  break");
			    				exit_loop = true;
			    				break;
			    			}
			    			idata[idata_i++] = us_idata[us_idata_ii];	
			    		}			    	
			    	}
			    	if (debug_tile) {
			    		System.out.println("line="+line+" idata_i="+idata_i+" us_idata_i="+us_idata_i+"  "+in_w+"  "+in_h+" XXI" );  
			    	}
			    	
			    } else {
			    	idata = data.getIntData(band);
			    	offset = data.getOffset(band);
			    	for (line=0; line < h; line++) {
			    		readRecordNS(idata, x, w, offset,data.pixelStride, line+y,band);
			    		offset += data.lineStride;
			    	}
			    }
			    break;
			case SystemLabel.TYPE_REAL:
				if (doSS) {
					// us_  unsampled values
					us_fdata = unsubsampledData.getFloatData(band);
					// do we need to take real byte count or does lineStride pixelStride handle that??
					// should this always be 0 ?? might be the offset specified with subsampling
					// maybe we won't support it. always use 0,0
					us_offset = unsubsampledData.getOffset(band);
					
					// output tile
					fdata = data.getFloatData(band);
					offset = data.getOffset(band);
					// int data_offset = offset; 
					if (debug_tile) {
					System.out.println("TYPE_REAL h="+h+" x="+x+" w="+w+" offset="+offset+" y="+y+" band="+band+" data.pixelStride="+data.pixelStride+" doSS="+doSS);
					System.out.println("   in_h="+in_h+" in_x="+in_x+" in_w="+in_w+" us_offset="+us_offset+" in_y="+in_y+" band="+band+" unsubsampledData.pixelStride="+unsubsampledData.pixelStride+" doSS="+doSS);
					System.out.println("   fdata.length = "+fdata.length +"  us_fdata.length = "+us_fdata.length +" h="+h+" "+(fdata.length/w)+"  in_h="+in_h+" "+(us_fdata.length/in_w)+"  ");
					}
					/* read the input file data at full resolution */
			    	for (line=0; line < in_h; line++) {			    	
			    		readRecordNS(us_fdata, in_x, in_w, us_offset, unsubsampledData.pixelStride, line+in_y,srcBand);			    	
			    		us_offset += unsubsampledData.lineStride;
			    	}
			    	
			    	// copy the data to the output buffer, subsampling as we do it
			    	int us_fdata_i = 0; // index into the us_bdata array
			    	
			    	int fdata_i = offset; 
			    	boolean exit_loop = false;
			    	for (line=0; line < in_h && !exit_loop ; line += _sourceYSubsample ) {	
			    		us_fdata_i = line * in_w; // * unsubsampledData.lineStride
			    		// System.out.println("line="+line+" bdata_i="+bdata_i+" us_bdata_i="+us_bdata_i+"  "+in_w+"  "+in_h );  
			    		for (int in_i=0; in_i < in_w; in_i = in_i + _sourceXSubsample ) {
			    			int us_fdata_ii = us_fdata_i + in_i;
			    			if (us_fdata_ii >= us_fdata.length || fdata_i >= fdata.length) {
			    				System.out.println("line="+line+" fdata_i="+fdata_i+" us_fdata_i="+us_fdata_i+"  us_fdata_ii="+us_fdata_ii+" "+in_w+" in_i="+in_i+"  break");
			    				exit_loop = true;
			    				break;
			    			}
			    			fdata[fdata_i++] = us_fdata[us_fdata_ii];	
			    		}			    	
			    	}
			    	if (debug_tile) {
			    		System.out.println("line="+line+" fdata_i="+fdata_i+" us_fdata_i="+us_fdata_i+"  "+in_w+"  "+in_h+" XXX" );  
			    	}
			    	
			    } else { // standard read
			    	fdata = data.getFloatData(band);
			    	offset = data.getOffset(band);
			    	for (line=0; line < h; line++) {
			    		readRecordNS(fdata, x, w, offset, data.pixelStride, line+y,band);
			    		offset += data.lineStride;
			    	}
			    }
			    break;
			case SystemLabel.TYPE_DOUB:
				if (doSS) { 
					// us_  unsampled values
					us_ddata = unsubsampledData.getDoubleData(band);
					// do we need to take real byte count or does lineStride pixelStride handle that??
					// should this always be 0 ?? might be the offset specified with subsampling
					// maybe we won't support it. always use 0,0
					us_offset = unsubsampledData.getOffset(band);
					
					// output tile
					ddata = data.getDoubleData(band);
					offset = data.getOffset(band);
					// int data_offset = offset; 
					if (debug_tile) {
					System.out.println("TYPE_DOUB h="+h+" x="+x+" w="+w+" offset="+offset+" y="+y+" band="+band+" data.pixelStride="+data.pixelStride+" doSS="+doSS);
					System.out.println("   in_h="+in_h+" in_x="+in_x+" in_w="+in_w+" us_offset="+us_offset+" in_y="+in_y+" band="+band+" unsubsampledData.pixelStride="+unsubsampledData.pixelStride+" doSS="+doSS);
					System.out.println("   ddata.length = "+ddata.length +"  us_ddata.length = "+us_ddata.length +" h="+h+" "+(ddata.length/w)+"  in_h="+in_h+" "+(us_ddata.length/in_w)+"  ");
					}
					/* read the input file data at full resolution */
			    	for (line=0; line < in_h; line++) {			    	
			    		readRecordNS(us_ddata, in_x, in_w, us_offset, unsubsampledData.pixelStride, line+in_y,srcBand);			    	
			    		us_offset += unsubsampledData.lineStride;
			    	}
			    	
			    	// copy the data to the output buffer, subsampling as we do it
			    	int us_ddata_i = 0; // index into the us_ddata array
			    	
			    	int ddata_i = offset; 
			    	boolean exit_loop = false;
			    	for (line=0; line < in_h && !exit_loop ; line += _sourceYSubsample ) {	
			    		us_ddata_i = line * in_w; // * unsubsampledData.lineStride
			    		// System.out.println("line="+line+" bdata_i="+bdata_i+" us_bdata_i="+us_bdata_i+"  "+in_w+"  "+in_h );  
			    		for (int in_i=0; in_i < in_w; in_i = in_i + _sourceXSubsample ) {
			    			int us_ddata_ii = us_ddata_i + in_i;
			    			if (us_ddata_ii >= us_ddata.length || ddata_i >= ddata.length) {
			    				System.out.println("line="+line+" ddata_i="+ddata_i+" us_ddata_i="+us_ddata_i+"  us_ddata_ii="+us_ddata_ii+" "+in_w+" in_i="+in_i+"  break");
			    				exit_loop = true;
			    				break;
			    			}
			    			ddata[ddata_i++] = us_ddata[us_ddata_ii];	
			    		}			    	
			    	}
			    	if (debug_tile) {
			    		System.out.println("line="+line+" ddata_i="+ddata_i+" us_ddata_i="+us_ddata_i+"  "+in_w+"  "+in_h+" XXD" );  
			    	}
			    	
			    } else { // standard read
			    	ddata = data.getDoubleData(band);
			    	offset = data.getOffset(band);
			    	for (line=0; line < h; line++) {
			    		readRecordNS(ddata, x, w, offset, data.pixelStride, line+y,band);
			    		offset += data.lineStride;
			    	}
			    }
			    break;
			case SystemLabel.TYPE_COMP:
			    throw new UnsupportedOperationException("readTile() for Complex data not implemented yet!");
/*!!!!
			    // "band" is file_nb*2, where evens are real and
			    // odds are imaginary.  "band_ind" is a simple
			    // index into the output band array.
			    for (line=0; line < h; line++) {
				readRecordCompNS(_float_buffer, x, w, 0, line+y,
						band/2);
				int c = band % 2;	// 0=real, 1=imag
				for (i=0; i < w; i++) {
				    sm.setSample(x_off+i, y_off+line, band_ind,
					_float_buffer[i+c], db);
				}
			    }
			    break;
!!!!*/
		    }
		}		// end band loop
		break;

	    case SystemLabel.ORG_BIL:

		for (line=0; line < h; line++) {

		    switch (data_type) {
			case SystemLabel.TYPE_BYTE:
			    for (band_ind=0; band_ind < num_bands; band_ind++) {
				band = bandList[band_ind];
				bdata = data.getByteData(band);
				offset = data.getOffset(band) +
							line * data.lineStride;
				readRecordNS(bdata, x, w, offset,
					data.pixelStride, band, line+y);
			    }
			    break;
			case SystemLabel.TYPE_HALF:
			    for (band_ind=0; band_ind < num_bands; band_ind++) {
				band = bandList[band_ind];
				sdata = data.getShortData(band);
				offset = data.getOffset(band) +
							line * data.lineStride;
				readRecordNS(sdata, x, w, offset,
					data.pixelStride, band, line+y);
			    }
			    break;
			case SystemLabel.TYPE_USHORT:
			    for (band_ind=0; band_ind < num_bands; band_ind++) {
				band = bandList[band_ind];
				sdata = data.getShortData(band);
				offset = data.getOffset(band) +
							line * data.lineStride;
				readRecordUshortNS(sdata, x, w, offset,
					data.pixelStride, band, line+y);
			    }
			    break;
			case SystemLabel.TYPE_FULL:
			    for (band_ind=0; band_ind < num_bands; band_ind++) {
				band = bandList[band_ind];
				idata = data.getIntData(band);
				offset = data.getOffset(band) +
							line * data.lineStride;
				readRecordNS(idata, x, w, offset,
					data.pixelStride, band, line+y);
			    }
			    break;
			case SystemLabel.TYPE_REAL:
			    for (band_ind=0; band_ind < num_bands; band_ind++) {
				band = bandList[band_ind];
				fdata = data.getFloatData(band);
				offset = data.getOffset(band) +
							line * data.lineStride;
				readRecordNS(fdata, x, w, offset,
					data.pixelStride, band, line+y);
			    }
			    break;
			case SystemLabel.TYPE_DOUB:
			    for (band_ind=0; band_ind < num_bands; band_ind++) {
				band = bandList[band_ind];
				ddata = data.getDoubleData(band);
				offset = data.getOffset(band) +
							line * data.lineStride;
				readRecordNS(ddata, x, w, offset,
					data.pixelStride, band, line+y);
			    }
			    break;
			case SystemLabel.TYPE_COMP:
			    throw new UnsupportedOperationException("readTile() for Complex data not implemented yet!");
/*!!!!
			    // "band" is file_nb*2, where evens are real and
			    // odds are imaginary.  "band_ind" is a simple
			    // index into the output band array.
			    for (band_ind=0; band_ind < num_bands; band_ind++) {
				band = bandList[band_ind];
				readRecordCompNS(_float_buffer, x, w, 0,
						band/2, line+y);
				int c = band % 2;	// 0=real, 1=imag
				for (i=0; i < w; i++) {
				    sm.setSample(x_off+i, y_off+line, band_ind,
					_float_buffer[i+c], db);
				}
			    }
			    break;
!!!!*/
		    }
		}		// end line loop
		break;

	    case SystemLabel.ORG_BIP:

		// BIP is a problem to read because it is quite likely that
		// each band in the SM is in a different bank (array) of data,
		// thus making array-based reads impractical.  That would be
		// okay(ish) if we wanted to skip around in the file, but we
		// want to read data in the same order as in the file (to
		// support sequential-only streams).
		// So, we read the entire pixel into a temporary buffer and
		// then transfer that to the data banks.  Somewhat inefficient,
		// but BIP files are fairly rare.
		// TBD!!!!: one special case that could be improved is if we
		// have a PixelInterleavedSampleModel, and we're reading all
		// bands.  Then it's a BIP -> BIP transfer.  However, this
		// would not be trivial code and given the rarity of BIP
		// files, does not seem necessary at this time.

		// Is band_offsets[] needed?  Not sure HotSpot is good enough
		// to optimize data.getOffset(band_ind) inside the pixel loop...
		int band_offsets[] = new int[num_bands];
		for (band_ind=0; band_ind<num_bands; band_ind++) {
		    band_offsets[band_ind] = data.getOffset(band_ind);
		}

		// get starting band and # of bands to read from file
		// Note that band_ind indexes into the SM whild band (or
		// bandList[band_ind]) indexes into the file.

		int sb = 0;
		int nb = num_bands;
		if (bandListGiven) {		// get min/max of bandList
		    int min = bandList[0];
		    int max = bandList[0];
		    for (i=1; i<num_bands; i++) {
			if (bandList[i] < min)
			    min = bandList[i];
			if (bandList[i] > max)
			    max = bandList[i];
		    }
		    sb = min;
		    nb = max - min + 1;
		}

		switch (data_type) {
		    case SystemLabel.TYPE_BYTE:
			bbdata = data.getByteData();
			byte bbuf[] = new byte[nb];
			for (line=0; line < h; line++) {
			    for (samp=0; samp < w; samp++) {
				readRecordNS(bbuf, sb, nb, 0, 1, samp+x,line+y);
				for (band_ind=0; band_ind < num_bands;
								band_ind++) {
				    band = bandList[band_ind];
				    bbdata[band_ind][band_offsets[band_ind] +
						     samp * data.pixelStride +
						     line * data.lineStride]
					= bbuf[band-sb];
				}
			    }
			}
			break;
		    case SystemLabel.TYPE_HALF:
			ssdata = data.getShortData();
			short sbuf[] = new short[nb];
			for (line=0; line < h; line++) {
			    for (samp=0; samp < w; samp++) {
				readRecordNS(sbuf, sb, nb, 0, 1, samp+x,line+y);
				for (band_ind=0; band_ind < num_bands;
								band_ind++) {
				    band = bandList[band_ind];
				    ssdata[band_ind][band_offsets[band_ind] +
						     samp * data.pixelStride +
						     line * data.lineStride]
					= sbuf[band-sb];
				}
			    }
			}
			break;
		    case SystemLabel.TYPE_USHORT:
			ssdata = data.getShortData();
			short ubuf[] = new short[nb];
			for (line=0; line < h; line++) {
			    for (samp=0; samp < w; samp++) {
				readRecordUshortNS(ubuf, sb, nb, 0, 1,
								samp+x,line+y);
				for (band_ind=0; band_ind < num_bands;
								band_ind++) {
				    band = bandList[band_ind];
				    ssdata[band_ind][band_offsets[band_ind] +
						     samp * data.pixelStride +
						     line * data.lineStride]
					= ubuf[band-sb];
				}
			    }
			}
			break;
		    case SystemLabel.TYPE_FULL:
			iidata = data.getIntData();
			int ibuf[] = new int[nb];
			for (line=0; line < h; line++) {
			    for (samp=0; samp < w; samp++) {
				readRecordNS(ibuf, sb, nb, 0, 1, samp+x,line+y);
				for (band_ind=0; band_ind < num_bands;
								band_ind++) {
				    band = bandList[band_ind];
				    iidata[band_ind][band_offsets[band_ind] +
						     samp * data.pixelStride +
						     line * data.lineStride]
					= ibuf[band-sb];
				}
			    }
			}
			break;
		    case SystemLabel.TYPE_REAL:
			ffdata = data.getFloatData();
			float fbuf[] = new float[nb];
			for (line=0; line < h; line++) {
			    for (samp=0; samp < w; samp++) {
				readRecordNS(fbuf, sb, nb, 0, 1, samp+x,line+y);
				for (band_ind=0; band_ind < num_bands;
								band_ind++) {
				    band = bandList[band_ind];
				    ffdata[band_ind][band_offsets[band_ind] +
						     samp * data.pixelStride +
						     line * data.lineStride]
					= fbuf[band-sb];
				}
			    }
			}
			break;
		    case SystemLabel.TYPE_DOUB:
			dddata = data.getDoubleData();
			double dbuf[] = new double[nb];
			for (line=0; line < h; line++) {
			    for (samp=0; samp < w; samp++) {
				readRecordNS(dbuf, sb, nb, 0, 1, samp+x,line+y);
				for (band_ind=0; band_ind < num_bands;
								band_ind++) {
				    band = bandList[band_ind];
				    dddata[band_ind][band_offsets[band_ind] +
						     samp * data.pixelStride +
						     line * data.lineStride]
					= dbuf[band-sb];
				}
			    }
			}
			break;

		    case SystemLabel.TYPE_COMP:
			throw new UnsupportedOperationException("readTile() for Complex data not implemented yet!");

/*!!!!
			    // "band" is file_nb*2, where evens are real and
			    // odds are imaginary.  "band_ind" is a simple
			    // index into the output band array.
			    float fb[] = new float[2];
			    for (samp=0; samp < w; samp++) {
				for (band_ind=0; band_ind < num_bands; band_ind++) {
				    if (bandList == null)
					band = band_ind;
				    else
					band = bandList[band_ind];

				    readRecordCompNS(fb, band/2, 1, 0,
								samp+x, line+y);
				    _float_buffer[band_ind] = fb[band%2];
				}
				sm.setPixel(x_off+samp, y_off+line,
							_float_buffer, db);
			    }
			    break;
!!!!*/
		}		// end switch data_type
		break;		// end BIP case
	}			// end switch org_code
	pa.setPixels(data, false);

    }

////////////////////////////////////////////////////////////////////////

/***********************************************************************
 * Internal routine for calculating the position to seek to given the
 * record address (n2/n3) and the offset within the record.  The binary
 * headers and prefixes are taken into account, i.e. position 0 is the first
 * pixel of the record (past the prefix), and line/band 0 is the first
 * line/band of pixel data (past the headers).
 * @throws IOException if start, n2 or n3 exceed the bounds of the image
 */
    protected long calcFilePos(int start, int n2, int n3) throws IOException
    {
    	if (debug) 
    	{
    	/**
    	// System.out.print("========================================");
    	
    	System.out.print("VicarInputFile.calcFilePos: "+_system.getOrg());
		System.out.print(" nBB="+_system.getNBB() );
        System.out.print(" start="+start+" N1="+_system.getN1() );
        System.out.print(" n2="+n2+" N2="+_system.getN2() );
        System.out.print(" n3="+n3+" N3="+_system.getN3() );
        System.out.println(" pixelSize="+_system.getPixelSize() );
        **/
    }
	if (start < 0 || start >= _system.getN1())
	    throw new IOException(
		"Attempt to read past edge of image for dimension 1: N1=" +
		_system.getN1() + ", read position=" + start);
	if (n2 < 0 || n2 >= _system.getN2())
	    throw new IOException(
		"Attempt to read past edge of image for dimension 2: N2=" +
		_system.getN2() + ", read position=" + n2);
	if (n3 < 0 || n3 >= _system.getN3())
	    throw new IOException(
		"Attempt to read past edge of image for dimension 3: N3=" +
		_system.getN3() + ", read position=" + n3);

	return _lblsize_front +
		(_system.getNLB() + (long)n3 * _system.getN2() + (long)n2)
							* _system.getRecsize() +
		(long)start * _system.getPixelSize() +
		_system.getNBB();
    }

/***********************************************************************
 * Internal routine to seek to a given spot in the file.  This is trivial
 * for random-access files, but a bit tricky for sequential ones.
 * <p>
 * The file offset is added to the position before seeking.  So the provided
 * pos is the logical position within the VICAR image, not the absolute
 * position within the file.
 */
    protected void seekToLocation(long pos) throws IOException
    {
	// Adjust position based on the file offset

	pos += _file_offset;
	if (debug) {
		// System.out.println("VicarInputFile.seekToLocation pos="+pos+"   _file_offset="+_file_offset);
	}

	// If random, just seek there.  Don't pay attention to _current_file_pos
	// because someone might have changed the underlying file.

	if (_random_allowed) {
	    stream_seek(pos);
	    _current_file_pos = pos;
	    return;
	}

	// Sequential file, we must do some checking

	if (pos < _current_file_pos)
	    throw new NonSequentialAccessException("Non-sequential access attempted to file, desired pos=" + pos + ", current pos=" + _current_file_pos);

	// If not random, we might need to skip some data

	if (!_random_allowed && pos > _current_file_pos) {
	    if (pos - _current_file_pos > Integer.MAX_VALUE)
		throw new IOException("Attempt to skip too many bytes ("+ (pos-_current_file_pos) + ") in sequential-only VICAR file");
	    stream_skipBytes((int)(pos - _current_file_pos));
	    _current_file_pos = pos;
	}
    }

/***********************************************************************
 * Low-level routine to seek on <code>_input_stream</code>.  This function,
 * as well as all the other <code>stream_</code>* functions, is required
 * to handle the plethora of possible input stream objects mandated by the
 * pathetically poor design of <code>java.io</code>.
 * <p>
 * The file offset is not added here; it is assumed that the caller takes
 * care of that already.
 * @throws UnsupportedOperationException if the stream's type is not recognized.
 */
    protected void stream_seek(long pos) throws IOException
    {
	if (_input_stream instanceof SeekableStream) {
	    ((SeekableStream)_input_stream).seek(pos);
	    return;
	}

	if (_input_stream instanceof ImageInputStream) {
	    ((ImageInputStream)_input_stream).seek(pos);
	    return;
	}

	throw new UnsupportedOperationException("Seek not supported for this stream type");
    }

/***********************************************************************
 * Low-level routine to skipBytes on <code>_input_stream</code>.
 * @see #stream_seek
 * @throws UnsupportedOperationException if the stream's type is not recognized.
 */
    protected int stream_skipBytes(int n) throws IOException
    {
	if (_input_stream instanceof SeekableStream) {
	    return ((SeekableStream)_input_stream).skipBytes(n);
	}

	if (_input_stream instanceof ImageInputStream) {
	    return ((ImageInputStream)_input_stream).skipBytes(n);
	}

	if (_input_stream instanceof InputStream) {
	    return (int)((InputStream)_input_stream).skip((long)n);
	}

	throw new UnsupportedOperationException("SkipBytes not supported for this stream type");
    }

	/***********************************************************************
	 * Low-level routine to fully read an array of bytes from
	 * <code>_input_stream</code>.
	 * @see #stream_seek
	 * @throws UnsupportedOperationException if the stream's type is not recognized.
	 */
		protected void stream_read(byte[] b) throws IOException
		{
			if (debug) {
				System.out.println("VicarInputFile.stream_read() b.length = " + b.length);
				}
		if (_input_stream instanceof SeekableStream) {
			if (debug) {
				System.out.println("VicarInputFile.stream_read() SeekableStream");
				}
			((SeekableStream)_input_stream).readFully(b);
			return;
		}

		if (_input_stream instanceof ImageInputStream) {
			
			((ImageInputStream)_input_stream).readFully(b);
			if (debug) {
				System.out.println("VicarInputFile.stream_read() ImageInputStream");
				
				DOMutils domUtils = new DOMutils();
				domUtils.dataDump(b, _record_size, 8, 5);
			}
			return;
		}

		if (_input_stream instanceof InputStream) {
			if (debug) {
				System.out.println("VicarInputFile.stream_read() InputStream");
				}
			int off = 0;
			int len = b.length;
			// Shamelessly ripped off from RandomAccessFile.readFully()
			int n = 0;
			do {
			int count = ((InputStream)_input_stream).read(b, off+n, len-n);
			if (count < 0)
				throw new EOFException();
			n += count;
			} while (n < len);
			return;
		}

		throw new UnsupportedOperationException("Read not supported for this stream type: " + _input_stream.getClass().toString());
		}


/***********************************************************************
 * Low-level routine to close the <code>_input_stream</code>.
 * @see #stream_seek
 * @throws UnsupportedOperationException if the stream's type is not recognized.
 */
    protected void stream_close() throws IOException
    {
	if (_input_stream instanceof InputStream) {
	    ((InputStream)_input_stream).close();
	    return;
	}

	if (_input_stream instanceof ImageInputStream) {
	    ((ImageInputStream)_input_stream).close();
	    return;
	}

	throw new UnsupportedOperationException("Close not supported for this stream type");
    }
    
    /***
     * protected int _sourceXSubsample = 1;
   protected int _sourceYSubsample = 1;
   protected int _subsamplingXOffset = 0;
   protected int _subsamplingYOffset = 0;
   protected Rectangle originalRegion;
   protected Rectangle destinationRegion;
     * @param sourceXsubsample
     */
    
    public void setSourceXSubsample(int sourceXSubsample) {
		_sourceXSubsample = sourceXSubsample;
	}
	
	public void setSourceYSubsample(int sourceYSubsample) {
		_sourceYSubsample = sourceYSubsample;
	}
	
	public void setOriginalRegion(Rectangle r) {
		_originalRegion = new Rectangle(r);	
	}
	
	public void setSourceRegion(Rectangle r) {
		_sourceRegion = new Rectangle(r);	
	}
	
	public void setDestinationRegion(Rectangle r) {
		_destinationRegion = new Rectangle(r);	
	}
    
    // ImageReadParam values setters getters
    /**
     * protected ImageReadParam _param ;
   protected Rectangle sourceRegion;
   protected int[] sourceBands;
   protected int sourceXSubsampling;
   protected int sourceYSubsampling;
   protected int subsamplingXOffset;
   protected int subsamplingYOffset;
   ***/
    /**
     * setImageReadParam
     * @param param
     */
    public void setImageReadParam(ImageReadParam param) {
    	_param = param;
    	// get the values and set 
    	_sourceBands = param.getSourceBands();
		_sourceRegion = param.getSourceRegion();
		// _sourceRegion.x y width height
		
				
		_sourceXSubsample = param.getSourceXSubsampling();
		_sourceYSubsample = param.getSourceYSubsampling();
		_subsamplingXOffset = param.getSubsamplingXOffset();
		_subsamplingYOffset = param.getSubsamplingYOffset();
    	
    }
    
    public void printImageReadParams() {
    	
    	
    	System.out.println("VicarInputFile.printImageReadParams");
    	System.out.println("_param "+_param);
    	System.out.println("_sourceBands = "+_sourceBands);
    	if (_sourceBands != null) {
    		for (int i = 0 ; i< _sourceBands.length ; i++) {
    			System.out.println("_sourceBands["+i+"] = "+_sourceBands[i]);
    		}
    	} else {
    		System.out.println("_sourceBands are null"); 	
    	}
    	
    	if (_sourceRegion != null) {
    		System.out.println("_sourceRegion origin "+_sourceRegion.x+","+_sourceRegion.y+
    				" width="+_sourceRegion.width+" height="+_sourceRegion.height);
    	} else {
    		System.out.println("_sourceRegion are null"); 	
    	}
    	
    	System.out.println("_sourceXSubsampling = "+_sourceXSubsample);
    	System.out.println("_sourceYSubsampling = "+_sourceYSubsample);
    	System.out.println("_subsamplingXOffset = "+_subsamplingXOffset);
    	System.out.println("_subsamplingYOffset = "+_subsamplingYOffset);
    	
    }
    

    public int bytesForDataType(int data_type) {
    	int bytes = 1;
    	switch (data_type) {
		case SystemLabel.TYPE_BYTE:
		    bytes=1;
		    break;
		case SystemLabel.TYPE_HALF:
		    bytes=2;
		    break;
		case SystemLabel.TYPE_USHORT:
		    bytes=2;
		    break;
		case SystemLabel.TYPE_FULL:
		    bytes=4;
		    break;
		case SystemLabel.TYPE_REAL:
		    bytes=4;
		    break;
		case SystemLabel.TYPE_DOUB:
		    bytes=8;
		    break;
		case SystemLabel.TYPE_COMP:
			bytes=16;
			break;
    	
    	}
    	return bytes;
    }
    
    public void setDebug(boolean d) {
    	debug = d;
    }
    
    public boolean getDebug() {
    	return debug;
    }
    
    public void setDebug_tile(boolean d) {
    	debug_tile = d;
    }
    
    public boolean getDebug_tile() {
    	return debug_tile;
    }

}

