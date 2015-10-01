package jpl.mipl.io.vicar;

import java.util.*;
import java.io.*;
import java.lang.reflect.*;
import java.beans.*;
import java.awt.image.*;
import javax.media.jai.*;

/**
 * This class manages a set of VICAR input image files, making the set appear
 * like one image to the outside world.  The primary use is to make a set
 * of three separate files (R,G,B triplet) look like a single image to any
 * callers.
 * <p>
 * Each input file is mapped to one (or more) bands in the virtual image
 * presented by this class.  The file size is the max dimension of all
 * input images.  If the input files are not the same size, 0's are read
 * for the parts of the virtual image that are outside the bounds of the
 * input.  However, doing this may slow down the reads a bit.
 * <p>
 * The <code>VicarLabel</code> object is the one associated with the first
 * file.  The <code>SystemLabel</code> object, however, reflects the aggregate
 * of all files (i.e. dimension and data type widening).  So, this means
 * that <code>VicarInputImage.getSystemLabel()</code> does <em>not</em> return
 * the same info as <code>VicarInputImage.getVicarLabel().getSystem()</code>
 * (besides the return type difference).
 * <p>
 * In addition, the files may be offset with respect to one another, for uses
 * like color registration.  However, since this is only implemented per file,
 * not for multiple bands in a single file, nor for other file formats, it is
 * recommended that independent-plane panning be done via a JAI operator
 * or some other mechanism.  It is implemented here only because it is easy.
 * <p>
 * It is recommended that the files match in terms of data type (host
 * representation is irrelevant).  However, mixed types are allowed; the
 * data will be read in the format specified by the SampleModel for the
 * high-level interface, or the format of the buffer for the low-level one.
 * Created SampleModel's use the "widest" data type of all the inputs, i.e.
 * byte->half->full->real->doub->comp.  (note that mixing Complex with others
 * is not recommended; specifically you'll lose data precision if mixed with
 * Double).
 * <p>
 * File organization of the virtual image is always BSQ.  Any org is allowed
 * in the inputs, but a BIP input will be significantly slower.  It is
 * anticipated that BIP images will rarely be used with this class, however.
 * <p>
 * Most other system label items are simply taken from the first input.
 * <p>
 * The binary headers are returned for the first input only.  Access
 * to the other input files' headers must go through the file directly.
 * Binary prefixes are returned for the virtual band in question (the given
 * band is mapped to the file/band number), but beware that prefixes from
 * different files might be of different sizes.  If the physical file mapped
 * to is BIP, prefixes are not returned, as the translation makes no sense
 * in that case.
 * <p>
 * This class allows for multiple threads to simultaneously issue read
 * requests (as <code>VicarInputFile</code> does; see the description there).
 * Not much synchronization is required here; we rely on the
 * <code>VicarInputFile</code> to handle most of that for us.
 * <p>
 * @see VicarInput
 * @see VicarInputFile
 */

public class VicarInputImage implements VicarInput
{
    protected VicarInputFile _files[];

    // Maps from virtual band number to input file and band within that file
    protected int _band_map_file[];
    protected int _band_map_band[];

    protected SystemLabel _system;
    protected SystemLabel _system_in[];		// cached copies of sys lbl

    protected boolean _random_allowed;
    protected boolean _random_easy;

////////////////////////////////////////////////////////////////////////

/***********************************************************************
 * Dummy constructor (for now). Need to add good ones that call open().
 */
    public VicarInputImage()
    {
	_files = null;
	_band_map_file = null;
	_band_map_band = null;
	_system = null;
	_random_allowed = false;
	_random_easy = false;
    }

/***********************************************************************
 * Constructor which calls <code>open(String)</code>.
 * @throws IOException
 */
    public VicarInputImage(String fn) throws IOException
    {
	this();
	open(fn);
    }

/***********************************************************************
 * Opens a virtual file given a set of three <code>VicarInputFile</code>
 * objects, which must already be individually opened.
 * @throws IOException
 */
    public synchronized void open(VicarInputFile red,
				  VicarInputFile grn, VicarInputFile blu)
				  throws IOException
    {
	_files = new VicarInputFile[3];
	_files[0] = red;
	_files[1] = grn;
	_files[2] = blu;
	openInternal();
    }

/***********************************************************************
 * Opens a virtual file given an array of <code>VicarInputFile</code>
 * objects, which must already be individually opened.
 * @throws IOException
 */
    public synchronized void open(VicarInputFile[] files) throws IOException
    {
	_files = (VicarInputFile[])files.clone();
	openInternal();
    }

/***********************************************************************
 * Opens a virtual file given a filename.
 * What about URL's?
 * <p>
 * The given name may contain multiple filenames separated by spaces or
 * commas.  (This prevents a name from containing a space or comma).
 * As a convenience, if a filename (other than the first) starts with
 * a ".", it is considered to be an extension change from the previous
 * file.  The previous name is taken, the last "." and all that follows are
 * stripped, and the given name is appended (if there is no ".", the extension
 * is simply appended).  This allows strings of the form
 * "/some/directory/path/image.red, .grn, .blu" as a convenient user
 * shorthand.
 * @throws IOException
 * @see #open(List)
 */
    public synchronized void open(String fn) throws IOException
    {
	List name_list = new ArrayList();
	String last_name = null;

	StringTokenizer st = new StringTokenizer(fn, ", ");
	while (st.hasMoreTokens()) {
	    String current = st.nextToken();

	    // Check for .ext

	    if (last_name != null && current.charAt(0) == '.') {
		int ind = last_name.lastIndexOf('.');
		if (ind >= 0)
		    last_name = last_name.substring(0, ind);
		current = last_name + current;
	    }

	    name_list.add(current);
	    last_name = current;
	}

	open(name_list);
    }

/***********************************************************************
 * Opens a virtual file given a List of things.  The List must contain
 * any type that's acceptable to <code>VicarInputFile.open(Object)</code>.
 * Typically, this will be String's, but can be various kinds of streams
 * as well.  Note that unlike open(String), no parsing or extension
 * swapping is done on the names; what you pass in is the file you get.
 * (This does allow blanks and commas in the name, and files that start
 * with ".").
 * @throws IOException
 * @see VicarInputFile#open(Object)
 */
    public synchronized void open(List v) throws IOException
    {
	_files = new VicarInputFile[v.size()];

	for (int i=0; i < v.size(); i++) {
	    _files[i] = new VicarInputFile();
	    _files[i].open(v.get(i));
	}

	openInternal();
    }

/***********************************************************************
 * Does the actual work of opening the virtual file.  Assumes that _files
 * array has been set up, but nothing else has.  Each of the individual
 * files must already be open.
 * @throws IOException
 */
    protected void openInternal() throws IOException
    {
	int i;

	if (_files.length == 0)
	    throw new FileNotFoundException(
	       "No VicarInputFile objects specified in VicarInputImage.open()");

	// Create the SystemLabel for this virtual file.  Use the label
	// from the first input as a base.

	try {
	    _system = new SystemLabel(_files[0].getVicarLabel().getSystem());
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

	// Count the number of bands in all files.  Generate the system_in
	// label cache while we're at it.

	_system_in = new SystemLabel[_files.length];
	int nb = 0;
	for (i=0; i < _files.length; i++) {
	    _system_in[i] = _files[i].getSystemLabel();
	    nb += _system_in[i].getNB();
	}
	_system.setNB(nb);
	_band_map_file = new int[nb];
	_band_map_band = new int[nb];

	// Now adjust the system label based on the input images

	int band_index = 0;
	_random_allowed = true;
	_random_easy = true;

	for (i=0; i < _files.length; i++) {

	    SystemLabel sys_in = _system_in[i];

	    // Format codes are conveniently in numerical order of increasing
	    // width.  Pick the widest one (byte->half->full->real->doub->comp).

	    if (sys_in.getFormatCode() > _system.getFormatCode())
		_system.setFormatCode(sys_in.getFormatCode());

	    if (sys_in.getNL() > _system.getNL())
		_system.setNL(sys_in.getNL());

	    if (sys_in.getNS() > _system.getNS())
		_system.setNS(sys_in.getNS());

	    // Create band maps

	    for (int j=0; j < sys_in.getNB(); j++) {
		_band_map_file[band_index] = i;
		_band_map_band[band_index] = j;
		band_index++;
	    }

	    // Random is allowed/easy only if so for all files.

	    if (!_files[i].isRandomAccessAllowed())
		_random_allowed = false;
	    if (!_files[i].isRandomAccessEasy())
		_random_easy = false;
	}

	// Dimension must be 3, since this is a multi-band virtual file
	// And, we're always in BSQ.

	_system.setDim(3);
	_system.setOrg("BSQ");

    }

/***********************************************************************
 * Closes all component <code>VicarInputFile</code>s.
 */
    public synchronized void close() throws IOException
    {
	if (_files == null)
	    return;
	for (int i=0; i < _files.length; i++)
	    _files[i].close();
    }

/***********************************************************************
 * Returns the actual file for a given file index (NOT a band number!).
 * @see #getFileIndex(int)
 */
    public VicarInputFile getFile(int index)
    {
	if (_files == null)
	    return null;
	return _files[index];
    }

/***********************************************************************
 * Maps a virtual band number into a file index.  This index can then be
 * used with <code>getFile()</code>.
 * @see #getFile(int)
 * @see #getBand(int)
 */
    public int getFileIndex(int band)
    {
	return _band_map_file[band];
    }

/***********************************************************************
 * Maps a virtual band number into a band number within the actual file.
 * This should be used in conjunction with <code>getFileIndex()</code>.
 * @see #getFileIndex(int)
 */
    public int getBand(int band)
    {
	return _band_map_band[band];
    }

////////////////////////////////////////////////////////////////////////

/***********************************************************************
 * Returns the <code>VicarLabel</code> object for this label.  This
 * routine should <em>not</em> be used to retrieve the system label
 * bean; use <code>getSystemLabel()</code> instead.  In the case of
 * the <code>VicarInputImage</code> class, the label of the first actual
 * input file is returned.
 * <p>
 * Note that requesting the label via this routine will cause the EOL labels
 * to be read if needed.  On a sequential-only stream, this could get you
 * into trouble and thus this should be done only after the image data has
 * been read.  If random access is possible but not easy, this can be done
 * before reading the data but you could take a significant performance hit.
 * If it is easy, then this can be called any time.
 * <p>
 * @see #getSystemLabel()
 */
    public VicarLabel getVicarLabel() throws IOException
    {
	if (_files == null)
	    return null;
	return _files[0].getVicarLabel();
    }

/***********************************************************************
 * Indicates whether or not the <code>VicarLabel</code> has been completely
 * read.  This can be used with sequential or random-hard streams to determine
 * whether or not <code>getVicarLabel()</code> will do bad things.  The label
 * will always be completely read for random-easy streams, or for any stream
 * if there are no EOL labels.
 * @see #getVicarLabel()
 */
    public boolean isLabelComplete()
    {
	if (_files == null)
	    return false;
	return _files[0].isLabelComplete();
    }

/***********************************************************************
 * Retrieves a deep copy of the <code>SystemLabel</code> object associated
 * with this virtual image.  This should not be confused with
 * <code>VicarLabel.getSystem()</code>, which returns a
 * <code>VicarLabelSet</code> rather than a <code>SystemLabel</code> object.
 * And, in this case, the <code>SystemLabel</code> object is synthetic,
 * derived from the first input file but not identical to any of them.
 */
    public synchronized SystemLabel getSystemLabel()
    {
	return (SystemLabel)_system.clone();
    }

/***********************************************************************
 * Return true if random access to this file is <em>possible</em>.  It
 * might be expensive, however.  Random access is possible only if all
 * component files support it.
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
 * Random access is easy only if all component files are easy.
 */
    public boolean isRandomAccessEasy()
    {
	return _random_easy;
    }

////////////////////////////////////////////////////////////////////////

/***********************************************************************
 * Low-level read function, returns a byte array for the given record.
 * Since the virtual file is always BSQ, n2==line and n3==band.
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
 * @param n2 Line number to read (the virtual file is always BSQ).
 * @param n3 Band number to read (the virtual file is always BSQ).
 * @throws NonSequentialAccessException (subclass of IOException)
 * @throws IOException
 */
    public void readRecord(byte[] data, int start, int length, int offset,
			int pixelStride, int n2, int n3) throws IOException
    {
	if (length == 0)
	    length = _system.getN1() - start;

	int file = _band_map_file[n3];		// translate band to file
	int band = _band_map_band[n3];		// band within that file

	switch (_system_in[file].getOrgCode()) {
	    case SystemLabel.ORG_BSQ:
		_files[file].readRecord(data, start, length, offset,
							pixelStride, n2, n3);
		break;
	    case SystemLabel.ORG_BIL:
		_files[file].readRecord(data, start, length, offset,
							pixelStride, n3, n2);
		break;
	    case SystemLabel.ORG_BIP:
		// We must do it the hard way, since we can't read a single
		// record.  Read one element (specified by the desired band)
		// from the given samp/line combintion and put it directly in
		// the data buffer.
		for (int i=0; i < length; i++) {
		    _files[file].readRecord(data, band, 1, i*pixelStride+offset,
							1, i+start, n2);
		}
		break;
	}
    }

/***********************************************************************
 * Low-level read function, returns a short array for the given record.
 * Since the virtual file is always BSQ, n2==line and n3==band.
 * Follow the See Also link for parameters
 * @see #readRecord(byte[], int, int, int, int, int, int)
 */
    public void readRecord(short[] data, int start, int length, int offset,
			int pixelStride, int n2, int n3) throws IOException
    {
	if (length == 0)
	    length = _system.getN1() - start;

	int file = _band_map_file[n3];		// translate band to file
	int band = _band_map_band[n3];		// band within that file

	switch (_system_in[file].getOrgCode()) {
	    case SystemLabel.ORG_BSQ:
		_files[file].readRecord(data, start, length, offset,
							pixelStride, n2, n3);
		break;
	    case SystemLabel.ORG_BIL:
		_files[file].readRecord(data, start, length, offset,
							pixelStride, n3, n2);
		break;
	    case SystemLabel.ORG_BIP:
		// We must do it the hard way, since we can't read a single
		// record.  Read one element (specified by the desired band)
		// from the given samp/line combintion and put it directly in
		// the data buffer.
		for (int i=0; i < length; i++) {
		    _files[file].readRecord(data, band, 1, i*pixelStride+offset,
							1, i+start, n2);
		}
		break;
	}
    }

/***********************************************************************
 * Low-level read function, returns an unsigned short array for the given
 * record.  Although unsigned short is not a VICAR data type, it is useful
 * for some other formats which use this library.  Note that a short array
 * must be used, although the data should be interpreted as unsigned.
 * For this reason, a different method name is needed.
 * <p>
 * Since the virtual file is always BSQ, n2==line and n3==band.
 * Follow the See Also link for parameters
 * @see #readRecord(byte[], int, int, int, int, int, int)
 */
    public void readRecordUshort(short[] data, int start, int length,
		int offset, int pixelStride, int n2, int n3) throws IOException
    {
	if (length == 0)
	    length = _system.getN1() - start;

	int file = _band_map_file[n3];		// translate band to file
	int band = _band_map_band[n3];		// band within that file

	switch (_system_in[file].getOrgCode()) {
	    case SystemLabel.ORG_BSQ:
		_files[file].readRecordUshort(data, start, length, offset,
							pixelStride, n2, n3);
		break;
	    case SystemLabel.ORG_BIL:
		_files[file].readRecordUshort(data, start, length, offset,
							pixelStride, n3, n2);
		break;
	    case SystemLabel.ORG_BIP:
		// We must do it the hard way, since we can't read a single
		// record.  Read one element (specified by the desired band)
		// from the given samp/line combintion and put it directly in
		// the data buffer.
		for (int i=0; i < length; i++) {
		    _files[file].readRecordUshort(data, band, 1,
					i*pixelStride+offset, 1, i+start, n2);
		}
		break;
	}
    }

/***********************************************************************
 * Low-level read function, returns an int array for the given record.
 * Follow the See Also link for parameters
 * @see #readRecord(byte[], int, int, int, int, int, int)
 */
    public void readRecord(int[] data, int start, int length, int offset,
			int pixelStride, int n2, int n3) throws IOException
    {
	if (length == 0)
	    length = _system.getN1() - start;

	int file = _band_map_file[n3];		// translate band to file
	int band = _band_map_band[n3];		// band within that file

	switch (_system_in[file].getOrgCode()) {
	    case SystemLabel.ORG_BSQ:
		_files[file].readRecord(data, start, length, offset,
							pixelStride, n2, n3);
		break;
	    case SystemLabel.ORG_BIL:
		_files[file].readRecord(data, start, length, offset,
							pixelStride,  n3, n2);
		break;
	    case SystemLabel.ORG_BIP:
		// We must do it the hard way, since we can't read a single
		// record.  Read one element (specified by the desired band)
		// from the given samp/line combintion and put it directly in
		// the data buffer.
		for (int i=0; i < length; i++) {
		    _files[file].readRecord(data, band, 1, i*pixelStride+offset,
							1, i+start, n2);
		}
		break;
	}
    }

/***********************************************************************
 * Low-level read function, returns a float array for the given record.
 * Follow the See Also link for parameters
 * @see #readRecord(byte[], int, int, int, int, int)
 */
    public void readRecord(float[] data, int start, int length, int offset,
			int pixelStride, int n2, int n3) throws IOException
    {
	if (length == 0)
	    length = _system.getN1() - start;

	int file = _band_map_file[n3];		// translate band to file
	int band = _band_map_band[n3];		// band within that file

	switch (_system_in[file].getOrgCode()) {
	    case SystemLabel.ORG_BSQ:
		_files[file].readRecord(data, start, length, offset,
							pixelStride, n2, n3);
		break;
	    case SystemLabel.ORG_BIL:
		_files[file].readRecord(data, start, length, offset,
							pixelStride, n3, n2);
		break;
	    case SystemLabel.ORG_BIP:
		// We must do it the hard way, since we can't read a single
		// record.  Read one element (specified by the desired band)
		// from the given samp/line combintion and put it directly in
		// the data buffer.
		for (int i=0; i < length; i++) {
		    _files[file].readRecord(data, band, 1, i*pixelStride+offset,
							1, i+start, n2);
		}
		break;
	}
    }

/***********************************************************************
 * Low-level read function, returns a double array for the given record.
 * Follow the See Also link for parameters
 * @see #readRecord(byte[], int, int, int, int, int, int)
 */
    public void readRecord(double[] data, int start, int length, int offset,
			int pixelStride, int n2, int n3) throws IOException
    {
	if (length == 0)
	    length = _system.getN1() - start;

	int file = _band_map_file[n3];		// translate band to file
	int band = _band_map_band[n3];		// band within that file

	switch (_system_in[file].getOrgCode()) {
	    case SystemLabel.ORG_BSQ:
		_files[file].readRecord(data, start, length, offset,
							pixelStride, n2, n3);
		break;
	    case SystemLabel.ORG_BIL:
		_files[file].readRecord(data, start, length, offset,
							pixelStride, n3, n2);
		break;
	    case SystemLabel.ORG_BIP:
		// We must do it the hard way, since we can't read a single
		// record.  Read one element (specified by the desired band)
		// from the given samp/line combintion and put it directly in
		// the data buffer.
		for (int i=0; i < length; i++) {
		    _files[file].readRecord(data, band, 1, i*pixelStride+offset,
							1, i+start, n2);
		}
		break;
	}
    }

/***********************************************************************
 * Low-level read function, returns complex data in a float array for the
 * given record.  Each complex value is stored in two consecutive float
 * elements in the array, so it must be twice as long as you would normally
 * expect, and <code>offset</code> must be multiplied by 2.  <code>length</code>
 * is still measured in <em>pixels</em>, so there are 2*<code>length</code>
 * values filled in the array.
 * <p>
 * Follow the See Also link for parameters
 * @see #readRecord(byte[], int, int, int, int, int, int)
 */
    public void readRecordComp(float[] data, int start, int length, int offset,
			int pixelStride, int n2, int n3) throws IOException
    {
	// Not quite sure how to handle complex w.r.t. multiple bands
	throw new UnsupportedOperationException("Complex data not yet supported in VicarInputImage!!!!");
    }

////////////////////////////////////////////////////////////////////////

/***********************************************************************
 * Provides access to the binary header, which is NLB records at the beginning
 * of the file.  Only the first physical file's header is returned.  To access
 * other headers, get a pointer to the physical file and use that.
 * Follow the See Also link for complete description
 * @see VicarInput#getBinaryHeader()
 * @see #getFile(int)
 */
    public VicarBinaryLabel getBinaryHeader() throws IOException
    {
	if (_files == null)
	    return null;

	return _files[0].getBinaryHeader();
    }

/***********************************************************************
 * Provides access to the binary prefix, which is NBB bytes at the beginning
 * of each record.  The given band number (always n3 because the virtual
 * file is always BSQ) is mapped into the proper file/band combination
 * and that prefix is returned, but beware that prefixes from different files
 * might be of different sizes.  If the physical file mapped to is BIP,
 * null is returned, as the translation makes no sense in that case.  Use
 * <code>getFile()</code> to get the physical file in this case.
 * <p>
 * Follow the See Also link for complete description.
 * @see VicarInput#getBinaryPrefix(int,int) 
 */
    public VicarBinaryLabel getBinaryPrefix(int n2, int n3) throws IOException
    {
	int file = _band_map_file[n3];		// translate band to file
	int band = _band_map_band[n3];		// band within that file

	int org = _system_in[file].getOrgCode();
	switch (org) {
	    case SystemLabel.ORG_BSQ:
		return _files[file].getBinaryPrefix(n2, band);
	    case SystemLabel.ORG_BIL:
		return _files[file].getBinaryPrefix(band, n2);
	    case SystemLabel.ORG_BIP:
		return null;
	}
	return null;				// never reached
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
	if (_system.getFormatCode() == SystemLabel.TYPE_COMP)
	    num_bands *= 2;			//!!!! is this right????

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

	//!!!! BUG IN JDK 1.2 !!!!
	//!!!! ComponentSampleModel and subclasses always return null for
	//!!!! createDataBuffer for TYPE_SHORT.  In order to work around
	//!!!! this, we use ComponentSampleModelJAI instead and sacrifice
	//!!!! the added efficiency of the Banded/PixelInterleaved classes.
	//!!!! The easiest way to do this is to turn on float_type, since
	//!!!! the JAI class must be used for floats anyway, and this is
	//!!!! the only use of this flag.  When JDK is fixed, turn this
	//!!!! flag back off!!!!
		float_type = true;

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

	// Compute offsets, etc... then create the SM
	// Always BSQ... one bank per band of data
	pixel_stride = 1;
	scanline_stride = tileWidth;
	for (i=0; i < num_bands; i++) {
	    band_offsets[i] = 0;
	    bank_indices[i] = i;
	}
	if (float_type)
	    return new ComponentSampleModelJAI(data_buffer_type,
			tileWidth, tileHeight,
			pixel_stride, scanline_stride,
			bank_indices, band_offsets);

	return new BandedSampleModel(data_buffer_type,
			tileWidth, tileHeight,
			scanline_stride,
			bank_indices, band_offsets);
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
    public void readTile(int x, int y, int w, int h, int x_off, int y_off,
			int bandList[],
			SampleModel sm, DataBuffer db)
			throws IOException
    {
	int i;
	int band, band_ind, virt_band, file;
	int band_subset[] = new int[1];

	int num_bands = sm.getNumBands();
	if (bandList != null && num_bands > bandList.length)
	    num_bands = bandList.length;

	if (w == 0)
	    w = sm.getWidth() - x_off;
	if (h == 0)
	    h = sm.getHeight() - y_off;

	if (x_off + w > sm.getWidth())
	    throw new ArrayIndexOutOfBoundsException(
		"Illegal width in VICAR readTile: " + w + ", x_off=" + x_off +
			", width=" + sm.getWidth());
	if (y_off + h > sm.getHeight())
	    throw new ArrayIndexOutOfBoundsException(
		"Illegal height in VICAR readTile: " + h + ", y_off=" + y_off +
			", height=" + sm.getHeight());

	// Loop through the bands, processing one at a time.  For each one,
	// we map that to a file/band combination, then read that band
	// directly into the DataBuffer.

	for (band_ind=0; band_ind < num_bands; band_ind++) {

	    // virt_band is the virtual (logical, to caller) band number.
	    // band is the physical (in a given file) band number.

	    if (bandList == null)
		virt_band = band_ind;
	    else
		virt_band = bandList[band_ind];

	    file = _band_map_file[virt_band];	// translate band to file
	    band = _band_map_band[virt_band];	// band within that file

	    // Create a sample model that references just this one virtual
	    // band.  That way, we can simply pull the right parts of the
	    // file directly into the right place in the DataBuffer.

	    band_subset[0] = virt_band;
//!!!! JAI BUG !!!!  Should be the first line.  See createSubsetSampleModel().
//!!!!	    SampleModel one_band_sm = sm.createSubsetSampleModel(band_subset);
	    SampleModel one_band_sm = createSubsetSampleModel(sm, band_subset);

	    // Read a single band's tile from the file.

	    band_subset[0] = band;	// what to pull from input file
	    _files[file].readTile(x, y, w, h, x_off, y_off,
                        band_subset, one_band_sm, db);

	}
    }

/***********************************************************************
 * Work around a bug in ComponentSampleModel{JAI}.createSubsetSampleModel().
 * The <em>only</em> reason this routine exists is because ComponentSampleModel
 * and ComponentSampleModelJAI have a bug in createSubsetSampleModel() such
 * that the band indices are ignored.  This routine works around this bug in
 * the case of ComponentSampleModel and ComponentSampleModelJAI <em>ONLY</em>.
 * Turns out that BandedSampleModel works, and that the bug doesn't matter for
 * PixelInterleavedSampleModel because the indices are 0.
 * <p>
 * <em>NOTE:</em>  This function knows only about the standard subclasses of
 * ComponentSampleModel.  If you create your own subclass, it will not be
 * returned by this function; an instance of ComponentSampleModel will instead
 * (or ComponentSampleModelJAI if you derive from it).
 * <p>
 * !!!! REMOVE THIS FUNCTION WHEN THE CSM/JAI BUGS ARE FIXED!!!!
 */
    protected SampleModel createSubsetSampleModel(SampleModel sm_in,int[] bands)
    {
	if (sm_in instanceof BandedSampleModel)		// works
	     sm_in.createSubsetSampleModel(bands);
	if (sm_in instanceof PixelInterleavedSampleModel)  // no band indices
	     sm_in.createSubsetSampleModel(bands);

	if (!(sm_in instanceof ComponentSampleModel))
	     sm_in.createSubsetSampleModel(bands);	// who knows

	ComponentSampleModel sm = (ComponentSampleModel) sm_in;

	int[] bankIndices = sm.getBankIndices();
	int[] bandOffsets = sm.getBandOffsets();

	int newBankIndices[] = new int[bands.length];
	int newBandOffsets[] = new int[bands.length];
	for (int i=0; i<bands.length; i++) {
	    newBankIndices[i] = bankIndices[bands[i]];
	    newBandOffsets[i] = bandOffsets[bands[i]];
	}

	// Return the right type, based on what we're given.
	// Might break if an unknown subclass is passed in.

	if (sm_in instanceof ComponentSampleModelJAI)
	    return new ComponentSampleModelJAI(sm.getDataType(),
		sm.getWidth(), sm.getHeight(),
		sm.getPixelStride(), sm.getScanlineStride(),
		newBankIndices, newBandOffsets);

	return new ComponentSampleModel(sm.getDataType(),
		sm.getWidth(), sm.getHeight(),
		sm.getPixelStride(), sm.getScanlineStride(),
		newBankIndices, newBandOffsets);
    }

}

