package jpl.mipl.io.vicar;

import jpl.mipl.io.streams.*;
import java.io.*;
import java.awt.image.*;
import javax.media.jai.*;
import javax.imageio.stream.*;
import java.nio.ByteOrder;
import java.awt.Rectangle;

/**
 * This class manages a single VICAR output image file.
 * <p>
 * All accesses to the VICAR file are thread-safe, assuming that nobody else
 * tries to access the underlying stream directly.  Thus, multiple threads
 * can issue simultaneous <code>writeRecord()</code> or <code>writeTile()</code>
 * requests, although each request is handled one at a time via synchronization
 * on the <code>VicarOutputFile</code> object.  However, if you have a
 * sequential-only stream, all accesses must still be strictly sequential...
 * meaning that the use of multiple threads with sequential streams will not
 * work (the request order would be non-deterministic).  For random-hard
 * streams, threads will work but could cause performance hits depending on
 * the ordering of the requests.  Random-easy streams should be fine.
 * <p>
 * @see VicarOutputImage
 * @see VicarOutput
 */
 
public class VicarOutputFile implements VicarOutput
{
    /** The output stream object.  Must be a <code>OutputStream</code> or
     *  <code>DataOutput</code>, but specific subclasses may be handled
     *  specially (e.g. <code>RandomAccessFileOutputStream</code>,
     *  <code>ImageOutputStream</code>).  See the <code>stream_</code>*()
     *  functions.
     */
    protected Object _output_stream;

    /** Wrapper around <code>_output_stream</code> that implements
     *  <code>ImageOutputStreamStride</code> and handles any required
     *  byte-swapping etc.
     */
    protected ImageOutputStreamStride _output_stream_wrap;

    protected VicarLabel _label;
    protected SystemLabel _system;

    protected int _lblsize_front;
    protected long _image_size_bytes;
    protected long _current_file_pos;	// Current position for seq files
    protected boolean _expandable;	// true iff file length not specified
    protected int _expandable_size;	// size N2 or N3 should be

    protected boolean _file_open;

    /** Random access mode flags */
    protected boolean _random_allowed;
    protected boolean _random_easy;

    protected VicarDataFormat _data_format;
    protected VicarDataFormat _binary_data_format;

    protected VicarInput _primary_input;
    protected VicarLabel _primary_label;
    protected SystemLabel _primary_system;
    
    protected VicarBinaryLinePrefix _vicarBinaryLinePrefix; // used by Vicar and PDS to hold any binary prefix data

    // Internal data buffers

    protected int _int_buffer[];
    protected int _int_bufsize;
    protected float _float_buffer[];
    protected int _float_bufsize;
    protected double _double_buffer[];
    protected int _double_bufsize;
    
    boolean _debug = false;
    // boolean _debug = true;
   
////////////////////////////////////////////////////////////////////////

/***********************************************************************
 * Dummy constructor (for now). Need to add good ones that call open().
 */
    public VicarOutputFile()
    {
	_output_stream = null;
	_label = null;
	_system = null;
	_lblsize_front = 0;
	_current_file_pos = 0;
	_image_size_bytes = 0;
	_expandable = false;
	_expandable_size = 0;
	_file_open = false;
	_random_allowed = false;
	_random_easy = false;
	_data_format = null;
	_binary_data_format = null;
	_int_buffer = null;
	_int_bufsize = -1;
	_float_buffer = null;
	_float_bufsize = -1;
	_double_buffer = null;
	_double_bufsize = -1;
    }

/***********************************************************************
 * Opens a file given an <code>OutputStream</code>.  Random access is
 * available only if the given stream is a subclass of
 * <code>RandomAccessFileOutputStream</code>.
 * @throws IOException
 */
    public synchronized void open(OutputStream os) throws IOException
    {
	_random_allowed = false;
	_random_easy = false;

	_output_stream = os;

	if (os instanceof RandomAccessFileOutputStream) {
	    _random_allowed = true;
	    _random_easy = true;
	}

	openInternal();
    }

/***********************************************************************
 * Opens a file given an <code>ImageOutputStream</code>.
 * @throws IOException
 */
    public synchronized void open(ImageOutputStream os) throws IOException
    {
	_random_allowed = true;
	_random_easy = !os.isCached();

	_output_stream = os;

	openInternal();
    }

/***********************************************************************
 * Opens a file given a <code>RandomAccessFile</code>.
 * @throws IOException
 */
    public synchronized void open(RandomAccessFile raf) throws IOException
    {
	open(new RandomAccessFileOutputStream(raf));
    }

/***********************************************************************
 * Opens a file given a filename.
 * What about URL's?
 * @throws IOException
 */
    public synchronized void open(String fn) throws IOException
    {
	open(new RandomAccessFile(fn, "rw"));
    }

/***********************************************************************
 * Opens a file given a generic <code>Object</code>.  The appropriate
 * <code>open()</code> routine is called based on whether the object is
 * a <code>String</code> (filename), <code>ImageOutputStream</code>,
 * <code>RandomAccessFile</code>, or generic <code>OutputStream</code>.
 * @throws UnsupportedOperationException if it's an unrecognized type.
 */
    public void open(Object obj) throws IOException
    {
	if (obj instanceof String) {
	    open((String) obj);
	    return;
	}
	if (obj instanceof ImageOutputStream) {
	    open((ImageOutputStream) obj);
	    return;
	}
	if (obj instanceof RandomAccessFile) {
	    open((RandomAccessFile) obj);
	    return;
	}
	if (obj instanceof OutputStream) {
	    open((OutputStream) obj);
	    return;
	}

	throw new UnsupportedOperationException(
			"Unrecognized object type in VicarOutputFile.open");
    }

/***********************************************************************
 * Does the actual work of opening the file.  Writes the SystemLabel, and
 * any Vicar label currently present.
 * @throws IOException
 * @throws AlreadyOpenException if the file's been opened already
 */
    protected void openInternal() throws IOException, AlreadyOpenException
    {
	if (_file_open)
	    throw new AlreadyOpenException("file has been opened already");

	// Make sure these are not null.  The get's will create if so.

	_label = getVicarLabel();
	_system = getSystemLabel();

	// Set up defaults for missing label items.
	// Note that the host format defaults for input files are VAX, because
	// that was the only kind of file in existence before the host type
	// labels were added.  Output files default to Java.

	if (!_system.isHostValid()) {
	    _system.setHost("JAVA");
	}
	if (!_system.isIntFmtValid()) {
	    _system.setIntFmt("HIGH");
	}
	if (!_system.isRealFmtValid()) {
	    _system.setRealFmt("IEEE");
	}

	// Check to make sure the image sizes are specified.  Check for the
	// case of a grow-able file and set the flag appropriately.

	if ((_system.getN1() <= 0) || (_system.getN2() < 0) ||
		(_system.getN3() < 0)) {
	    throw new IllegalArgumentException("Image sizes must be > 0");
	}
	_expandable = false;
	_expandable_size = 0;
	if (_system.getN2() == 0) {		// expandable line
	    if (_system.getN3() != 1)
		throw new IllegalArgumentException(
		     "If N2 is 0 to indicate an expandable file, N3 must be 1");
	    _expandable = true;
	}
	if (_system.getN3() == 0)
	    _expandable = true;

	if (_expandable && !_random_allowed)
	    throw new IllegalArgumentException(
				"Expandable files must be random-access");

	// Install the system label into the VICAR one

	try {
	    _system.writeLabel(_label.getSystem());
	} catch (Exception e) {
	    throw new IOException("Error in System bean processing: " + e);
	}

	// Now write the label to the file.  The length is unlimited since
	// we have not written any data.

	_file_open = true;

	seekToLocation(0);	// just to be sure - affects only random streams

	// Get the host and data formats and set up the VicarDataFormat object.

	_data_format = new VicarDataFormat(_system.getHost(),
			_system.getIntFmt(), _system.getRealFmt());

	// This is a bit messy but avoids having the io.streams package
	// depend on the io.vicar package...

	int int_order = ImageInputStreamStride.HIGH_ORDER;
	if (_data_format.getIntFormatCode() == VicarDataFormat.INT_FMT_LOW)
	    int_order = ImageInputStreamStride.LOW_ORDER;

	int float_order = ImageInputStreamStride.HIGH_ORDER;
	if (_data_format.getIntFormatCode() == VicarDataFormat.REAL_FMT_RIEEE)
	    float_order = ImageInputStreamStride.LOW_ORDER;
	if (_data_format.getIntFormatCode() == VicarDataFormat.REAL_FMT_VAX)
	    float_order = ImageInputStreamStride.VAX_ORDER;

	// If an ImageOutputStream, set the byte ordering there too.  If
	// it's VAX or inconsistent, leave it BIG and the Stride class
	// will take care of it (it re-verifies the order is right).

	if (_output_stream instanceof ImageOutputStream) {
	    if (int_order == ImageInputStreamStride.LOW_ORDER &&
		float_order == ImageInputStreamStride.LOW_ORDER)

		((ImageOutputStream)_output_stream).setByteOrder(
						ByteOrder.LITTLE_ENDIAN);
	    else
		((ImageOutputStream)_output_stream).setByteOrder(
						ByteOrder.BIG_ENDIAN);
	}

	_output_stream_wrap = new ImageOutputStreamStride(_output_stream,
					int_order, float_order);

	if (_debug) {
		System.out.println("openInternal() calling writeOutLabels()");
		System.out.println("_current_file_pos = "+_current_file_pos);
	}
	writeOutLabels();
    }

/***********************************************************************
 * Writes the label information out to the file.  This is intended to be
 * overridden by subclasses for different label formats.  The method must
 * set up: _lblsize_front, _image_size_bytes, and _current_file_pos.
 */
    protected void writeOutLabels() throws IOException
    {
    	if (_debug) {
    		System.out.println("writeOutLabels() A");
    		System.out.println("_lblsize_front = " +_lblsize_front);
    		System.out.println("_system.getRecsize() = "+_system.getRecsize());
    		System.out.println("_current_file_pos = "+_current_file_pos);
    	}
	VicarLabel.ItemPos pos = _label.writeLabelChunk(_output_stream,
			0, _system.getRecsize(), null);
	if (!pos.isComplete) {
	    throw new IOException("Error writing initial VICAR label");
	}

	_lblsize_front = pos.lblsize;

	_image_size_bytes = (_system.getNLB() +
					(_system.getN2() * _system.getN3()))
			* _system.getRecsize();
	
	if (_debug) {
		System.out.println("writeOutLabels() B");
		System.out.println("_lblsize_front = " +_lblsize_front);
		System.out.println("_image_size_bytes = "+_image_size_bytes);
		System.out.println("_lblsize_front + _image_size_bytes = "+(_image_size_bytes+_lblsize_front) );
		System.out.println("_current_file_pos = "+_current_file_pos);
	}

	_current_file_pos = _lblsize_front;
    }

/***********************************************************************
 * Closes the file or stream.  For expandable files, also writes out the
 * label including the actual file size.  For non-expandable files, the
 * label is written every time it changes, so we don't need to do it here.
 */
    public synchronized void close() throws IOException
    {
	if (!_file_open)
	    throw new IOException("Can't close a file that's not open");

	if (_expandable) {
	    if (_expandable_size == 0)
		throw new IOException("Expandable file has no records");

	    // +1 because size is really a line #, which is 0-based
	    if (_system.getN2() == 0)
		_system.setN2(_expandable_size + 1);
	    else
		_system.setN3(_expandable_size + 1);

	    _image_size_bytes = (_system.getNLB() +
					(_system.getN2() * _system.getN3()))
			* _system.getRecsize();

	    _expandable = false;	// allow labels to write

	    // Write the label to the file.

	    writeLabelInternal();
	}
	
	if (_debug) {
		System.out.println("close()");
		System.out.println("_current_file_pos = "+_current_file_pos);
	}

	stream_close();

	// Setting _file_open to false here allows someone to re-use this
	// object with another file.  Weird, but no reason to disallow it.

	_file_open = false;

    }

////////////////////////////////////////////////////////////////////////

/***********************************************************************
 * Sets the "primary input" for this output file.  The initial system and
 * VICAR labels are obtained from the primary input.  If the PI is not
 * specified, the initial labels are default/empty.  Once the system or
 * VICAR labels have been set, or obtained once, the primary input is no
 * longer used.
 * <p>
 * This routine should obviously be called before getting the system or
 * VICAR labels if it is to have any effect.
 * <p>
 * The label is not obtained from the input until/unless it is actually needed
 * (deferred execution).
 * Calling this routine nulls any labels set via the other
 * <code>setPrimaryInput()</code> routine.
 * <p>
 * Passing in null will completely disable the primary input mecahnism.
 * <p>
 * @see #setPrimaryInput(SystemLabel, VicarLabel)
 * @see VicarOutput#setPrimaryInput
 * @throws AlreadyOpenException if the file has been opened already
 */
    public synchronized void setPrimaryInput(VicarInput inp)
						throws AlreadyOpenException
    {
	if (_file_open)
	    throw new AlreadyOpenException(
			"Can't set primary input on an open file");
	_primary_input = inp;
	_primary_system = null;
	_primary_label = null;
    }

/***********************************************************************
 * Gets the primary input, or null if none is available.  PI's set via
 * the second method (<code>setPrimaryInput(SystemLabel, VicarLabel)</code>)
 * are not returned by this method.
 */
    public synchronized VicarInput getPrimaryInput()
    {
	return _primary_input;
    }

/***********************************************************************
 * Sets the "primary input" labels directly, rather than via a
 * <code>VicarInput</code> object.  If <code>VicarLabel</code> is null,
 * a default (empty) label is used instead.  If <code>SystemLabel</code>
 * is null, the system label from the <code>VicarLabel</code> parameter
 * is used (which is otherwise ignored).
 * <p>
 * Calling this routine nulls out any primary input set via the other
 * <code>setPrimaryInput()</code> routine.
 * <p>
 * To completely disable the primary input, don't use this routine; pass in
 * null to the other.
 * @see #setPrimaryInput(VicarInput)
 * @see VicarOutput#setPrimaryInput(SystemLabel, VicarLabel)
 * @throws AlreadyOpenException if the file has been opened already
 */
    public synchronized void setPrimaryInput(SystemLabel slbl, VicarLabel vlbl)
						throws AlreadyOpenException
    {
	if (_file_open)
	    throw new AlreadyOpenException(
			"Can't set primary input on an open file");

	_primary_input = null;
	_primary_system = slbl;
	_primary_label = vlbl;

	if (_primary_label == null)
	    _primary_label = new VicarLabel();
	if (_primary_system == null) {
	    try {
		_primary_system = new SystemLabel(_primary_label.getSystem());
	    } catch (Exception e) {
    		_primary_system = null;
	    }
	}
    }

/***********************************************************************
 * Returns a deep clone of the primary input system label (set via either
 * method).  This should be used only in special cases; normally the main
 * <code>getSystemLabel()</code> should be used instead.  Returns null
 * if the PI or label doesn't exist; a default is not returned.
 * @see #setPrimaryInput(VicarInput)
 * @see #setPrimaryInput(SystemLabel, VicarLabel)
 * @see #getSystemLabel()
 */
    public synchronized SystemLabel getPrimaryInputSystemLabel()
							throws IOException
    {
	if (_primary_input != null)
	    return _primary_input.getSystemLabel();
	if (_primary_system != null)
	    return (SystemLabel)_primary_system.clone();
	return null;
    }

/***********************************************************************
 * Returns a deep clone of the primary input VICAR label (set via either
 * method).  This should be used only in special cases; normally the main
 * <code>getVicarLabel()</code> should be used instead.  Returns null
 * if the PI or label doesn't exist; a default is not returned.
 * @see #setPrimaryInput(VicarInput)
 * @see #setPrimaryInput(SystemLabel, VicarLabel)
 * @see #getVicarLabel()
 */
    public synchronized VicarLabel getPrimaryInputVicarLabel()
							throws IOException
    {
	if (_primary_input != null)
	    return _primary_input.getVicarLabel();
	if (_primary_label != null)
	    return (VicarLabel)_primary_label.clone();
	return null;
    }

////////////////////////////////////////////////////////////////////////

/***********************************************************************
 * Returns a deep copy of the <code>VicarLabel</code> object for this file.
 * This routine should <em>not</em> be used to retrieve the system label
 * bean; use <code>getSystemLabel()</code> instead.
 * <p>
 * This routine returns the first item from the following that is set:
 * <ul>
 * <li>Label set via <code>setVicarLabel</code>
 * <li>Primary input's label
 * <li>Empty (newly constructed) label
 * </ul>
 * <p>
 * @see #getSystemLabel()
 */
    public synchronized VicarLabel getVicarLabel() throws IOException
    {
	if (_label != null)
	    return (VicarLabel)_label.clone();

	// label can't be null if the file's not open, so we can set it here

	_label = getPrimaryInputVicarLabel();
	if (_label != null)
	    return (VicarLabel)_label.clone();

	_label = new VicarLabel();
	return (VicarLabel)_label.clone();
    }

/***********************************************************************
 * Indicates whether or not the <code>VicarLabel</code> has been completely
 * read.  Always true for output files.
 * @see #getVicarLabel()
 */
    public synchronized boolean isLabelComplete()
    {
	return true;
    }

/***********************************************************************
 * Sets the VICAR label for this image.  The provided <code>VicarLabel</code>
 * object contains the property and history labels that are to be written
 * to the file.  Note that the system label portion of <code>VicarLabel</code>
 * is ignored; in fact, the system label portion (only!) might be modified
 * by this routine to reflect the current system label (no guarantees one
 * way or the other).
 * <p>
 * Unlike <code>setSystemLabel()</code>, this routine can be called at any
 * time after the system label is set, assuming the file is random-access.
 * You can call this routine multiple times in order to update the label, as
 * well.  For a sequential-only file, however, this routine must only be
 * called before the file is opened.
 * <p>
 * The VICAR label is generally obtained first via a call to
 * <code>getVicarLabel</code> on this (or another) image.  It may then
 * be modified before submitting it back via this routine.
 * <p>
 * Before the label is set, a call to <code>getVicarLabel</code> will
 * return the existing label if the file already exists.  If not, the
 * primary input's label is returned.  If that hasn't been set, an empty
 * VICAR label is returned.  Once the label is set via this routine,
 * future get calls will return the current label.
 * <p>
 * It is up to the application to add a VICAR history task if desired.
 * Use <code>VicarLabel.createHistoryTask()</code>.
 * @see VicarIOBase#getVicarLabel
 * @see VicarLabel#createHistoryTask
 * @throws IOException
 */
    public synchronized void setVicarLabel(VicarLabel vl) throws IOException
    {
	if (_file_open && !_random_allowed) {
	    throw new NonSequentialAccessException(
	       "Can't write VICAR labels to a sequential file after it's open");
	}

	_label = (VicarLabel)vl.clone();

	if (_file_open) {		// write the new label out
	    writeLabelInternal();
	}
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
 * <p>
 * This routine returns the first item from the following that is set:
 * <ul>
 * <li>Label set via <code>setSystemLabel</code>
 * <li>Primary input's label
 * <li>Default (newly constructed) label
 * </ul>
 */
    public synchronized SystemLabel getSystemLabel()
    {
	if (_system != null)
	    return (SystemLabel)_system.clone();

	// label can't be null if the file's not open, so we can set it here

	try {
	    _system = getPrimaryInputSystemLabel();
	} catch (Exception e) {
	    _system = null;
	}
	if (_system != null)
	    return (SystemLabel)_system.clone();

	_system = new SystemLabel();
	return (SystemLabel)_system.clone();
    }

/***********************************************************************
 * Sets the system label of this image.  The system label determines
 * image size, data type, pixel format, etc.
 * <p>
 * <em>This routine must be called before <code>open()</code>.</em>
 * Once the file has been opened, the system label cannot be modified, with
 * one exception:  The n3 dimension (or n2 iff n3==1) can be set to 0, meaning
 * that it is unknown at open time.  The file can thus expand as lines are
 * written to whatever size is desired.  When the file is closed, the size
 * will be set to the maximum record written.  This is only possible with
 * random-access output streams, and is generally discouraged in any case.
 * Note that files expandable in the Band dimension cannot be used with
 * <code>createSampleModel</code> since the # of bands must be known in
 * order to create a sample model.
 * <p>
 * The system label is generally obtained first via a call to
 * <code>getSystemLabel</code> on this (or another) image.  It may then
 * be modified before submitting it back via this routine.
 * <p>
 * Before the label is set, a call to <code>getSystemLabel</code> will
 * return the existing label if the file already exists.  If not, the
 * primary input's label is returned.  If that hasn't been set, a default
 * system label is returned (which is not very useful; the size at least
 * should be set).  Once the label is set via this routine, that's what will
 * be returned by all further calls to the get routine.
 * <p>
 * Attempts to set the label on an already-open file will throw an exception.
 * <p>
 * @throws AlreadyOpenException if the label has already been set
 * @see VicarIOBase#getSystemLabel
 */
    public synchronized void setSystemLabel(SystemLabel sl)
					throws AlreadyOpenException
    {
	if (_file_open) {
	    throw new AlreadyOpenException(
	       "Cannot set system label of an open file");
	}

	_system = (SystemLabel)sl.clone();
    }

    
    public void setDebug (boolean f) {
    	_debug = f;
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
 * Low-level write function, writes a byte array to the given record.
 * The most normal call, to write an entire record, would be
 * <code>x.writeRecord(data, 0, 0, 0, line, band);</code>
 * @param data The array to write from
 * @param start Starting position in the record.  Normally 0 to write the whole
 *  record.
 * @param length Number of pixels to write.  If 0, the rest of the record is
 *  written.
 * @param offset Offset into the data array at which to get the data.
 * @param pixelStride Offset from one pixel to the next in the array.  For
 *  example, a stride of 1 has all pixels contiguous while a stride of 3
 *  allows band interleaved pixels in the array (pixels read from the file
 *  will be put 3 elements apart in the array).  This has nothing to do with
 *  what is read from the file - only where the data is put in the array.
 *  Note that this value is measured in pixels or array elements - not bytes.
 * @param n2 "Line number" to write (for BSQ organization).  (For BIL: band
 *  number, for BIP: sample number).  The second dimension's index.
 * @param n3 "Band number" to write (for BSQ organization).  (For BIL or BIP:
 *  line number).  The third dimension's index.
 * @throws NonSequentialAccessException (subclass of IOException)
 * @throws IOException
 */
    public synchronized void writeRecord(byte[] data,
			int start, int length, int offset, int pixelStride,
			int n2, int n3) throws IOException
    {
	writeRecordNS(data, start, length, offset, pixelStride, n2, n3);
    }

/** Non-synchronized version */
    protected void writeRecordNS(byte[] data,
			int start, int length, int offset, int pixelStride,
			int n2, int n3) throws IOException
    {
	long pos = calcFilePos(start, n2, n3);
	seekToLocation(pos);
	if (length == 0)
	    length = _system.getN1() - start;
	if (_system.getFormatCode() == SystemLabel.TYPE_BYTE) {
	    _output_stream_wrap.writeBytes(data, offset, length, pixelStride);
	    _current_file_pos += length * _system.getPixelSize();
	}
	else 		//!!!! OTHER CASES: MAYBE DONE IN WRAPPER????!!!!
	    throw new UnsupportedOperationException("Data type conversions not implemented yet!!!!");
    }

/***********************************************************************
 * Low-level write function, writes a short array to the given record.
 * Follow the See Also link for parameters
 * @see #writeRecord(byte[], int, int, int, int, int, int)
 */
    public synchronized void writeRecord(short[] data,
			int start, int length, int offset,
			int pixelStride, int n2, int n3) throws IOException
    {
	writeRecordNS(data, start, length, offset, pixelStride, n2, n3);
    }

/** Non-synchronized version */
    protected void writeRecordNS(short[] data,
			int start, int length, int offset,
			int pixelStride, int n2, int n3) throws IOException
    {
	long pos = calcFilePos(start, n2, n3);
	
	seekToLocation(pos);
    
	if (length == 0)
	    length = _system.getN1() - start;
	if (_system.getFormatCode() == SystemLabel.TYPE_HALF) {
	    _output_stream_wrap.writeShorts(data, offset, length, pixelStride);
	    _current_file_pos += length * _system.getPixelSize();
	}
	else if (_system.getFormatCode() == SystemLabel.TYPE_USHORT) {
	    _output_stream_wrap.writeUShorts(data, offset, length, pixelStride);
	    _current_file_pos += length * _system.getPixelSize();
	}
	else 		//!!!! OTHER CASES: MAYBE DONE IN WRAPPER????!!!!
	    throw new UnsupportedOperationException("Data type conversions not implemented yet!!!!");
    }

/***********************************************************************
 * Low-level write function, writes an unsigned short array to the given
 * record.  Although unsigned short is not a VICAR data type, it is useful
 * for some other formats which use this library.  Note that a short array
 * must be used, although the data should be interpreted as unsigned.
 * For this reason, a different method name is needed.
 * <p>
 * Follow the See Also link for parameters
 * @see #writeRecord(byte[], int, int, int, int, int, int)
 */
    public synchronized void writeRecordUshort(short[] data,
			int start, int length, int offset,
			int pixelStride, int n2, int n3) throws IOException
    {
	writeRecordUshortNS(data, start, length, offset, pixelStride, n2, n3);
    }

/** Non-synchronized version */
    protected void writeRecordUshortNS(short[] data,
			int start, int length, int offset,
			int pixelStride, int n2, int n3) throws IOException
    {
	long pos = calcFilePos(start, n2, n3);
	seekToLocation(pos);
	if (length == 0)
	    length = _system.getN1() - start;
	if (_system.getFormatCode() == SystemLabel.TYPE_HALF) {
	    _output_stream_wrap.writeShorts(data, offset, length, pixelStride);
	    _current_file_pos += length * _system.getPixelSize();
	}
	else if (_system.getFormatCode() == SystemLabel.TYPE_USHORT) {
	    _output_stream_wrap.writeUShorts(data, offset, length, pixelStride);
	    _current_file_pos += length * _system.getPixelSize();
	}
	else 		//!!!! OTHER CASES: MAYBE DONE IN WRAPPER????!!!!
	    throw new UnsupportedOperationException("Data type conversions not implemented yet!!!!");
    }
/***********************************************************************
 * Low-level write function, writes an int array to the given record.
 * Follow the See Also link for parameters
 * @see #writeRecord(byte[], int, int, int, int, int, int)
 */
    public synchronized void writeRecord(int[] data,
			int start, int length, int offset,
			int pixelStride, int n2, int n3) throws IOException
    {
	writeRecordNS(data, start, length, offset, pixelStride, n2, n3);
    }

/** Non-synchronized version */
    protected void writeRecordNS(int[] data,
			int start, int length, int offset,
			int pixelStride, int n2, int n3) throws IOException
    {
	long pos = calcFilePos(start, n2, n3);
	seekToLocation(pos);
	if (length == 0)
	    length = _system.getN1() - start;
	if (_system.getFormatCode() == SystemLabel.TYPE_FULL) {
	    _output_stream_wrap.writeInts(data, offset, length, pixelStride);
	    _current_file_pos += length * _system.getPixelSize();
	}
	else		//!!!! OTHER CASES: MAYBE DONE IN WRAPPER????!!!!
	    throw new UnsupportedOperationException("Data type conversions not implemented yet!!!!");
    }

/***********************************************************************
 * Low-level write function, writes a float array to the given record.
 * Follow the See Also link for parameters
 * @see #writeRecord(byte[], int, int, int, int, int, int)
 */
    public synchronized void writeRecord(float[] data,
			int start, int length, int offset,
			int pixelStride, int n2, int n3) throws IOException
    {
	writeRecordNS(data, start, length, offset, pixelStride, n2, n3);
    }

/** Non-synchronized version */
    protected void writeRecordNS(float[] data,
			int start, int length, int offset,
			int pixelStride, int n2, int n3) throws IOException
    {
	long pos = calcFilePos(start, n2, n3);
	seekToLocation(pos);
	if (length == 0)
	    length = _system.getN1() - start;
	if (_system.getFormatCode() == SystemLabel.TYPE_REAL) {
	    _output_stream_wrap.writeFloats(data, offset, length, pixelStride);
	    _current_file_pos += length * _system.getPixelSize();
	}
	else 		//!!!! OTHER CASES: MAYBE DONE IN WRAPPER????!!!!
	    throw new UnsupportedOperationException("Data type conversions not implemented yet!!!!");
    }

/***********************************************************************
 * Low-level write function, writes a double array to the given record.
 * Follow the See Also link for parameters
 * @see #writeRecord(byte[], int, int, int, int, int, int)
 */
    public synchronized void writeRecord(double[] data,
			int start, int length, int offset,
			int pixelStride, int n2, int n3) throws IOException
    {
	writeRecordNS(data, start, length, offset, pixelStride, n2, n3);
    }

/** Non-synchronized version */
    protected void writeRecordNS(double[] data,
			int start, int length, int offset,
			int pixelStride, int n2, int n3) throws IOException
    {
	long pos = calcFilePos(start, n2, n3);
	seekToLocation(pos);
	if (length == 0)
	    length = _system.getN1() - start;
	if (_system.getFormatCode() == SystemLabel.TYPE_DOUB) {
	    _output_stream_wrap.writeDoubles(data, offset, length, pixelStride);
	    _current_file_pos += length * _system.getPixelSize();
	}
	else 		//!!!! OTHER CASES: MAYBE DONE IN WRAPPER????!!!!
	    throw new UnsupportedOperationException("Data type conversions not implemented yet!!!!");
    }

/***********************************************************************
 * Low-level write function, writes complex data from a float array for the
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
 * @see #writeRecord(byte[], int, int, int, int, int, int)
 */
    public synchronized void writeRecordComp(float[] data,
			int start, int length, int offset,
			int pixelStride, int n2, int n3) throws IOException
    {
	writeRecordCompNS(data, start, length, offset, pixelStride, n2, n3);
    }

/** Non-synchronized version */
    protected void writeRecordCompNS(float[] data,
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
	    for (int i=0; i<length; i++) {			// simple write
		_output_stream_wrap.writeFloats(data, offset+i*pixelStride*2,
							1, 1);		// real
		_output_stream_wrap.writeFloats(data, offset+i*pixelStride*2+1,
							1, 1);		// imag
	    }
	    _current_file_pos += length * _system.getPixelSize();
	}
	else 		//!!!! OTHER CASES: MAYBE DONE IN WRAPPER????!!!!
	    throw new UnsupportedOperationException("Data type conversions not implemented yet!!!!");
    }

////////////////////////////////////////////////////////////////////////

/***********************************************************************
 * Creates an empty binary header object for this image.  The application
 * is responsible for filling up the object, and for writing it out via
 * <code>setBinaryHeader()</code>.  The returned <code>VicarBinaryLabel</code>
 * will be of the proper size, with the data format set to the BINTFMT and
 * BREALFMT of this image.  The object should be filled with the stream
 * returned by <code>VicarBinaryLabel.getDataOutput()</code> (although
 * direct access to the buffer is possible, it is not recommended).  Note
 * that this routine is just a factory; nothing is written to the image.
 * @see VicarBinaryLabel
 */
    public synchronized VicarBinaryLabel createBinaryHeader()
    {
	if (_binary_data_format == null)
	    _binary_data_format = new VicarDataFormat(_system.getBHost(),
				_system.getBIntFmt(), _system.getBRealFmt());

	int size = _system.getNLB() * _system.getRecsize();

	VicarBinaryLabel vbl = new VicarBinaryLabel(size, _binary_data_format);

	return vbl;
    }

/***********************************************************************
 * Creates an empty binary prefix object for this image.  The application
 * is responsible for filling up the object, and for writing it out via
 * <code>setBinaryPrefix()</code>.  The returned <code>VicarBinaryLabel</code>
 * will be of the proper size, with the data format set to the BINTFMT and
 * BREALFMT of this image.  Note that the line number is not required because
 * all prefixes are the same size for a given file; you provide the line
 * number when writing.  The object should be filled with the stream
 * returned by <code>VicarBinaryLabel.getDataOutput()</code> (although
 * direct access to the buffer is possible, it is not recommended).  Note
 * that this routine is just a factory; nothing is written to the image.
 * @see VicarBinaryLabel
 */
    public synchronized VicarBinaryLabel createBinaryPrefix()
    {
	if (_binary_data_format == null)
	    _binary_data_format = new VicarDataFormat(_system.getBHost(),
				_system.getBIntFmt(), _system.getBRealFmt());

	int size = _system.getNBB();

	VicarBinaryLabel vbl = new VicarBinaryLabel(size, _binary_data_format);

	return vbl;
    }

/***********************************************************************
 * Writes the binary header to the file, which is NLB records at the
 * beginning of the file.  The supplied <code>VicarBinaryLabel</code>
 * must already be filled in, and its data format must match the BINTFMT/
 * BREALFMT of this image, or a <code>BinaryFormatMismatchException</code>
 * is thrown.  The size must also match (NLB * the record size), or a
 * <code>BinarySizeMismatchException</code> is thrown.
 * <p>
 * The supplied <code>VicarBinaryLabel</code> may come from a call to
 * <code>createBinaryHeader</code>, or it may come from a call to another
 * image's <code>getBinaryHeader</code> (or even this image, if it is both
 * read and write).  Be careful using a get, though - if the binary data
 * format of the source doesn't match this file, an exception will be
 * generated.  If you wish to copy a binary label unchanged, you must set
 * the BINTFMT/BREALFMT of the output image to match the input you are copying
 * from, in order to avoid this exception.
 * <p>
 * If this routine is used with a strictly sequential file, it must be called
 * before any pixel data (or binary prefixes) are written.
 * <p>
 * A typical binary header might be written like this:
 * <pre>
 *   VicarBinaryLabel bh = file.createBinaryHeader();
 *   DataOutput dout = bh.getDataOutput();
 *   int first = ...;
 *   dout.writeInt(first);
 *   short second = ...;
 *   dout.writeShort(second);
 *   String third = ...;
 *   byte b[] = third.getBytes();
 *   for (i=0; i < b.length; i++) dout.writeByte(b[i]);  // or dout.write(b);
 *   ... etc ...
 *   file.setBinaryHeader(bh);
 * </pre>
 * <p>
 * TBD:  Should VBL's be in the metadata somehow?  i.e. should they
 * automatically carry across to outputs?  Or do apps specifically need
 * to transfer them?
 *
 * @param vbl <code>VicarBinaryLabel</code> containing the data to write.
 * @throws BinaryFormatMismatchException if format doesn't match
 *  BINTFMT/BREALFMT of the image (subclass of IOException)
 * @throws BinarySizeMismatchException if size doesn't match NLB*recsize of
 *  image (subclass of IOException)
 * @throws NonSequentialAccessException (subclass of IOException)
 * @throws IOException if the file is not open, or other error occurs
 *
 * @see VicarBinaryLabel
 * @see #createBinaryHeader
 * @see VicarInput#getBinaryHeader
 */
    public void setBinaryHeader(VicarBinaryLabel vbl)
                throws IOException, NonSequentialAccessException,
                BinaryFormatMismatchException, BinarySizeMismatchException
    {
	if (_binary_data_format == null)
	    _binary_data_format = new VicarDataFormat(_system.getBHost(),
				_system.getBIntFmt(), _system.getBRealFmt());

	if (vbl.size() != _system.getNLB() * _system.getRecsize()) {
	    throw new BinarySizeMismatchException(
"Attempt to write binary headers of incorrect size for this image: size="
		+ vbl.size() + ", image NLB=" +
		(_system.getNLB() * _system.getRecsize()));
	}
	if (!_binary_data_format.equals(vbl.getVicarDataFormat())) {
	    throw new BinaryFormatMismatchException(
"Attempt to write binary headers of incorrect type for this image: type="
		+ vbl.getVicarDataFormat() + ", image=" + _binary_data_format);
	}

	long pos = _lblsize_front;	// header starts right after labels
	seekToLocation(pos);

	int size = vbl.size();

	// Write the entire header

	stream_write(vbl.getBuffer(), 0, size);

	_current_file_pos += size;
    }
    
    /**
     * sets the VicarBinaryLinePrefix data Object
     * This data is used to hold any binary prefix data for each line
     * The image tiles only retrieve "image" data. Any binary pefix is skipped
     * when a tile is read.
     * This allows a reader or other process to add the binary prefix back into 
     * the image being written out to a file.
     */
    public void setVicarBinaryLinePrefix(VicarBinaryLinePrefix vicarBinaryLinePrefix) {
    	if (_debug) {   	
		  System.out.println("++++++++++++++++++++++++++++++++++++++++");
		  System.out.println("++++++++++++++++++++++++++++++++++++++++");
    	  System.out.println("VicarOutputFile.setVicarBinaryLinePrefix "+vicarBinaryLinePrefix);
		  System.out.println("++++++++++++++++++++++++++++++++++++++++");
    	}
		_vicarBinaryLinePrefix = vicarBinaryLinePrefix;
    }

/***********************************************************************
 * Writes a binary prefix to the file, which is NBB <em>bytes</em> at
 * the beginning of each record.  The supplied <code>VicarBinaryLabel</code>
 * must already be filled in, and its data format must match the BINTFMT/
 * BREALFMT of this image, or a <code>BinaryFormatMismatchException</code>
 * is thrown.  The size must also match (NBB), or a
 * <code>BinarySizeMismatchException</code> is thrown.
 * <p>
 * The supplied <code>VicarBinaryLabel</code> may come from a call to
 * <code>createBinaryPrefix</code>, or it may come from a call to another
 * image's <code>getBinaryPrefix</code> (or even this image, if it is both
 * read and write).  Be careful using a get, though - if the binary data
 * format of the source doesn't match this file, an exception will be
 * generated.  If you wish to copy a binary label unchanged, you must set
 * the BINTFMT/BREALFMT of the output image to match the input you are copying
 * from, in order to avoid this exception.
 * <p>
 * If this routine is used with a strictly sequential file, it must be called
 * before the pixel data for this record is written (and of course, after the
 * previous record's data).  For this reason, this routine is incompatible
 * with the Tile write functions for strictly sequential files (unless the
 * tile height is 1).
 * <p>
 * See <code>getBinaryHeader()</code> for an example of usage.
 *
 * @param vbl <code>VicarBinaryLabel</code> containing the data to write.
 * @param n2 "Line number" to write (for BSQ organization).  (For BIL: band
 *  number, for BIP: sample number).  The second dimension's index.
 * @param n3 "Band number" to write (for BSQ organization).  (For BIL or BIP:
 *  line number).  The third dimension's index.
 * @throws BinaryFormatMismatchException if format doesn't match
 *  BINTFMT/BREALFMT of the image (subclass of IOException)
 * @throws BinarySizeMismatchException if size doesn't match NBB of the
 *  image (subclass of IOException)
 * @throws NonSequentialAccessException (subclass of IOException)
 * @throws IOException if the file is not open, or other error occurs
 *
 * @see VicarBinaryLabel
 * @see #createBinaryPrefix
 * @see VicarInput#getBinaryPrefix
 * @see #setBinaryHeader
 */
    public void setBinaryPrefix(VicarBinaryLabel vbl, int n2, int n3)
                throws IOException, NonSequentialAccessException,
                BinaryFormatMismatchException, BinarySizeMismatchException
    {
	if (_binary_data_format == null)
	    _binary_data_format = new VicarDataFormat(_system.getBHost(),
				_system.getBIntFmt(), _system.getBRealFmt());

	if (vbl.size() != _system.getNBB()) {
	    throw new BinarySizeMismatchException(
"Attempt to write binary prefixes of incorrect size for this image: size="
		+ vbl.size() + ", image NBB=" + _system.getNBB());
	}
	if (!_binary_data_format.equals(vbl.getVicarDataFormat())) {
	    throw new BinaryFormatMismatchException(
"Attempt to write binary prefixes of incorrect type for this image: type="
		+ vbl.getVicarDataFormat() + ", image=" + _binary_data_format);
	}

	long pos = calcFilePos(0, n2, n3);
	pos -= _system.getNBB();	// calcFilePos skips past prefix
	seekToLocation(pos);

	int size = vbl.size();

	// Write the entire header

	stream_write(vbl.getBuffer(), 0, size);

	_current_file_pos += size;
    }

////////////////////////////////////////////////////////////////////////

/***********************************************************************
 * Creates a SampleModel that is most compatible with the output image.
 * Follow the See Also link for complete description
 * @throws IllegalArgumentException if the number of bands is left
 * expandable (i.e. 0).
 * @see VicarIOBase#createSampleModel()
 */
    public SampleModel createSampleModel()
    {
	int width, height;

	// width = VICAR_TILE_WIDTH;
	width = _system.getNS(); // use full width, should be more efficient
	
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
 * Creates a SampleModel that is most compatible with the output image.
 * Follow the See Also link for complete description
 * @throws IllegalArgumentException if the number of bands is left
 * expandable (i.e. 0).
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

	if (num_bands == 0)
	    throw new IllegalArgumentException(
"createSampleModel cannot be used if the Bands dimension is expandable (0)");

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
	// using the standard samplem odels.  So if we're not in 1.3, simply
	// reset the flag and use the standard sample models.

	// Except that PixelAccessor in JAI 1.1.1 doesn't work with 1.4's
	// DataBufferFloat/Double... only JAI's.  1.1.2 is better.

	if ( ! VicarInputFile.jdk_version_1_3 &&
	     ! VicarInputFile.jai_version_1_1_1)
	    float_type = false;

	// Compute offsets, etc... then create the SM

	switch (_system.getOrgCode()) {
	    case SystemLabel.ORG_BSQ:
		// One bank per band of data
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

		if (num_bands == 1)
		    return new PixelInterleavedSampleModel(data_buffer_type,
				tileWidth, tileHeight,
				pixel_stride, scanline_stride,
				band_offsets);
		else
		    return new BandedSampleModel(data_buffer_type,
				tileWidth, tileHeight,
				scanline_stride,
				bank_indices, band_offsets);

	    case SystemLabel.ORG_BIL:
		// One bank for all bands of data
		pixel_stride = 1;
		scanline_stride = tileWidth * num_bands;
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


/**
 * If there is line prefix data this method will write it to the file.
 * It first checks to see that we are at the start of the line and 
 * there is prefix data available.
 */
	public void writeLinePrefix(int x, int y, int line, int band) {
		int nbb = _system.getNBB();
		
		try {
		
		if (x == 0 && nbb != 0 && _vicarBinaryLinePrefix != null) {
						
			long pos = calcFilePos(x, line+y, band);
			pos -= nbb;
			if (_debug) {
			
			 System.out.println("VicarOutputFile.writeTile writing prefix x="+x+" line="+line+" pos="+pos+ " nbb="+nbb);
			}
			seekToLocation(pos);
						
			byte[] prefix = _vicarBinaryLinePrefix.getPrefix(band, (line+y));
			byte b;
			
			if (_debug) {
			
			    int start = (line + y) * (band + 1) * nbb ; 
			
						if (((line+ y) % 500 == 1) ||
							((line+ y) % 500 == 2) ||
							((line+ y) % 500 == 3) ) {
			
						  System.out.print("WRITE "+band+" "+line+" "+y+" "+(line+y)+" "+start+" "+nbb+" >");
						  for (int i =0 ; i< nbb ; i++) {
							b = prefix[i] ;
							Byte by = new Byte(b);
							System.out.print(" "+Byte.toString(b));
							}
						  System.out.println(" <*");
						}
			}	  
			
			
			_output_stream_wrap.writeBytes(prefix,0, nbb, 1);
			_current_file_pos += nbb ;
			} 
		}
		catch( IOException ioe) {
			System.out.println("IOException "+ioe);
			// rethrow ??
		}
}
/***********************************************************************
 * High-level write function.
 * Follow the See Also link for complete description
 * @see VicarOutput#writeTile(int,int,SampleModel,DataBuffer)
 */
    public void writeTile(int x, int y, SampleModel m, DataBuffer b)
			throws IOException
    {
	writeTile(x, y, m.getWidth(), m.getHeight(), 0, 0, null, m, b);
    }

/***********************************************************************
 * High-level write function.
 * Follow the See Also link for complete description
 * @see VicarOutput#writeTile(int,int,int,int,int,int,SampleModel,DataBuffer)
 */
    public void writeTile(int x, int y, int w, int h, int x_off, int y_off,
			SampleModel m, DataBuffer b)
			throws IOException
    {
	writeTile(x, y, w, h, x_off, y_off, null, m, b);
    }

/***********************************************************************
 * High-level write function.
 * Follow the See Also link for complete description
 * @see VicarOutput#writeTile(int,int,int,int,int,int,int[],SampleModel,DataBuffer)
 */
    public synchronized void writeTile(int x, int y, int w, int h,
			int x_off, int y_off,
			int bandList[],
			SampleModel sm, DataBuffer db)
			throws IOException
    {
	int i;
	int buffer_needed;
	int line, samp;
	int band = 0; 
	int band_ind;
	int offset = 0;
	boolean bandListGiven;
	// Array pointers that are filled in from UnpackedImageData.getXxxData()
	byte bdata[], bbdata[][];
	short sdata[], ssdata[][];
	int idata[], iidata[][];
	float fdata[], ffdata[][];
	double ddata[], dddata[][];

	int num_bands = sm.getNumBands();
	if (bandList != null && num_bands > bandList.length)
	    num_bands = bandList.length;

	bandListGiven = (bandList != null);	// flag if it was given
	if (!bandListGiven) {			// make one for convenience
	    bandList = new int[num_bands];
	    for (i=0; i<num_bands; i++)
		bandList[i] = i;
	}

	if (x + w > _system.getNS())		// last tile may be incomplete
	    w = _system.getNS() - x;
	if (y + h > _system.getNL() && _system.getNL() != 0)
	    h = _system.getNL() - y;
	    
	 //   System.out.println("VicarOutputFile.writeTile x="+x+" y="+y+" w="+w+" sm.w="+sm.getWidth());

	if (x_off + w > sm.getWidth())
	    throw new ArrayIndexOutOfBoundsException(
		"Illegal width in VICAR writeTile: " + w + ", x_off=" + x_off +
			", width=" + sm.getWidth());
	if (y_off + h > sm.getHeight())
	    throw new ArrayIndexOutOfBoundsException(
		"Illegal height in VICAR writeTile: " + h + ", y_off=" + y_off +
			", height=" + sm.getHeight());

	PixelAccessor pa = new PixelAccessor(sm, null);
	Rectangle area = new Rectangle(x_off, y_off, w, h);
	UnpackedImageData data = pa.getPixels(
			Raster.createRaster(sm,db,null),
			area, db.getDataType(), false);

	int data_type = _system.getFormatCode();
	int org_code = _system.getOrgCode();
	int nbb = _system.getNBB();
	int recsize = _system.getRecsize();
	int ns = _system.getNS();
	int nb = _system.getNB();
	
	try {
	

	switch (org_code) {

	    case SystemLabel.ORG_BSQ:

		for (band_ind=0; band_ind < num_bands; band_ind++) {
		    band = bandList[band_ind];

		    switch (data_type) {
			case SystemLabel.TYPE_BYTE:
			    bdata = data.getByteData(band);
			    offset = data.getOffset(band);
			    for (line=0; line < h; line++) {
					writeLinePrefix(x, y, line, band) ;
				writeRecordNS(bdata, x, w, offset,
					data.pixelStride, line+y, band);
				offset += data.lineStride;
			    }
			    break;
			case SystemLabel.TYPE_HALF:
			    sdata = data.getShortData(band);
			    offset = data.getOffset(band);
			    if (_debug) {			    
				System.out.println("VicarOutputFile.writeTile x="+x+" y="+y+" w="+w+" TYPE_HALF nbb="+nbb);
				System.out.println("  _vicarBinaryLinePrefix "+ _vicarBinaryLinePrefix );				
				System.out.println("  recsize="+recsize+" nb="+nb+" ns="+ns+" nbb=" +nbb);
		    	}
				
			    for (line=0; line < h; line++) {
			    	
					// public void writeLinePrefix(int x, int y, int line, int band) 
					writeLinePrefix(x, y, line, band) ;
								    	
			    // protected void writeRecordNS(short[] data,
				// int start, int length, int offset,int pixelStride, int n2, int n3)				    	
				writeRecordNS(sdata, x, w, offset, data.pixelStride, line+y, band);
				offset += data.lineStride;
			    }
			    break;
			case SystemLabel.TYPE_USHORT:
			    sdata = data.getShortData(band);
			    offset = data.getOffset(band);
				if (_debug) {
					System.out.println("VicarOutputFile.writeTile x="+x+" y="+y+" w="+w+" TYPE_USHORT");
				}
			    for (line=0; line < h; line++) {
					writeLinePrefix(x, y, line, band) ;
				writeRecordUshortNS(sdata, x, w, offset,
					data.pixelStride, line+y, band);
				offset += data.lineStride;
			    }
			    break;
			case SystemLabel.TYPE_FULL:
			    idata = data.getIntData(band);
			    offset = data.getOffset(band);
			    for (line=0; line < h; line++) {
					writeLinePrefix(x, y, line, band) ;
				writeRecordNS(idata, x, w, offset,
					data.pixelStride, line+y, band);
				offset += data.lineStride;
			    }
			    break;
			case SystemLabel.TYPE_REAL:
			    fdata = data.getFloatData(band);
			    offset = data.getOffset(band);
			    for (line=0; line < h; line++) {
					writeLinePrefix(x, y, line, band) ;
				writeRecordNS(fdata, x, w, offset,
					data.pixelStride, line+y, band);
				offset += data.lineStride;
			    }
			    break;
			case SystemLabel.TYPE_DOUB:
			    ddata = data.getDoubleData(band);
			    offset = data.getOffset(band);
			    for (line=0; line < h; line++) {
					writeLinePrefix(x, y, line, band) ;
				writeRecordNS(ddata, x, w, offset,
					data.pixelStride, line+y, band);
				offset += data.lineStride;
			    }
			    break;
			case SystemLabel.TYPE_COMP:
			    throw new UnsupportedOperationException("writeTile() for Complex data not implemented yet!");
/*!!!!
			    // "band" is file_nb*2, where evens are real and
			    // odds are imaginary.  "band_ind" is a simple
			    // index into the output band array.
			    for (line=0; line < h; line++) {
				int c = band % 2;	// 0=real, 1=imag
				for (i=0; i < w; i++) {
				    _float_buffer[i+c] = sm.getSample(
					x_off+i, y_off+line, band_ind, db);
				}
				writeRecordCompNS(_float_buffer, x, w, 0,line+y,
						band/2);
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
				writeLinePrefix(x, y, line, band) ;
				writeRecordNS(bdata, x, w, offset,
					data.pixelStride, band, line+y);
			    }
			    break;
			case SystemLabel.TYPE_HALF:
			    for (band_ind=0; band_ind < num_bands; band_ind++) {
				band = bandList[band_ind];
				sdata = data.getShortData(band);
				offset = data.getOffset(band) +
							line * data.lineStride;
				writeLinePrefix(x, y, line, band) ;
				writeRecordNS(sdata, x, w, offset,
					data.pixelStride, band, line+y);
			    }
			    break;
			case SystemLabel.TYPE_USHORT:
			    for (band_ind=0; band_ind < num_bands; band_ind++) {
				band = bandList[band_ind];
				sdata = data.getShortData(band);
				offset = data.getOffset(band) +
							line * data.lineStride;
					writeLinePrefix(x, y, line, band) ;
					writeRecordUshortNS(sdata, x, w, offset,
					data.pixelStride, band, line+y);
			    }
			    break;
			case SystemLabel.TYPE_FULL:
			    for (band_ind=0; band_ind < num_bands; band_ind++) {
				band = bandList[band_ind];
				idata = data.getIntData(band);
				offset = data.getOffset(band) +
							line * data.lineStride;
				writeLinePrefix(x, y, line, band) ;
				writeRecordNS(idata, x, w, offset,
					data.pixelStride, band, line+y);
			    }
			    break;
			case SystemLabel.TYPE_REAL:
			    for (band_ind=0; band_ind < num_bands; band_ind++) {
				band = bandList[band_ind];
				fdata = data.getFloatData(band);
				offset = data.getOffset(band) +
							line * data.lineStride;
				writeLinePrefix(x, y, line, band) ;
				writeRecordNS(fdata, x, w, offset,
					data.pixelStride, band, line+y);
			    }
			    break;
			case SystemLabel.TYPE_DOUB:
			    for (band_ind=0; band_ind < num_bands; band_ind++) {
				band = bandList[band_ind];
				ddata = data.getDoubleData(band);
				offset = data.getOffset(band) +
							line * data.lineStride;
				writeLinePrefix(x, y, line, band) ;
				writeRecordNS(ddata, x, w, offset,
					data.pixelStride, band, line+y);
			    }
			    break;
			case SystemLabel.TYPE_COMP:
			    throw new UnsupportedOperationException("writeTile() for Complex data not implemented yet!");
/*!!!!
			    // "band" is file_nb*2, where evens are real and
			    // odds are imaginary.  "band_ind" is a simple
			    // index into the output band array.
			    for (band_ind=0; band_ind < num_bands; band_ind++) {
				if (bandList == null)
				    band = band_ind;
				else
				    band = bandList[band_ind];
				int c = band % 2;	// 0=real, 1=imag
				for (i=0; i < w; i++) {
				    _float_buffer[i+c] = sm.getSample(
					x_off+i, y_off+line, band_ind, db);
				}
				writeRecordCompNS(_float_buffer, x, w, 0,
						band/2, line+y);
			    }
			    break;
!!!!*/
		    }
		}		// end line loop
		break;

	    case SystemLabel.ORG_BIP:

		// BIP is a problem to write because it is quite likely that
		// each band in the SM is in a different bank (array) of data,
		// thus making array-based writes impractical.  That would be
		// okay(ish) if we wanted to skip around in the file, but we
		// want to write data in the same order as in the file (to
		// support sequential-only streams).
		//
		// It's even worse for writes than for reads because with
		// reads, we can transfer the whole pixel then pick and choose
		// the bands to return... but for writes, that only works if
		// we're writing the entire record.  We can't overwrite the
		// other bands without reading the record first.
		//
		// We certainly could optimize the case where we're writing
		// entire record (which is by far the most common case) as
		// we do for read.  But, given the scarcity of BIP images in
		// the first place, this hardly seems worthwhile at the moment.

		// Is band_offsets[] needed?  Not sure HotSpot is good enough
		// to optimize data.getOffset(band_ind) inside the pixel loop...
		int band_offsets[] = new int[num_bands];
		for (band_ind=0; band_ind<num_bands; band_ind++) {
		    band_offsets[band_ind] = data.getOffset(band_ind);
		}
		
		
		/**
		 * There will be no attempt to write line prefix data.
		 * to a BIP file.
		 * In particular I don't know how you might try to interleave
		 * line prefix data. I doublt this will ever come up so I won't 
		 * even try now. 
		 * If someday we have a need we can revisit it.
		 * 
		 */

		switch (data_type) {
		    case SystemLabel.TYPE_BYTE:
			bbdata = data.getByteData();
			for (line=0; line < h; line++) {
			    for (samp=0; samp < w; samp++) {
				for (band_ind=0; band_ind < num_bands;
								band_ind++) {
				    band = bandList[band_ind];
				    writeRecordNS(bbdata[band_ind], band, 1,
						  band_offsets[band_ind] +
						      samp * data.pixelStride +
						      line * data.lineStride,
						  1, samp+x, line+y);
				}
			    }
			}
			break;
		    case SystemLabel.TYPE_HALF:
			ssdata = data.getShortData();
			for (line=0; line < h; line++) {
			    for (samp=0; samp < w; samp++) {
				for (band_ind=0; band_ind < num_bands;
								band_ind++) {
				    band = bandList[band_ind];
				    writeRecordNS(ssdata[band_ind], band, 1,
						  band_offsets[band_ind] +
						      samp * data.pixelStride +
						      line * data.lineStride,
						  1, samp+x, line+y);
				}
			    }
			}
			break;
		    case SystemLabel.TYPE_USHORT:
			ssdata = data.getShortData();
			for (line=0; line < h; line++) {
			    for (samp=0; samp < w; samp++) {
				for (band_ind=0; band_ind < num_bands;
								band_ind++) {
				    band = bandList[band_ind];
				    writeRecordUshortNS(ssdata[band_ind],band,1,
						  band_offsets[band_ind] +
						      samp * data.pixelStride +
						      line * data.lineStride,
						  1, samp+x, line+y);
				}
			    }
			}
			break;
		    case SystemLabel.TYPE_FULL:
			iidata = data.getIntData();
			for (line=0; line < h; line++) {
			    for (samp=0; samp < w; samp++) {
				for (band_ind=0; band_ind < num_bands;
								band_ind++) {
				    band = bandList[band_ind];
				    writeRecordNS(iidata[band_ind], band, 1,
						  band_offsets[band_ind] +
						      samp * data.pixelStride +
						      line * data.lineStride,
						  1, samp+x, line+y);
				}
			    }
			}
			break;
		    case SystemLabel.TYPE_REAL:
			ffdata = data.getFloatData();
			for (line=0; line < h; line++) {
			    for (samp=0; samp < w; samp++) {
				for (band_ind=0; band_ind < num_bands;
								band_ind++) {
				    band = bandList[band_ind];
				    writeRecordNS(ffdata[band_ind], band, 1,
						  band_offsets[band_ind] +
						      samp * data.pixelStride +
						      line * data.lineStride,
						  1, samp+x, line+y);
				}
			    }
			}
			break;
		    case SystemLabel.TYPE_DOUB:
			dddata = data.getDoubleData();
			for (line=0; line < h; line++) {
			    for (samp=0; samp < w; samp++) {
				for (band_ind=0; band_ind < num_bands;
								band_ind++) {
				    band = bandList[band_ind];
				    writeRecordNS(dddata[band_ind], band, 1,
						  band_offsets[band_ind] +
						      samp * data.pixelStride +
						      line * data.lineStride,
						  1, samp+x, line+y);
				}
			    }
			}
			break;
		    case SystemLabel.TYPE_COMP:
			throw new UnsupportedOperationException("COMPLEX BIP data not implemented yet!!!!");
/*!!!!
			    // "band" is file_nb*2, where evens are real and
			    // odds are imaginary.  "band_ind" is a simple
			    // index into the output band array.
			    float fb[] = new float[2];
			    for (samp=0; samp < w; samp++) {
				sm.getPixel(x_off+samp, y_off+line,
							_float_buffer, db);
				for (band_ind=0; band_ind < num_bands; band_ind++) {
				    if (bandList == null)
					band = band_ind;
				    else
					band = bandList[band_ind];

				    fb[band%2] = _float_buffer[band_ind];
				    if (band%2 == 1)
				        writeRecordCompNS(fb, band/2, 1, 0,
								samp+x, line+y);
				}
			    }
			    break;
!!!!*/
		}		// end data type switch
		break;		// end BIP case

	   }			// end org_code switch
	 }
	 catch (IOException ioe) {
	 	System.out.println("IOException VicarOutputFile.writeTile()");
	 	
		System.out.println("x=" +x+" y="+y+"  w="+w+" h="+h);
		System.out.println("band="+band+" org_code="+org_code+" data_type="+data_type);		System.out.println("exception: "+ioe);
		ioe.printStackTrace();
		/**	
		System.out.println("exit with status = 2");
		System.exit(2);
		**/
		// rethrow this exception so users of this library routine can catch and handle it
		throw ioe ;
		
		
	 }
    }

    public long get_current_file_pos() {
    	
    	return _current_file_pos;
    }
////////////////////////////////////////////////////////////////////////

/***********************************************************************
 * Internal routine for calculating the position to seek to given the
 * record address (n2/n3) and the offset within the record.  The binary
 * headers and prefixes are taken into account, i.e. position 0 is the first
 * pixel of the record (past the prefix), and line/band 0 is the first
 * line/band of pixel data (past the headers).
 * <p>
 * If the file is expandable, this routine takes care of expanding it
 * automatically.
 * @throws IOException if start, n2 or n3 exceed the bounds of the image
 */
    protected long calcFilePos(int start, int n2, int n3) throws IOException
    {
	if (start < 0 || start >= _system.getN1())
	    throw new IOException(
		"Attempt to write past edge of image for dimension 1: N1=" +
		_system.getN1() + ", write position=" + start);

	int max_n2 = _system.getN2();
	int max_n3 = _system.getN3();
	if (_expandable) {		// Make this dimension unlimited
	    if (_system.getN2() == 0)		// N2 is expandable
		max_n2 = Integer.MAX_VALUE;
	    else
		max_n3 = Integer.MAX_VALUE;
	}
	if (n2 < 0 || n2 >= max_n2)
	    throw new IOException(
		"Attempt to write past edge of image for dimension 2: N2=" +
		_system.getN2() + ", write position=" + n2);
	if (n3 < 0 || n3 >= max_n3)
	    throw new IOException(
		"Attempt to write past edge of image for dimension 3: N3=" +
		_system.getN3() + ", write position=" + n3);

	if (_expandable) {		// Reset max size
	    if (_system.getN2() == 0) {		// N2 is expandable
		if (n2 > _expandable_size)
		    _expandable_size = n2;
	    }
	    else {
		if (n3 > _expandable_size)
		    _expandable_size = n3;
	    }
	}

	// Note that for n2-expandable files, N3 must be 1 (and thus n3==0)
	// so we don't have to worry about that in the below calculation

	return _lblsize_front +
		(_system.getNLB() + (long)n3 * _system.getN2() + (long)n2)
							* _system.getRecsize() +
		(long)start * _system.getPixelSize() +
		_system.getNBB();
    }

/***********************************************************************
 * Internal routine to seek to a given spot in the file.  This is trivial
 * for random-access files, but a bit tricky for sequential ones.
 * @throws IOException if the file is not open, or other error occurs
 */
    protected void seekToLocation(long pos) throws IOException
    {
	if (!_file_open)
	    throw new IOException("File must be opened before writing to it");

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

	// If not random, we might need to write some null's

	if (!_random_allowed && pos > _current_file_pos) {
	    byte b[] = new byte[1];
	    b[0] = 0;
	    for (int i=0; i < (pos - _current_file_pos); i++)
		_output_stream_wrap.writeBytes(b, 0, 1, 1);
	    _current_file_pos = pos;
	}
    }

/***********************************************************************
 * Internal routine to write the label to the file.  Writes both the initial
 * and EOL labels.  Sequential checks must already have been performed (this
 * can only be called for random files).  This routine does merge in the
 * system label first.
 * <p>
 * Labels are NOT written if the file is expandable.  close() is responsible
 * for calling this routine (with _expandable false) in order to flush the
 * final labels.
 */
    protected void writeLabelInternal() throws IOException
    {
    if (_debug) {
    	System.out.println("writeLabelInternal() A _expandable = "+_expandable);
    	System.out.println("_current_file_pos = "+_current_file_pos);
    }
    	
	if (_expandable)		// do nothing
	    return;

	
	seekToLocation(0);

	// Install the current system label into the VICAR one

	try {
	    _system.writeLabel(_label.getSystem());
	} catch (Exception e) {
	    throw new IOException("Error in System bean processing: " + e);
	}

	// Write the label

	VicarLabel.ItemPos pos = _label.writeLabelChunk(_output_stream,
			_lblsize_front, _system.getRecsize(), null);
	_lblsize_front = pos.lblsize;
	
	if (_debug) {
		System.out.println("writeLabelInternal() A");
		System.out.println("_lblsize_front = " +_lblsize_front);
		System.out.println("_system.getRecsize() = "+_system.getRecsize());
		System.out.println("_current_file_pos = "+_current_file_pos);
	}
	
	if (!pos.isComplete) {
	    seekToLocation(_lblsize_front + _image_size_bytes);
	    if (_debug) {
			System.out.println("writeLabelInternal() A  !pos.isComplete");
			System.out.println("_current_file_pos = "+_current_file_pos);
		}

	    pos = _label.writeLabelChunk(_output_stream,
			0, _system.getRecsize(), pos);
	    if (!pos.isComplete)
		throw new IOException("Error writing VICAR EOL label");
	}
    }

/***********************************************************************
 * Low-level routine to seek on <code>_output_stream/code>.  This function,
 * as well as all the other <code>stream_</code>* functions, is required
 * to handle the plethora of possible output stream objects mandated by the
 * pathetically poor design of <code>java.io</code>.
 * <p>
 * The file offset is not added here; it is assumed that the caller takes
 * care of that already.
 * @throws UnsupportedOperationException if the stream's type is not recognized.
 */
    protected void stream_seek(long pos) throws IOException
    {
	if (_output_stream instanceof RandomAccessFileOutputStream) {
	    ((RandomAccessFileOutputStream)_output_stream).seek(pos);
	    return;
	}

	if (_output_stream instanceof ImageOutputStream) {
	    ((ImageOutputStream)_output_stream).seek(pos);
	    return;
	}

	throw new UnsupportedOperationException("Seek not supported for this stream type");
    }

/***********************************************************************
 * Low-level routine to write an array of bytes to <code>_output_stream</code>.
 * @see #stream_seek
 * @throws UnsupportedOperationException if the stream's type is not recognized.
 */
    protected void stream_write(byte[] b, int off, int len) throws IOException
    {
	if (_output_stream instanceof ImageOutputStream) {
	    ((ImageOutputStream)_output_stream).write(b, off, len);
	    return;
	}

	if (_output_stream instanceof OutputStream) {
	    ((OutputStream)_output_stream).write(b, off, len);
	    return;
	}

	if (_output_stream instanceof DataOutput) {
		((DataOutput)_output_stream).write(b, off, len);
		return;
		}
		
	throw new UnsupportedOperationException("Write not supported for this stream type");
    }

/***********************************************************************
 * Low-level routine to close the <code>_output_stream</code>.
 * @see #stream_seek
 * @throws UnsupportedOperationException if the stream's type is not recognized.
 */
    protected void stream_close() throws IOException
    {
	if (_output_stream instanceof ImageOutputStream) {
	    ((ImageOutputStream)_output_stream).close();
	    return;
	}

	if (_output_stream instanceof OutputStream) {
	    ((OutputStream)_output_stream).close();
	    return;
	}
	
	if (_debug) {
		System.out.println("stream_close()");
	}

	throw new UnsupportedOperationException("Close not supported for this stream type");
    }

}

