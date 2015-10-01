package jpl.mipl.io.vicar;

import java.util.*;
import java.io.*;
import java.lang.reflect.*;
import java.beans.*;
import java.awt.image.*;
import javax.media.jai.*;

/**
 * This class manages a set of VICAR output image files, making the set appear
 * like one image to the outside world.  The primary use is to make a set
 * of three separate files (R,G,B triplet) look like a single image to any
 * callers.
 * <p>
 * Each band in the virtual image presented by this class is mapped to one
 * (or more) bands in the output files.  Each file will have the same size.
 * <p>
 * The <code>VicarLabel</code> object is the one associated with the first
 * file for any Get operations, but the same <code>VicarLabel</code> is written
 * to all output files.  The <code>SystemLabel</code> object, however, reflects
 * the aggregate of all files (thus it indicates the full number of bands
 * in the virtual image).  So, this means that
 * <code>VicarOutputImage.getSystemLabel()</code> does <em>not</em> return
 * the same info as <code>VicarOutputImage.getVicarLabel().getSystem()</code>
 * (besides the return type difference).
 * <p>
 * Unlike <code>VicarInputImage</code>, files may not be offset with respect
 * to one another.  Each output file created will have the same size and data
 * type.
 * <p>
 * File organization of the virtual image is always BSQ, as will be the
 * organization of the outputs.
 * <p>
 * The binary headers are written to <em>every</em> output file (although
 * the Get applies to the first file only.  Binary prefixes are written to
 * the virtual band in question (the given band is mapped to the file/band
 * number).  All prefixes in different files will be the same size.
 * <p>
 * This class allows for multiple threads to simultaneously issue write
 * requests (as <code>VicarOutputFile</code> does; see the description there).
 * Not much synchronization is required here; we rely on the
 * <code>VicarOutputFile</code> to handle most of that for us.
 * <p>
 * Normally the same Vicar label and binary headers are written to each file,
 * as described above.  If different labels/headers are desired, they may
 * be written directly to the component files.  Be warned, however...
 * attempts to modify the system labels of the component files will cause
 * undefined results.
 * <p>
 * @see VicarOutput
 * @see VicarOutputFile
 * @see VicarInputImage
 */

public class VicarOutputImage implements VicarOutput
{
    protected VicarOutputFile _files[];

    // Maps from virtual band number to output file and band within that file
    protected int _band_map_file[];
    protected int _band_map_band[];

    protected SystemLabel _system;
    protected VicarLabel _vicar_label;	// used only before open()

    protected boolean _file_open;

    protected boolean _random_allowed;
    protected boolean _random_easy;

    protected VicarDataFormat _binary_data_format;

    protected VicarInput _primary_input;
    protected VicarLabel _primary_label;
    protected SystemLabel _primary_system;

////////////////////////////////////////////////////////////////////////

/***********************************************************************
 * Dummy constructor (for now). Need to add good ones that call open().
 */
    public VicarOutputImage()
    {
	_files = null;
	_band_map_file = null;
	_band_map_band = null;
	_system = null;
	_random_allowed = false;
	_random_easy = false;
	_vicar_label = null;
	_file_open = false;
    }

/***********************************************************************
 * Opens a virtual file given a set of three <code>VicarOutputFile</code>
 * objects, which must already be individually opened.
 * <p>
 * <em>Warning!</em>  Undefined results will occur if the system labels of
 * these files do not match what is set on this object!  Only a limited
 * set of items are actually checked.
 * <p>
 * Note that labels set on this object prior to calling this version of
 * <code>open()</code> will be ignored in favor of each file's individual
 * labels.  Subsequent calls to <code>this.setVicarLabel()</code> however
 * will set the labels in each file.
 * @throws IOException
 * @throws IllegalArgumentException if the system labels don't match
 */
    public synchronized void open(VicarOutputFile red,
				  VicarOutputFile grn, VicarOutputFile blu)
				  throws IOException
    {
	_files = new VicarOutputFile[3];
	_files[0] = red;
	_files[1] = grn;
	_files[2] = blu;
	openInternal();
    }

/***********************************************************************
 * Opens a virtual file given an array of <code>VicarOutputFile</code>
 * objects, which must already be individually opened.
 * <p>
 * <em>Warning!</em>  Undefined results will occur if the system labels of
 * these files do not match what is set on this object!  Only a limited set
 * of items are actually checked.  Note that the number of bands in each
 * file can vary but the total must equal the number of bands in this object.
 * <p>
 * Note that labels set on this object prior to calling this version of
 * <code>open()</code> will be ignored in favor of each file's individual
 * labels.  Subsequent calls to <code>this.setVicarLabel()</code> however
 * will set the labels in each file.
 * @throws IOException
 * @throws IllegalArgumentException if the system labels don't match
 */
    public synchronized void open(VicarOutputFile[] files) throws IOException
    {
	_files = (VicarOutputFile[])files.clone();
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
 * @see #open(List, int[])
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

	open(name_list, null);
    }

/***********************************************************************
 * Opens a virtual file given a List of filenames.  The List must
 * contain Strings.  Note that unlike open(String), no parsing or extension
 * swapping is done on the names; what you pass in is the file you get.
 * (This does allow blanks and commas in the name, and files that start
 * with ".").
 * @throws IOException
 * @see #open(String)
 * @see #open(List, int[])
 */
    public synchronized void open(List v) throws IOException
    {
	open(v, null);
    }

/***********************************************************************
 * Opens a virtual file given a List of filenames and a number of bands for
 * each file.  The List must contain Strings.  Note that unlike open(String),
 * no parsing or extension swapping is done on the names; what you pass in
 * is the file you get.  (This does allow blanks and commas in the name, and
 * files that start with ".").
 * @param v The list of filenames.
 * @param bands An array containing the number of bands for each corresponding
 * file in the list of filenames.  If bands is null, 1 is assumed for each.
 * The total # of bands must match the # of bands set in this virtual image's
 * system label, and the length of <code>bands</code> must match the length
 * of <code>v</code>.
 * @throws IOException
 * @see #open(String)
 */
    public synchronized void open(List v, int[] bands) throws IOException
    {
	SystemLabel out_sys = getSystemLabel();
	out_sys.setOrg("BSQ");
	VicarLabel out_vic = getVicarLabel();
	_vicar_label = null;		// already obtained saved one

	_files = new VicarOutputFile[v.size()];

	for (int i=0; i < v.size(); i++) {
	    _files[i] = new VicarOutputFile();
	    if (bands == null)
		out_sys.setNB(1);
	    else
		out_sys.setNB(bands[i]);
	    _files[i].setSystemLabel(out_sys);
	    _files[i].setVicarLabel(out_vic);
	    _files[i].open((String)v.get(i));
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
	     "No VicarOutputFile objects specified in VicarOutputImage.open()");

	// Make sure we have a system label.  The get will create if not.

	_system = getSystemLabel();

	// Count the number of bands in all files and set up the band_maps.
	// Also check the sizes to make sure they all match.

	int counted_nb = 0;
	_band_map_file = new int[_system.getNB()];
	_band_map_band = new int[_system.getNB()];
	int band_index = 0;
	_random_allowed = true;
	_random_easy = true;

	for (i=0; i < _files.length; i++) {
	    SystemLabel out = _files[i].getSystemLabel();
	    counted_nb += out.getNB();
	    if (out.getNS() != _system.getNS() ||
		out.getNL() != _system.getNL()) {
		throw new IOException("Image size of image " + i +
					" doesn't match in VicarOutputImage");
	    }
	    if (out.getOrg() != "BSQ") {
		throw new IOException("Output file " + i +
			       " must be BSQ organization in VicarOutputImage");
	    }
	    if (counted_nb > _system.getNB()) {
		throw new IOException(
		       "Physical files contain too many bands for VicarOutputImage: " + counted_nb + " physical bands, " + _system.getNB() + " needed");
	    }

	    // Set up the band maps

	    for (int j=0; j < out.getNB(); j++) {
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

	if (counted_nb != _system.getNB()) {
	    throw new IOException(
		     "Number of bands in physical files must match # of bands in VicarOutputImage");
	}

	// Dimension must be 3, since this is a multi-band virtual file
	// And, we're always in BSQ.

	_system.setDim(3);
	_system.setOrg("BSQ");

	_file_open = true;
    }

/***********************************************************************
 * Closes all component <code>VicarOutputFile</code>s.
 */
    public synchronized void close() throws IOException
    {
	if (!_file_open)
	    throw new IOException("Can't close a file that's not open");
	_file_open = false;

	if (_files == null)
	    return;
	for (int i=0; i < _files.length; i++)
	    _files[i].close();
    }

/***********************************************************************
 * Returns the actual file for a given file index (NOT a band number!).
 * @see #getFileIndex(int)
 */
    public VicarOutputFile getFile(int index)
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
 * Sets the "primary input" for this output image.  The initial system and
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
 * <li>Label of first actual output file (if this image has been opened)
 * <li>Label set via <code>setVicarLabel</code>
 * <li>Primary input's label
 * <li>Empty (newly constructed) label
 * <p>
 * @see #getSystemLabel()
 */
    public synchronized VicarLabel getVicarLabel() throws IOException
    {
	if (_file_open)
	    return _files[0].getVicarLabel();

	if (_vicar_label != null)
	    return (VicarLabel)_vicar_label.clone();

	// We don't cache the value because we don't want it written out
	// unless set is called

	VicarLabel lbl = getPrimaryInputVicarLabel();
	if (lbl != null)
	    return lbl;

	lbl = new VicarLabel();
	return lbl;
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
 * to each component file.  Note that the system label portion of
 * <code>VicarLabel</code> is ignored; in fact, the system label portion
 * (only!) might be modified by this routine to reflect the current system
 * label (no guarantees one way or the other).
 * <p>
 * Unlike <code>setSystemLabel()</code>, this routine can be called at any
 * time after the system label is set, assuming the file is random-access.
 * You can call this routine multiple times in order to update the label, as
 * well.  For a sequential-only file, however, this routine must only be
 * called before the file is opened.
 * <p>
 * The labels of all physical output files are set by this routine.  In order
 * to set the label of only one file, it may be set directly via
 * <code>getFile()</code>.
 * <p>
 * The VICAR label is generally obtained first via a call to
 * <code>getVicarLabel</code> on this (or another) image.  It may then
 * be modified before submitting it back via this routine.
 * <p>
 * Before the label is set, a call to <code>getVicarLabel</code> will
 * return the existing label if the file already exists.  If not, the
 * primary input's label is returned.  If that hasn't been set, an empty
 * VICAR label is returned.  Once the label is set via this routine (or the
 * <code>open()</code> is called), future get calls will return the label
 * of the first physical output file.
 * <p>
 * It is up to the application to add a VICAR history task if desired.
 * Use <code>VicarLabel.createHistoryTask()</code>.
 * <p>
 * Note that if the VICAR label is set via this routine, the VICAR label is
 * written to all physical files, even ones that aren't open yet.  The
 * exception to this is any form of <code>open()</code> to which already-open
 * files are provided.  In this case only, existing labels are not modified.
 * However, if the labels are modified <em>after</em> the files are open
 * (via any means), the labels will be written to all physical files.
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

	if (_file_open) {
	    for (int i=0; i < _files.length; i++)
		_files[i].setVicarLabel(vl);
	}
	else {			// save until the files are opened
	    _vicar_label = (VicarLabel)vl.clone();
	}
    }

/***********************************************************************
 * Retrieves a deep copy of the <code>SystemLabel</code> object associated
 * with this image.  This should not be confused with
 * <code>VicarLabel.getSystem()</code>, which returns a
 * <code>VicarLabelSet</code> rather than a <code>SystemLabel</code> object.
 * The <code>SystemLabel</code> object returned by this function will not
 * contain the same information as the System part of the
 * <code>VicarLabel</code> object; specifically, the System label applies to
 * this virtual image as a whole, not any of the component parts.
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
 * order to create a sample model.  Management of the expandable files is
 * done by the physical files themselves.  So if an unequal amount is written
 * to each band, the output files might not be the same size.
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
 * Since the virtual file is always BSQ, n2==line and n3==band.
 * The most normal call, to write an entire record, would be
 * <code>x.writeRecord(data, 0, 0, 0, 1, line, band);</code>
 * @param data The array to write from
 * @param start Starting position in the record.  Normally 0 to write the whole
 *  record.
 * @param length Number of pixels to write.  If 0, the rest of the record is
 *  written.
 * @param offset Offset into the data array at which to get the data.
 * @param pixelStride Offset from one pixel to the next in the array.  For
 *  example, a stride of 1 has all pixels contiguous while a stride of 3
 *  allows band interleaved pixels in the array (pixels written to the file
 *  will be 3 elements apart in the array).  This has nothing to do with
 *  what is written to the file - only where the data comes from in the array.
 *  Note that this value is measured in pixels or array elements - not bytes.
 * @param n2 Line number to write (the virtual file is always BSQ).
 * @param n3 Band number to write (the virtual file is always BSQ).
 * @throws NonSequentialAccessException (subclass of IOException)
 * @throws IOException
 */
    public void writeRecord(byte[] data, int start, int length, int offset,
			int pixelStride, int n2, int n3) throws IOException
    {
	if (length == 0)
	    length = _system.getN1() - start;

	int file = _band_map_file[n3];		// translate band to file
	int band = _band_map_band[n3];		// band within that file

	_files[file].writeRecord(data, start, length, offset, pixelStride,
								n2, n3);
    }

/***********************************************************************
 * Low-level write function, writes a short array to the given record.
 * Since the virtual file is always BSQ, n2==line and n3==band.
 * Follow the See Also link for parameters
 * @see #writeRecord(byte[], int, int, int, int, int, int)
 */
    public void writeRecord(short[] data, int start, int length, int offset,
			int pixelStride, int n2, int n3) throws IOException
    {
	if (length == 0)
	    length = _system.getN1() - start;

	int file = _band_map_file[n3];		// translate band to file
	int band = _band_map_band[n3];		// band within that file

	_files[file].writeRecord(data, start, length, offset, pixelStride,
								n2, n3);
    }

/***********************************************************************
 * Low-level write function, writes an unsigned short array to the given
 * record.  Although unsigned short is not a VICAR data type, it is useful
 * for some other formats which use this library.  Note that a short array
 * must be used, although the data should be interpreted as unsigned.
 * For this reason, a different method name is needed.
 * <p>
 * Since the virtual file is always BSQ, n2==line and n3==band.
 * Follow the See Also link for parameters
 * @see #writeRecord(byte[], int, int, int, int, int, int)
 */
    public void writeRecordUshort(short[] data, int start, int length,
		int offset, int pixelStride, int n2, int n3) throws IOException
    {
	if (length == 0)
	    length = _system.getN1() - start;

	int file = _band_map_file[n3];		// translate band to file
	int band = _band_map_band[n3];		// band within that file

	_files[file].writeRecordUshort(data, start, length, offset, pixelStride,
								n2, n3);
    }

/***********************************************************************
 * Low-level write function, writes an int array to the given record.
 * Follow the See Also link for parameters
 * @see #writeRecord(byte[], int, int, int, int, int, int)
 */
    public void writeRecord(int[] data, int start, int length, int offset,
			int pixelStride, int n2, int n3) throws IOException
    {
	if (length == 0)
	    length = _system.getN1() - start;

	int file = _band_map_file[n3];		// translate band to file
	int band = _band_map_band[n3];		// band within that file

	_files[file].writeRecord(data, start, length, offset, pixelStride,
								n2, n3);
    }

/***********************************************************************
 * Low-level write function, writes a float array to the given record.
 * Follow the See Also link for parameters
 * @see #writeRecord(byte[], int, int, int, int, int, int)
 */
    public void writeRecord(float[] data, int start, int length, int offset,
			int pixelStride, int n2, int n3) throws IOException
    {
	if (length == 0)
	    length = _system.getN1() - start;

	int file = _band_map_file[n3];		// translate band to file
	int band = _band_map_band[n3];		// band within that file

	_files[file].writeRecord(data, start, length, offset, pixelStride,
								n2, n3);
    }

/***********************************************************************
 * Low-level write function, writes a double array to the given record.
 * Follow the See Also link for parameters
 * @see #writeRecord(byte[], int, int, int, int, int, int)
 */
    public void writeRecord(double[] data, int start, int length, int offset,
			int pixelStride, int n2, int n3) throws IOException
    {
	if (length == 0)
	    length = _system.getN1() - start;

	int file = _band_map_file[n3];		// translate band to file
	int band = _band_map_band[n3];		// band within that file

	_files[file].writeRecord(data, start, length, offset, pixelStride,
								n2, n3);
    }

/***********************************************************************
 * Low-level write function, writes complex data in a float array to the
 * given record.  Each complex value is stored in two consecutive float
 * elements in the array, so it must be twice as long as you would normally
 * expect, and <code>offset</code> must be multiplied by 2.  <code>length</code>
 * is still measured in <em>pixels</em>, so there are 2*<code>length</code>
 * values filled in the array.
 * <p>
 * Follow the See Also link for parameters
 * @see #writeRecord(byte[], int, int, int, int, int, int)
 */
    public void writeRecordComp(float[] data, int start, int length, int offset,
			int pixelStride, int n2, int n3) throws IOException
    {
	// Not quite sure how to handle complex w.r.t. multiple bands
	throw new UnsupportedOperationException("Complex data not yet supported in VicarOutputImage!!!!");
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
 * Writes the binary header to each output file, which is NLB records at the
 * beginning of the file.  See <code>VicarOutputFile.setBinaryHeader()</code>
 * for more details.
 * <p>
 * The same header is written to each physical output file.  Individual headers
 * can be written directly to the outputs using <code>getFile()</code>.
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
 * @see VicarOutputFile#setBinaryHeader
 * @see VicarBinaryLabel
 * @see #createBinaryHeader
 * @see VicarInput#getBinaryHeader
 */
    public void setBinaryHeader(VicarBinaryLabel vbl)
                throws IOException, NonSequentialAccessException,
                BinaryFormatMismatchException, BinarySizeMismatchException
    {
	for (int i=0; i < _files.length; i++)
	    _files[i].setBinaryHeader(vbl);
    }

/***********************************************************************
 * Writes the binary prefix to one output file, which is NBB <em>bytes</em>
 * at the beginning of the file.  See
 * <code>VicarOutputFile.setBinaryPrefix()</code> for more details.
 * <p>
 * The prefix is written to only one file, as determined by the band number.
 *
 * @param vbl <code>VicarBinaryLabel</code> containing the data to write.
 * @param n2 Line number to write (the virtual file is always BSQ)
 * @param n3 Band number to write (the virtual file is always BSQ)
 * @throws BinaryFormatMismatchException if format doesn't match
 *  BINTFMT/BREALFMT of the image (subclass of IOException)
 * @throws BinarySizeMismatchException if size doesn't match NBB of the
 *  image (subclass of IOException)
 * @throws NonSequentialAccessException (subclass of IOException)
 * @throws IOException if the file is not open, or other error occurs
 *
 * @see VicarOutputFile#setBinaryPrefix
 * @see VicarBinaryLabel
 * @see #createBinaryPrefix
 * @see VicarInput#getBinaryPrefix
 */
    public void setBinaryPrefix(VicarBinaryLabel vbl, int n2, int n3)
                throws IOException, NonSequentialAccessException,
                BinaryFormatMismatchException, BinarySizeMismatchException
    {
	int file = _band_map_file[n3];		// translate band to file
	int band = _band_map_band[n3];		// band within that file
	_files[file].setBinaryPrefix(vbl, n2, band);
    }

////////////////////////////////////////////////////////////////////////

/***********************************************************************
 * Creates a SampleModel that is most compatible with the input image.
 * Follow the See Also link for complete description
 * @throws IllegalArgumentException if the number of bands is left
 * expandable (i.e. 0).
 * @see VicarIOBase#createSampleModel(int,int)
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
    public void writeTile(int x, int y, int w, int h, int x_off, int y_off,
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
		"Illegal width in VICAR writeTile: " + w + ", x_off=" + x_off +
			", width=" + sm.getWidth());
	if (y_off + h > sm.getHeight())
	    throw new ArrayIndexOutOfBoundsException(
		"Illegal height in VICAR writeTile: " + h + ", y_off=" + y_off +
			", height=" + sm.getHeight());

	// Loop through the bands, processing one at a time.  For each one,
	// we map that to a file/band combination, then write that band
	// directly from the DataBuffer.

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

	    // Write a single band's tile to the file.

	    band_subset[0] = band;	// what to write to output file
	    _files[file].writeTile(x, y, w, h, x_off, y_off,
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

