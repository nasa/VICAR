package jpl.mipl.io.vicar;

import java.io.*;
import java.awt.image.*;

/**
 * This interface includes everything needed to access an output VICAR file.
 * <p>
 * There are two "levels" of data write functions here.  The low-level
 * routines (the various <code>writeRecord()</code>s) write a "record" (usually
 * a single line, or band for BIP) of pixels to the image, converting them
 * to the requested format.  These routines are useful for accessing the
 * basic data in the file.  The high-level routines (the various
 * <code>writeTile()</code>s) write a tile of data, given high-level constructs
 * like <code>SampleModel</code>s.
 * <p>
 * System label items, that describe the file (size, format, pixel type, etc.)
 * must be set <em>once</em>, before the file is opened.  Once the file is open,
 * they cannot be changed.  So you need to know all the relevant details of
 * the file structure before the file is created.  As a special case, the
 * n3 dimension (or n2 iff n3==1) can be set to 0, meaning that it is unknown
 * at open time.  The file can thus expand as lines are written to whatever
 * size is desired.  When the file is closed, the size will be set to the
 * maximum record written.  This is only possible with random-access output
 * streams.  Note that files expandable in the Band dimension cannot be used
 * with <code>createSampleModel</code> since the # of bands must be known in
 * order to create a sample model.
 * <p>
 * VICAR labels, on the other hand, can be written more than once (for
 * random-access streams).  If the stream is sequential, the VICAR label must
 * be written once, before any data is written.  The System label portion of
 * the <code>VicarLabel</code> object is ignored (and actually will be
 * modified; see <code>setVicarLabel()</code>).
 * <p>
 * Both the system label and VICAR (history/property) labels can be derived
 * from another VICAR image called for historical reasons the "primary input".
 * The <code>getSystemLabel()</code> and <code>getVicarLabel()</code>
 * functions will return the system/vicar labels of this primary input, if
 * no other label has been set.  This provides a convenient mechanism to copy
 * over history and property labels, and to set the minimum number of
 * parameters in the system label.
 * <p>
 * Applications should generally add a history task to the label.  First,
 * obtain the current label with <code>getVicarLabel()</code>.  Then, add a
 * history task with <code>VicarLabel.createHistoryTask()</code>.  Make
 * whatever other modifications to the label are desired, then put it back
 * via <code>setVicarLabel()</code>.
 * <p>
 * The general paradigm for labels is read-modify-write.  Get the label via
 * one of the calls, modify it, and Put it back.  The same goes for Binary
 * labels, except there is an additional function to create an empty binary
 * label (you can manually construct an empty <code>VicarLabel</code> or
 * <code>SystemLabel</code> if this capability is required with normal labels,
 * but it shouldn't be).
 * <p>
 * Binary labels can be used with any file that allows random access.  For
 * sequential-access files, they may only be used with the low-level write
 * routines, and then only in the proper sequence (write binary headers before
 * anything else, write binary prefix before each "record" of data).  See
 * <code>setBinaryHeader()</code> and <code>setBinaryPrefix()</code>.
 * <p>
 * It is likely that any classes implementing this interface will also
 * implement <code>VicarInput</code>, but this is not a requirement.  Shared
 * functions between the two are in the base class, <code>VicarIOBase</code>.
 * <p>
 * To pull all this together, here is the simplest form of opening a new
 * output file:
 * <p> <pre>
 * VicarOutputFile f = new VicarOutputFile();
 * // Create the system label (just get from the file; new one will be created)
 * SystemLabel sys = f.getSystemLabel();
 * sys.setNL(...);
 * sys.setNS(...);
 * ... other settings ...
 * // Open the file
 * f.setSystemLabel(sys);
 * f.open(filename);
 * // Optionally create an output label
 * VicarLabel lbl = f.getVicarLabel();
 * lbl.createHistoryTask("java_prog");	// use actual name of java program
 * ... other label mods ...
 * f.setVicarLabel(lbl);
 * // Write data
 * ... various pixel write functions ... e.g. f.writeTile(), f.writeRecord()
 * // Close file
 * f.close();
 * </pre> <p>
 *
 * A more realistic example would use a "primary input" file.  It looks mostly
 * the same; the paradigm is "get label, modify it, set it back" in both cases.
 *
 * <p> <pre>
 * VicarOutputFile f = new VicarOutputFile();
 * f.setPrimaryInput(input_file);
 * // Get and modify system label (optional if sys label info doesn't change)
 * SystemLabel sys = f.getSystemLabel();
 * sys.setNL(...);	// or other settings ...
 * f.setSystemLabel(sys);
 * // Open the file
 * f.open(filename);
 * // Add to the output label
 * VicarLabel lbl = f.getVicarLabel();
 * lbl.createHistoryTask("java_prog");	// use actual name of java program
 * ... other label mods ...
 * f.setVicarLabel(lbl);
 * // Write data
 * ... various pixel write functions ... e.g. f.writeTile(), f.writeRecord()
 * // Close file
 * f.close();
 * </pre> <p>
 *
 * @see VicarIOBase
 * @see VicarInput
 */

public interface VicarOutput extends VicarIOBase
{

/***********************************************************************
 * Sets the "primary input" for this output file.  The initial system and
 * VICAR labels are obtained from the primary input.  If the PI is not
 * specified, the initial labels are default/empty.  Once the system or
 * VICAR labels have been set, or obtained once, the primary input is no
 * longer used.
 * <p>
 * Note that for update files (those open for both read and write), the
 * primary input is completely ignored.  Since the file exists before being
 * opened for write, the existing labels are used instead.
 * <p>
 * This routine should obviously be called before getting the system or
 * VICAR labels if it is to have any effect.
 * <p>
 * The label is not obtained from the input until/unless it is actually needed
 * (deferred execution).
 * <p>
 * Calling this routine nulls any labels set via the other
 * <code>setPrimaryInput()</code> routine.
 * <p>
 * Passing in null will completely disable the primary input mecahnism.
 * <p>
 * @see #setPrimaryInput(SystemLabel, VicarLabel)
 * @throws AlreadyOpenException if the file has been opened already
 */
    public void setPrimaryInput(VicarInput inp) throws AlreadyOpenException;

/***********************************************************************
 * Gets the primary input, or null if none is available.  PI's set via
 * the second method (<code>setPrimaryInput(SystemLabel, VicarLabel)</code>)
 * are not returned by this method.
 */
    public VicarInput getPrimaryInput();

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
 * @throws AlreadyOpenException if the file has been opened already
 */
    public void setPrimaryInput(SystemLabel slbl, VicarLabel vlbl)
						throws AlreadyOpenException;

/***********************************************************************
 * Returns a deep clone of the primary input system label (set via either
 * method).  This should be used only in special cases; normally the main
 * <code>getSystemLabel()</code> should be used instead.  Returns null
 * if the PI or label doesn't exist; a default is not returned.
 * @see #setPrimaryInput(VicarInput)
 * @see #setPrimaryInput(SystemLabel, VicarLabel)
 * @see #getSystemLabel()
 */
    public SystemLabel getPrimaryInputSystemLabel() throws IOException;

/***********************************************************************
 * Returns a deep clone of the primary input VICAR label (set via either
 * method).  This should be used only in special cases; normally the main
 * <code>getVicarLabel()</code> should be used instead.  Returns null
 * if the PI or label doesn't exist; a default is not returned.
 * @see #setPrimaryInput(VicarInput)
 * @see #setPrimaryInput(SystemLabel, VicarLabel)
 * @see #getVicarLabel()
 */
    public VicarLabel getPrimaryInputVicarLabel() throws IOException;

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
 * well.  For a sequential-only file, however, this routine must be only be
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
    public void setVicarLabel(VicarLabel sl) throws IOException;

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
    public void setSystemLabel(SystemLabel sl) throws AlreadyOpenException;

////////////////////////////////////////////////////////////////////////

/***********************************************************************
 * Low-level write function, writes a byte array to the given record.
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
 * @param n2 "Line number" to write (for BSQ organization).  (For BIL: band
 *  number, for BIP: sample number).  The second dimension's index.
 * @param n3 "Band number" to write (for BSQ organization).  (For BIL or BIP:
 *  line number).  The third dimension's index.
 * @throws NonSequentialAccessException (subclass of IOException)
 * @throws IOException
 */
    public void writeRecord(byte[] data, int start, int length, int offset,
			int pixelStride, int n2, int n3) throws IOException;

/***********************************************************************
 * Low-level write function, writes a short array to the given record.
 * Follow the See Also link for parameters.
 * @see #writeRecord(byte[], int, int, int, int, int, int)
 */
    public void writeRecord(short[] data, int start, int length, int offset,
			int pixelStride, int n2, int n3) throws IOException;

/***********************************************************************
 * Low-level write function, writes an unsigned short array to the given
 * record.  Although unsigned short is not a VICAR data type, it is useful
 * for some other formats which use this library.  Note that a short array
 * must be used, although the data should be interepreted as unsigned.
 * For this reason, a different method name is needed.
 * <p>
 * Follow the See Also link for parameters.
 * @see #writeRecord(byte[], int, int, int, int, int, int)
 */
    public void writeRecordUshort(short[] data, int start, int length,
		int offset, int pixelStride, int n2, int n3) throws IOException;

/***********************************************************************
 * Low-level write function, writes an int array to the given record.
 * Follow the See Also link for parameters.
 * @see #writeRecord(byte[], int, int, int, int, int, int)
 */
    public void writeRecord(int[] data, int start, int length, int offset,
			int pixelStride, int n2, int n3) throws IOException;

/***********************************************************************
 * Low-level write function, writes a float array to the given record.
 * Follow the See Also link for parameters.
 * @see #writeRecord(byte[], int, int, int, int, int, int)
 */
    public void writeRecord(float[] data, int start, int length, int offset,
			int pixelStride, int n2, int n3) throws IOException;

/***********************************************************************
 * Low-level write function, writes a double array to the given record.
 * Follow the See Also link for parameters.
 * @see #writeRecord(double[], int, int, int, int, int, int)
 */
    public void writeRecord(double[] data, int start, int length, int offset,
			int pixelStride, int n2, int n3) throws IOException;

/***********************************************************************
 * Low-level write function, write complex data from a float array to the
 * given record.  Each complex value is stored in two consecutive float
 * elements in the array, so it must be twice as long as you would normally
 * expect, and <code>offset</code> must be multiplied by 2.  <code>length</code>
 * is still measured in <em>pixels</em>, so there are 2*<code>length</code>
 * values filled in the array.
 * <p>
 * Follow the See Also link for parameters.
 * @see #writeRecord(byte[], int, int, int, int, int, int)
 */
    public void writeRecordComp(float[] data, int start, int length, int offset,
			int pixelStride, int n2, int n3) throws IOException;

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
    public VicarBinaryLabel createBinaryHeader();

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
    public VicarBinaryLabel createBinaryPrefix();

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
 * @throws IOException
 *
 * @see VicarBinaryLabel
 * @see #createBinaryHeader
 * @see VicarInput#getBinaryHeader
 */
    public void setBinaryHeader(VicarBinaryLabel vbl)
		throws IOException, NonSequentialAccessException,
		BinaryFormatMismatchException, BinarySizeMismatchException;

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
 * @throws BinarySizeMismatchException if size doesn't match NLB*recsize of
 *  image (subclass of IOException)
 * @throws NonSequentialAccessException (subclass of IOException)
 * @throws IOException
 *
 * @see VicarBinaryLabel
 * @see #createBinaryPrefix
 * @see VicarInput#getBinaryPrefix
 * @see #setBinaryHeader
 */
    public void setBinaryPrefix(VicarBinaryLabel vbl, int n2, int n3)
		throws IOException, NonSequentialAccessException,
		BinaryFormatMismatchException, BinarySizeMismatchException;

////////////////////////////////////////////////////////////////////////

/***********************************************************************
 * High-level write function.  Given an x/y position in the file, write a
 * complete tile (size specified by the SampleModel) from the DataBuffer.
 * Data is taken from the SM starting at its origin.  If the file is not
 * large enough to support all the data in the SM, then only the parts that
 * can be written, are written.  No error is returned in this case (it is
 * up to the caller to check the boundary conditions if needed).
 * @throws IOException if there are problems writing the file
 */
    public void writeTile(int x, int y, SampleModel m, DataBuffer b)
			throws IOException;

/***********************************************************************
 * High-level write function.  Given an x/y position in the file, and a
 * width/height, write that portion of the file from the given DataBuffer.
 * Data is taken starting at an origin of x_off/y_off within the SampleModel
 * (i.e. x/y is the position in the file to write, x_off/y_off is the position
 * within the buffer from which to get the data).  If the file is not large
 * enough to support width/height, then only the parts that can be written, are
 * written.  No error is returned in this case (it is up to the caller to
 * check the boundary conditions if needed).  However, if the requested
 * width/height (combined with x_off/y_off) exceeds the bounds of the
 * SampleModel, an ArrayIndexOutOfBoundsException is thrown.
 * @throws IOException if there are problems writing the file
 */
    public void writeTile(int x, int y, int w, int h, int x_off, int y_off,
			SampleModel m, DataBuffer b)
			throws IOException;

/***********************************************************************
 * High-level write function.  Just like the previous writeTile, except that
 * an array of band numbers is also provided.  These band numbers specify
 * where in the file the provided bands are written; bands in the SampleModel
 * are always processed sequentially.  If too many bands, or an out-of-range
 * band number is specified, an ArrayIndexOutOfBoundsException is thrown.
 * <p>
 * bandList may be null, in which case all bands are written in order to the
 * file.  Note that for Complex data, each band in the output requires two from
 * the input (real/imaginary), so the number of bands in bandList is half of
 * the number required in the input, e.g. bands 2 and 3 from the SM are written
 * to the band specified by the second entry in bandList.
 * <p>
 * For BIP data (Band Interleaved by Pixel), it is much more efficient to
 * write all bands at once (i.e. pass bandList as null).
 * @see #writeTile(int,int,int,int,int,int,SampleModel,DataBuffer)
 * @throws IOException if there are problems writing the file
 */
    public void writeTile(int x, int y, int w, int h, int x_off, int y_off,
			int bandList[],
			SampleModel m, DataBuffer b)
			throws IOException;
}

