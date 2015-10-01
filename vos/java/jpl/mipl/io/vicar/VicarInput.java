package jpl.mipl.io.vicar;

import java.io.*;
import java.awt.image.*;

/**
 * This interface includes everything needed to access an input VICAR file.
 * <p>
 * There are two "levels" of data read functions here.  The low-level
 * routines (the various <code>readRecord()</code>s) read a "record" (usually
 * a single line, or band for BIP) of pixels from the image, converting them
 * to the requested format.  These routines are useful for accessing the
 * basic data in the file.  The high-level routines (the various
 * <code>readTile()</code>s) read a tile of data, given high-level constructs
 * like <code>SampleModel</code>s.
 * <p>
 * Binary labels can be used with any file that allows random access.  For
 * sequential-access files, they may only be used with the low-level read
 * routines, and then only in the proper sequence (read binary headers before
 * anything else, read binary prefix before each "record" of data).  See
 * <code>getBinaryHeader()</code> and <code>getBinaryPrefix()</code>.
 * <p>
 * @see VicarIOBase
 * @see VicarOutput
 */

public interface VicarInput extends VicarIOBase
{

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
    public void readRecord(byte[] data, int start, int length, int offset,
			int pixelStride, int n2, int n3) throws IOException;

/***********************************************************************
 * Low-level read function, returns a short array for the given record.
 * Follow the See Also link for parameters.
 * @see #readRecord(byte[], int, int, int, int, int, int)
 */
    public void readRecord(short[] data, int start, int length, int offset,
			int pixelStride, int n2, int n3) throws IOException;

/***********************************************************************
 * Low-level read function, returns an unsigned short array for the given
 * record.  Although unsigned short is not a VICAR data type, it is useful
 * for some other formats which use this library.  Note that a short array
 * must be used, although the data should be interepreted as unsigned.
 * For this reason, a different method name is needed.
 * <p>
 * Follow the See Also link for parameters.
 * @see #readRecord(byte[], int, int, int, int, int, int)
 */
    public void readRecordUshort(short[] data, int start, int length,
		int offset, int pixelStride, int n2, int n3) throws IOException;

/***********************************************************************
 * Low-level read function, returns an int array for the given record.
 * Follow the See Also link for parameters.
 * @see #readRecord(byte[], int, int, int, int, int, int)
 */
    public void readRecord(int[] data, int start, int length, int offset,
			int pixelStride, int n2, int n3) throws IOException;

/***********************************************************************
 * Low-level read function, returns a float array for the given record.
 * Follow the See Also link for parameters.
 * @see #readRecord(byte[], int, int, int, int, int, int)
 */
    public void readRecord(float[] data, int start, int length, int offset,
			int pixelStride, int n2, int n3) throws IOException;

/***********************************************************************
 * Low-level read function, returns a double array for the given record.
 * Follow the See Also link for parameters.
 * @see #readRecord(double[], int, int, int, int, int, int)
 */
    public void readRecord(double[] data, int start, int length, int offset,
			int pixelStride, int n2, int n3) throws IOException;

/***********************************************************************
 * Low-level read function, returns complex data in a float array for the
 * given record.  Each complex value is stored in two consecutive float
 * elements in the array, so it must be twice as long as you would normally
 * expect, and <code>offset</code> must be multiplied by 2.  <code>length</code>
 * is still measured in <em>pixels</em>, so there are 2*<code>length</code>
 * values filled in the array.
 * <p>
 * Follow the See Also link for parameters.
 * @see #readRecord(byte[], int, int, int, int, int, int)
 */
    public void readRecordComp(float[] data, int start, int length, int offset,
			int pixelStride, int n2, int n3) throws IOException;

////////////////////////////////////////////////////////////////////////

/***********************************************************************
 * Provides access to the binary header, which is NLB records at the beginning
 * of the file.  This function returns a <code>VicarBinaryLabel</code> object
 * which contains the entire header.  From it, you can get a limited-length
 * stream which implements <code>DataInput</code>.  This stream automatically
 * translates data types based on BINTFMT and BREALFMT, so e.g. if you call
 * <code>readFloat()</code>, the stream will either return the next four
 * bytes as a float (BREALFMT=IEEE), byte-swap them (BREALFMT=RIEEE), or
 * convert them from VAX format (BREALFMT=VAX).  So a typical binary header
 * might be read like this:
 * <pre>
 *   VicarBinaryLabel bh = file.getBinaryHeader();
 *   DataInput di = bh.getDataInput();
 *   int first = di.readInt();
 *   short second = di.readShort();
 *   byte b[8];
 *   for (i=0; i<8; i++) b[i] = di.readByte();
 *   String third(b);
 *   ... etc ...
 * </pre>
 * The returned stream will signal EOF when the end of the binary header area
 * is reached.  You can also access the label buffer directly via
 * <code>VicarBinaryLabel.getBuffer()</code>, but this is not recommended
 * (because no data type translation is done).
 * <p>
 * Internally, the header is copied to a buffer, so the
 * <code>VicarBinaryLabel</code> object will stay valid as long as you
 * need it (i.e., you can read data from the file and the header will still
 * stay valid).
 * <p>
 * If this routine is used with a strictly sequential file, it must be called
 * before any pixel data (or binary prefixes) are read.
 *
 * @return A VicarBinaryLabel that contains the header, or NULL if there is
 *  header.
 * @throws NonSequentialAccessException (subclass of IOException)
 * @throws IOException
 * @see VicarBinaryLabel
 */
    public VicarBinaryLabel getBinaryHeader() throws IOException;

/***********************************************************************
 * Provides access to the binary prefix, which is NBB <em>bytes</em> at the
 * beginning of each record.  This function returns a
 * <code>VicarBinaryLabel</code> object which contains the entire prefix.
 * From it, you can get a limited-length stream which implements
 * <code>DataInput</code>, and behaves as described in
 * <code>getBinaryHeader()</code>.  The stream will signal EOF at the end
 * of the prefix for this record.  A buffer is maintained, as with the
 * header, so the <code>VicarBinaryLabel</code> object will stay valid in
 * the future.  Direct access to the buffer is possibly but strongly
 * discouraged.
 * <p>
 * If this routine is used with a strictly sequential file, it must be called
 * before the pixel data for this record is read (and of course, after the
 * previous record's data).  For this reason, this routine is incompatible
 * with the Tile read functions for strictly sequential files (unless the
 * tile height is 1).
 *
 * @param n2 "Line number" to read (for BSQ organization).  (For BIL: band
 *  number, for BIP: sample number).  The second dimension's index.
 * @param n3 "Band number" to read (for BSQ organization).  (For BIL or BIP:
 *  line number).  The third dimension's index.
 * @return A VicarBinaryLabel that contains the prefix, or NULL if there is no
 *  prefix.
 * @throws NonSequentialAccessException (subclass of IOException)
 * @throws IOException
 * @see #getBinaryHeader()
 * @see VicarBinaryLabel
 */
    public VicarBinaryLabel getBinaryPrefix(int n2, int n3) throws IOException;

////////////////////////////////////////////////////////////////////////

/***********************************************************************
 * High-level read function.  Given an x/y position in the file, read a
 * complete tile (size specified by the SampleModel) into the DataBuffer.
 * Data is placed in the SM starting at its origin.  If the file does not
 * contain enough data to fill the SM, then only the parts that can be read,
 * are read.  No error is returned in this case (it is up to the caller to
 * check the boundary conditions if needed).
 * @throws IOException if there are problems reading the file
 */
    public void readTile(int x, int y, SampleModel m, DataBuffer b)
			throws IOException;

/***********************************************************************
 * High-level read function.  Given an x/y position in the file, and a
 * width/height, read that portion of the file into the given DataBuffer.
 * Data is placed starting at an origin of x_off/y_off within the SampleModel
 * (i.e. x/y is the position in the file to read, x_off/y_off is the position
 * within the buffer to place the data).  If the file does not contain enough
 * data to satisfy width/height, then only the parts that can be read, are
 * read.  No error is returned in this case (it is up to the caller to
 * check the boundary conditions if needed).  However, if the requested
 * width/height (combined with x_off/y_off) will not fit within the SampleModel,
 * an ArrayIndexOutOfBoundsException is thrown.
 * @throws IOException if there are problems reading the file
 */
    public void readTile(int x, int y, int w, int h, int x_off, int y_off,
			SampleModel m, DataBuffer b)
			throws IOException;

/***********************************************************************
 * High-level read function.  Just like the previous readTile, except that
 * an array of band numbers is also provided.  Only bands in this array are
 * read.  On output, the number of bands actually read will equal the size
 * of this array, i.e. original band numbers are not preserved.  If too many
 * bands, or an out-of-range band number is specified, an
 * ArrayIndexOutOfBoundsException is thrown.
 * <p>
 * bandList may be null, in which case all bands are read.  Note that for
 * Complex data, each part (real/imaginary) is treated as a separate band.
 * So, a multi-banded Complex file with have 2*nb bands.  The bands requested
 * in bandList take this into account, e.g. band 2 in the list is the real
 * part of the file's band 1 (counting from 0), band 5 in the list is the
 * imaginary part of the file's band 3, etc.
 * <p>
 * For BIP data (Band Interleaved by Pixel), it is much more efficient to
 * read all bands at once (i.e. pass bandList as null).
 * @see #readTile(int,int,int,int,int,int,SampleModel,DataBuffer)
 * @throws IOException if there are problems reading the file
 */
    public void readTile(int x, int y, int w, int h, int x_off, int y_off,
			int bandList[],
			SampleModel m, DataBuffer b)
			throws IOException;
}

