package jpl.mipl.io.streams;

import java.io.*;

/**
 * This class wraps a <code>RandomAccessFile</code> into an OutputStream.
 * It's unbelievable that this is not in <code>java.io</code>!  Nor does
 * <code>com.sun.media.jai.codec</code> have a similar class (unlike the
 * input side, where there is <code>FileSeekableStream</code>).
 * <p>
 * No buffering is performed in this class directly, so if someone seeks
 * on the underlying RAF, the next write via this class will go to that
 * location.  Likewise, if <code>seek()</code> is called in this class,
 * the underlying RAF will be seek'ed as well, which could affect other
 * users.
 */

public class RandomAccessFileOutputStream extends OutputStream
{

    protected RandomAccessFile _file;

/***********************************************************************
 * Creates a <code>RandomAccessFileOutputStream</code> as a wrapper around
 * the given <code>RandomAccessFile</code>.
 */
    public RandomAccessFileOutputStream(RandomAccessFile file)
    {
	_file = file;
    }

/***********************************************************************
 * Returns the <code>RandomAccessFile</code> object.
 */
    public RandomAccessFile getFile()
    {
	return _file;
    }

/***********************************************************************
 * Skips over and discards n bytes of data in the ouput stream.
 * Returns the amount skipped.  Will not skip past the end-of-file,
 * as specified in <code>RandomAccessFile.skipBytes()</code>.
 * @see RandomAccessFile#skipBytes
 * @throws IOException if the <code>RandomAccessFile</code> does
 */
    public long skip(long n) throws IOException
    {
	// Why is n in _file.skipBytes an int instead of a long??!
	if (n < Integer.MAX_VALUE)
	    return _file.skipBytes((int)n);
	if (n < 0)
	    return 0;
	long old_pos = _file.getFilePointer();
	_file.seek(old_pos + n);
	return (_file.getFilePointer() - old_pos);
    }

/***********************************************************************
 * Sets the length of the file.
 * @see RandomAccessFile#setLength(long)
 * @throws IOException if the <code>RandomAccessFile</code> does
 */
    public void setLength(long newLength) throws IOException
    {
	_file.setLength(newLength);
    }

/***********************************************************************
 * Returns the current offset in the file.
 * @see RandomAccessFile#getFilePointer
 * @throws IOException if the <code>RandomAccessFile</code> does
 */
    public long getFilePointer() throws IOException
    {
	return _file.getFilePointer();
    }

/***********************************************************************
 * Sets the file-pointer offset, measured from the beginning of this file,
 * at which the next read or write occurs.
 * @see RandomAccessFile#seek
 * @throws IOException if the <code>RandomAccessFile</code> does
 */
    public void seek(long pos) throws IOException
    {
	_file.seek(pos);
    }

////////////////////////////////////////////////////////////////////////
// OutputStream overridden methods
////////////////////////////////////////////////////////////////////////

/***********************************************************************
 * Writes the specified byte of data to this output stream.
 * @see OutputStream#write(int)
 * @throws IOException if the <code>RandomAccessFile</code> does
 */
    public void write(int b) throws IOException
    {
	_file.write(b);
    }

/***********************************************************************
 * Writes <code>b.length</code> bytes of data to this output stream.
 * Calls <code>RandomAccessFile.write(b)</code>, <em>not</em>
 * <code>this.write(b, 0, b.length)</code>..
 * @see OutputStream#write(byte[])
 * @throws IOException if the <code>RandomAccessFile</code> does
 */
    public void write(byte[] b) throws IOException
    {
	_file.write(b);
    }

/***********************************************************************
 * Writes len bytes from the specified byte array starting at offset
 * <code>off</code> to this output stream.
 * @see OutputStream#write(byte[], int, int)
 * @throws IOException if the <code>RandomAccessFile</code> does
 */
    public void write(byte[] b, int off, int len) throws IOException
    {
	_file.write(b, off, len);
    }


/***********************************************************************
 * Closes this output stream and releases any system resources associated
 * with the stream.  In this case, closes the <code>RandomAccessFile</code>.
 * @see OutputStream#close()
 * @throws IOException if the <code>RandomAccessFile</code> does
 */
    public void close() throws IOException
    {
	_file.close();
    }

}

