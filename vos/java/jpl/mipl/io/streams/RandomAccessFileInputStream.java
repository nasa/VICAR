package jpl.mipl.io.streams;

import java.io.*;

/**
 * This class wraps a <code>RandomAccessFile</code> into an InputStream.
 * It's unbelievable that this is not in <code>java.io</code>!  Note that
 * this is almost exactly what is in
 * <code>com.sun.media.jai.codec.FileSeekableStream</code>, but we have our
 * own in case the codec classes are not available.
 */

public class RandomAccessFileInputStream extends InputStream
{

    protected RandomAccessFile _file;
    protected long _mark_pos;

/***********************************************************************
 * Creates a <code>RandomAccessFileInputStream</code> as a wrapper around
 * the given <code>RandomAccessFile</code>.
 */
    public RandomAccessFileInputStream(RandomAccessFile file)
    {
	_file = file;
	_mark_pos = -1;
    }

/***********************************************************************
 * Returns the <code>RandomAccessFile</code> object.
 */
    public RandomAccessFile getFile()
    {
	return _file;
    }

////////////////////////////////////////////////////////////////////////
// InputStream overridden methods
////////////////////////////////////////////////////////////////////////

/***********************************************************************
 * Reads the next byte of data from this input stream.
 * @see InputStream#read()
 * @throws IOException if the <code>RandomAccessFile</code> does
 */
    public int read() throws IOException
    {
	return _file.read();
    }

/***********************************************************************
 * Reads up to len bytes of data from this input stream into an array of
 * bytes.
 * @return the total number of bytes read into the buffer, or -1 if
 * the end of the stream has been reached.
 * @see InputStream#read(byte[], int, int)
 * @throws IOException if the <code>RandomAccessFile</code> does
 */
    public int read(byte[] b, int off, int len) throws IOException
    {
	return _file.read(b, off, len);
    }

/***********************************************************************
 * Skips over and discards n bytes of data from the input stream.
 * Returns the amount skipped.  Will not skip past the maximum bytes.
 * @see InputStream#skip(long)
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
 * Returns the number of bytes that can be read from this input stream
 * without blocking.
 * @see InputStream#available()
 * @throws IOException if the <code>RandomAccessFile</code> does
 */
    public int available() throws IOException
    {
	return (int)(_file.length() - _file.getFilePointer());
    }

/***********************************************************************
 * Closes this input stream and releases any system resources associated
 * with the stream.  In this case, closes the <code>RandomAccessFile</code>.
 * @see InputStream#close()
 * @throws IOException if the <code>RandomAccessFile</code> does
 */
    public void close() throws IOException
    {
	_file.close();
    }

/***********************************************************************
 * Marks the current position in this input stream.  Since we're dealing
 * with a <code>RandomAccessFile</code>, simply remember the position
 * to seek to later.
 * <p>
 * Because <code>file.getFilePointer()</code> could possibly throw an
 * exception, we catch it here and set the mark value to -1 (i.e. invalid)
 * so a subsequent <code>reset()</code> would fail.  Since <code>mark</code>
 * is not defined to throw exceptions, there's not much else we can do.
 * @see InputStream#mark(int)
 */
    public void mark(int readlimit)
    {
	try {
	    _mark_pos = _file.getFilePointer();
	} catch (IOException e) {
	    _mark_pos = -1;
	}
    }

/***********************************************************************
 * Repositions this stream to the position at the time the <code>mark</code>
 * method was last called on this input stream.
 * @see InputStream#reset()
 * @throws IOException if the <code>RandomAccessFile</code> does
 */
    public void reset() throws IOException
    {
	if (_mark_pos < 0)
	    throw new IOException(
	   "RandomAccessFileInputStream: mark() must be called before reset()");

	_file.seek(_mark_pos);
    }

/***********************************************************************
 * Returns true, since <code>mark()</code> is supported.
 * @see InputStream#markSupported()
 */
    public boolean markSupported()
    {
	return true;
    }

}

