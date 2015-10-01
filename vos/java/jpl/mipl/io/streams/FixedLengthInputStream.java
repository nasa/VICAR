package jpl.mipl.io.streams;

import java.io.*;

/**
 * This class wraps another <code>InputStream</code> and gives it a
 * fixed, maximum length.  Once the given number of bytes are read, this
 * class will return an End Of File regardless of whether or not the wrapped
 * input stream is at the end of file.
 * <p>
 * The maximum length can be changed dynamically. The next read operation
 * will succeed or fail based on the new maximum length.
 */

public class FixedLengthInputStream extends FilterInputStream
{

    protected long _max_size;
    protected long _bytes_read;
    protected long _mark_bytes;

/***********************************************************************
 * Creates a <code>FixedLengthInputStream</code> as a wrapper around the
 * given input stream.  Note that the max size must be set separately.
 * The default is no maximum.
 */
    public FixedLengthInputStream(InputStream in)
    {
	super(in);
	_max_size = -1;
	_bytes_read = 0;
    }

/***********************************************************************
 * Creates a <code>FixedLengthInputStream</code> as a wrapper around the
 * given input stream, using the given maximum size.
 */
    public FixedLengthInputStream(InputStream in, long max)
    {
	this(in);
	setMaximumSize(max);
    }

/***********************************************************************
 * Sets the maximum size of this input stream.  Subsequent reads will fail
 * if the byte count is larger than this maximum.  A maximum of -1 disables
 * the maximum feature, i.e. EOF will be returned only when the real EOF
 * of the input stream is reached.
 */
    public void setMaximumSize(long max)
    {
	_max_size = max;
    }

/***********************************************************************
 * Get the maximum size
 */
    public long getMaximumSize()
    {
	return _max_size;
    }

/***********************************************************************
 * Get the number of bytes read so far
 */
    public long getBytesRead()
    {
	return _bytes_read;
    }

////////////////////////////////////////////////////////////////////////
// FilterInputStream overridden methods
////////////////////////////////////////////////////////////////////////

/***********************************************************************
 * Reads the next byte of data from this input stream.  Checks the max
 * count and returns -1 if it is exceeded (or if the wrapped stream
 * reaches EOF).
 * @see FilterInputStream#read()
 * @throws IOException if the wrapped stream does
 */
    public int read() throws IOException
    {
	if (_max_size >= 0 && _bytes_read >= _max_size)
	    return -1;
	_bytes_read++;
	return in.read();
    }

/***********************************************************************
 * Reads up to len bytes of data from this input stream into an array of
 * bytes.
 * @return the total number of bytes read into the buffer, or -1 if
 * the end of the stream has been reached or the byte count exceeded.
 * @see FilterInputStream#read(byte[], int, int)
 * @throws IOException if the wrapped stream does
 */
    public int read(byte[] b, int off, int len) throws IOException
    {
	if (_max_size >= 0 && _bytes_read >= _max_size)	// nothing left
	    return -1;

	long space_left = _max_size - _bytes_read;
	long amt_to_read = len;

	if (_max_size >= 0 && len > space_left)		// must do partial read
	    amt_to_read = space_left;

	int n = in.read(b, off, (int)amt_to_read);
	_bytes_read += n;
	return n;
    }

/***********************************************************************
 * Skips over and discards n bytes of data from the input stream.
 * Returns the amount skipped.  Will not skip past the maximum bytes.
 * @see FilterInputStream#skip(long)
 * @throws IOException if the wrapped stream does
 */
    public long skip(long n) throws IOException
    {
	long space_left = _max_size - _bytes_read;
	long skip_amt = n;
	if (n > space_left)
	    skip_amt = space_left;
	if (_max_size <= 0)
	    skip_amt = n;		// no limit
	long skipped = in.skip(skip_amt);
	_bytes_read += skipped;
	return skipped;
    }

/***********************************************************************
 * Returns the number of bytes that can be read from this input stream
 * without blocking.
 * @see FilterInputStream#available()
 * @throws IOException if the wrapped stream does
 */
    public int available() throws IOException
    {
	int avail = in.available();
	if (_max_size < 0)
	    return avail;
	if (avail < _max_size - _bytes_read)
	    return avail;
	return (int)(_max_size - _bytes_read);
    }

/***********************************************************************
 * Marks the current position in this input stream.  If the wrapped
 * stream supports mark, we simply save and restore the read byte count.
 * @see FilterInputStream#mark(int)
 */
    public void mark(int readlimit)
    {
	in.mark(readlimit);
	_mark_bytes = _bytes_read;
    }

/***********************************************************************
 * Repositions this stream to the position at the time the <code>mark</code>
 * method was last called on this input stream.
 * @see FilterInputStream#reset()
 * @throws IOException if the wrapped stream does
 */
    public void reset() throws IOException
    {
	in.reset();
	_bytes_read = _mark_bytes;
    }

}

