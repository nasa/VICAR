package jpl.mipl.io.streams;

import java.io.*;

/**
 * This class wraps another <code>OutputStream</code> and gives it a
 * fixed, maximum length.  Once the given number of bytes are written, this
 * class will throw an End Of File exception regardless of whether or not
 * the wrapped output stream is at the end of file.  Note however that as
 * much data as possible is written before the EOF is thrown.
 * <p>
 * It would be preferable to return a flag value rather than throwing an
 * exception, but we're constrained by the signatures of the superclass.
 * <p>
 * The maximum length can be changed dynamically. The next write operation
 * will succeed or fail based on the new maximum length.
 * <p>
 * <code>write(byte[])</code>, <code>flush()</code>, and <code>close()</code>
 * are not overridden because the base class is sufficient.
 */

public class FixedLengthOutputStream extends FilterOutputStream
{

    protected long _max_size;
    protected long _bytes_written;

/***********************************************************************
 * Creates a <code>FixedLengthOutputStream</code> as a wrapper around the
 * given output stream.  Note that the max size must be set separately.
 * The default is no maximum.
 */
    public FixedLengthOutputStream(OutputStream out)
    {
	super(out);
	_max_size = -1;
	_bytes_written = 0;
    }

/***********************************************************************
 * Creates a <code>FixedLengthOutputStream</code> as a wrapper around the
 * given input stream, using the given maximum size.
 */
    public FixedLengthOutputStream(OutputStream out, long max)
    {
	this(out);
	setMaximumSize(max);
    }

/***********************************************************************
 * Sets the maximum size of this output stream.  Subsequent writes will fail
 * if the byte count is larger than this maximum.  A maximum of -1 disables
 * the maximum feature, i.e. EOF will not be returned unless the wrapped
 * output stream itself returns EOF (which is unusual).
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
 * Get the number of bytes written so far
 */
    public long getBytesWritten()
    {
	return _bytes_written;
    }

////////////////////////////////////////////////////////////////////////
// FilterOutputStream overridden methods
////////////////////////////////////////////////////////////////////////

/***********************************************************************
 * Writes the specified byte to the output stream.  Checks the max count
 * and throws EOF if it is exceeded.
 * @see FilterOutputStream#write(int)
 * @throws EOFException if the max byte count is reached.
 * @throws IOException if the wrapped stream does.
 */
    public void write(int b) throws IOException, EOFException
    {
	if (_max_size >= 0 && _bytes_written >= _max_size)
	    throw new EOFException("Virtual EOF of " + _max_size +
			" reached in FixedLengthOutputStream");
	_bytes_written++;
	out.write(b);
    }

/***********************************************************************
 * Writes len bytes from the specified byte array starting at offset off
 * to this output stream.  If the maximum number of bytes is reached, as
 * much as possible is written, then an EOF exception is thrown.
 * @see FilterOutputStream#write(byte[], int, int)
 * @throws EOFException if the max byte count is reached.
 * @throws IOException if the wrapped stream does
 */
    public void write(byte[] b, int off, int len) throws
						IOException, EOFException
    {
	if (_max_size >= 0 && _bytes_written >= _max_size)	// nothing left
	    throw new EOFException("Virtual EOF of " + _max_size +
			" reached in FixedLengthOutputStream");

	long space_left = _max_size - _bytes_written;
	long amt_to_write = len;

	if (_max_size >= 0 && len > space_left)		// must do partial write
	    amt_to_write = space_left;

	out.write(b, off, (int)amt_to_write);
	_bytes_written += amt_to_write;

	if (_max_size >= 0 && len > space_left)	// throw EOF for partial write
	    throw new EOFException("Virtual EOF of " + _max_size +
			" reached in FixedLengthOutputStream");
    }

}

