package jpl.mipl.io.streams;

import java.io.*;
import java.io.ByteArrayOutputStream;		// make javadoc @see happy

/**
 * This class implements an output stream in which the data is written into
 * a constant, fixed-length byte array.  The buffer does not grow, unlike
 * <code>ByteArrayOutputStream</code>.  Also, the buffer itself can be
 * retrieved, not just a copy (as in <code>ByteArrayOutputStream</code>).
 * <p>
 * Once the given number of bytes are written, this class will throw an
 * End Of File exception for any further attempts at writing.  Note however
 * that as much data as possible is written before the EOF is thrown.
 * <p>
 * This class <em>should</em> subclass <code>ByteArrayOutputStream</code>.
 * However, as another example of why the java.io package is so poorly
 * designed, the various write() methods in BAOS do not throw exceptions...
 * meaning that overrides in this class could not either.  This despite the
 * fact that the comments for ensureOpen() state that it *should* be throwing
 * an exception if the file is closed.  Sheesh.  So, this class derives directly
 * from OutputStream instead.  (additionally, ensureOpen() is private instead
 * of protected, so we can't do it "right" anyway).
 */

public class FixedByteArrayOutputStream extends OutputStream
{

/** The buffer for the data */
    protected byte _buf[];

/** The number of valid bytes in the buffer */
    protected int _count;

/** Flag indicating whether the stream has been closed. */
    protected boolean _isClosed = false;

/***********************************************************************
 * Creates a new byte array output stream of the specified length.  The
 * buffer is allocated here, and is never re-allocated.
 */
    public FixedByteArrayOutputStream(int size)
    {
	if (size < 0)
	    throw new IllegalArgumentException("Negative initial size: "+size);

	_buf = new byte[size];
	_count = 0;
    }

/***********************************************************************
 * Creates a new byte array output stream using the specified buffer.
 * It will not be reallocated or extended.
 */
    public FixedByteArrayOutputStream(byte buf[])
    {
	_buf = buf;
	_count = 0;
    }

/***********************************************************************
 * Returns the actual buffer used by this stream.  If you change it, the
 * consequences are your problem.
 */
    public byte[] getBuffer()
    {
	return _buf;
    }

/***********************************************************************
 * Get the maximum size
 */
    public long getMaximumSize()
    {
	return _buf.length;
    }

/***********************************************************************
 * Get the number of bytes written so far
 */
    public long getBytesWritten()
    {
	return _count;
    }

/***********************************************************************
 * Writes the specified byte to the byte array stream.  Checks the max count
 * and throws EOF if it is exceeded.
 * @see ByteArrayOutputStream#write(int)
 * @throws EOFException if the max byte count is reached.
 * @throws IOException if the stream has been closed.
 */
    public synchronized void write(int b) throws EOFException, IOException
    {
	ensureOpen();
	if (_count >= _buf.length) {
	    throw new EOFException("Virtual EOF of " + _buf.length +
			" reached in FixedByteArrayOutputStream");
	}
	_buf[_count] = (byte)b;
	_count++;
    }

/***********************************************************************
 * Writes len bytes from the specified byte array starting at offset off
 * to this byte array stream.  If the maximum number of bytes is reached, as
 * much as possible is written, then an EOF exception is thrown.
 * @see ByteArrayOutputStream#write(byte[], int, int)
 * @throws EOFException if the max byte count is reached.
 * @throws IOException if the stream has been closed.
 */
    public synchronized void write(byte[] b, int off, int len)
					throws EOFException, IOException
    {
	ensureOpen();
	// Base class checks lots of conditions to throw
	// IndexOutOfBoundsException but this seems like it should happen
	// naturally so why bother checking?

	if (len == 0)
	    return;

	// Check for exceeding the array.  We write as much as possible before
	// throwing the EOF.

	int write_len = len;
	if (_count + len > _buf.length)
	    write_len = _buf.length - _count;

	System.arraycopy(b, off, _buf, _count, write_len);
	_count += write_len;

	if (write_len != len)			// we hit EOF
	    throw new EOFException("Virtual EOF of " + _buf.length +
			" reached in FixedLengthOutputStream");
    }

/***********************************************************************
 * Resets the <code>_count</code> field of this byte array output stream
 * to zero, so that all currently accumulated output in the output stream
 * is "discarded".  The actual bytes in the array are not affected.
 * @throws IOException if the stream has been closed.
 */
    public synchronized void reset() throws IOException
    {
	ensureOpen();
	_count = 0;
    }

/***********************************************************************
 * Closes the output stream.  Closed streams cannot perform any operations
 * and cannot be reopened.
 */
    public synchronized void close() throws IOException {
	_isClosed = true;
    }

/***********************************************************************
 * This should be protected, not private, in the base class...
 * <p>
 * Check to make sure that the stream has not been closed.  This method
 * does nothing for now.  Once we add throws clauses to the I/O methods
 * in this class, it will throw an IOException if the stream has been closed.
 * @throws IOException if the file has been closed
 */
    protected void ensureOpen() throws IOException {
	if (_isClosed)
	    throw new IOException("FixedByteArrayOutputStream has been closed");
    }

}

