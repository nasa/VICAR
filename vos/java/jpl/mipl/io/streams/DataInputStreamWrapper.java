package jpl.mipl.io.streams;

import java.io.*;

/**
 * This class wraps an object implementing <code>DataInput</code> into an
 * <code>InputStream</code>.  Although most objects that implement
 * <code>DataInput</code> are already <code>InputStream</code>s, not all are.
 * Most notably, <code>javax.media.imageio.stream.ImageInputStream</code> from
 * the Image I/O package is this way.
 * <p>
 * Only the <code>readUnsignedByte()</code> method is used from the
 * <code>DataInput</code> object.
 * <p>
 * It's unbelievable that this is not in <code>java.io</code>!  And it's even
 * more unbelievable that <code>InputStream</code> is not an interface, thus
 * avoiding the need for all this garbage.
 * <p>
 * Since <code>DataInput</code> provides absolutely NO file positioning,
 * obviously <code>markSupported</code> returns false.  Thus this class is
 * recommended as a last resort only.
 * <p>
 * @see RandomAccessFileInputStream
 */

public class DataInputStreamWrapper extends InputStream
{

    protected DataInput _input;

/***********************************************************************
 * Creates a <code>DataInputStreamWrapper</code> as a wrapper around
 * the given object implementing <code>DataInput</code>.
 */
    public DataInputStreamWrapper(DataInput input)
    {
	_input = input;
    }

/***********************************************************************
 * Returns the <code>DataInput</code> object.
 */
    public DataInput getDataInput()
    {
	return _input;
    }

////////////////////////////////////////////////////////////////////////
// InputStream overridden methods
////////////////////////////////////////////////////////////////////////

/***********************************************************************
 * Reads the next byte of data from this input stream.
 * @see InputStream#read()
 * @throws IOException if the <code>DataInput</code> does
 */
    public int read() throws IOException
    {
	try {
	    return _input.readUnsignedByte();
	} catch (EOFException e) {
	    return -1;
	}
    }

/***********************************************************************
 * Returns false, since <code>mark()</code> is not supported.
 * @see InputStream#markSupported()
 */
    public boolean markSupported()
    {
	return false;
    }

}

