package jpl.mipl.io.streams;

import java.io.*;

/**
 * This class wraps an object implementing <code>DataOutput</code> into an
 * <code>OutputStream</code>.  Although most objects that implement
 * <code>DataOutput</code> are already <code>OutputStream</code>s, not all are.
 * Most notably, <code>javax.media.imageio.stream.ImageOutputStream</code> from
 * the Image I/O package is this way.
 * <p>
 * Only the three <code>write()</code> methods are used from the
 * <code>DataOutput</code> object.
 * <p>
 * It's unbelievable that this is not in <code>java.io</code>!  And it's even
 * more unbelievable that <code>OutputStream</code> is not an interface, thus
 * avoiding the need for all this garbage.
 * <p>
 * Since <code>DataOutput</code> provides absolutely <em>nothing</em> in the
 * way of flushing or closing, it is assumed that the <code>DataOutput</code>
 * object takes care of that on its own (or the application does it directly,
 * outside of this wrapper).  Thus the <code>flush</code> and <code>close</code>
 * methods of this wrapper are no-ops.  Thus this class is recommended as
 * a last resort only.
 */

public class DataOutputStreamWrapper extends OutputStream
{

    protected DataOutput _output;

/***********************************************************************
 * Creates a <code>DataOutputStreamWrapper</code> as a wrapper around
 * the given object implementing <code>DataOutput</code>.
 */
    public DataOutputStreamWrapper(DataOutput output)
    {
	_output = output;
    }

/***********************************************************************
 * Returns the <code>DataOutput</code> object.
 */
    public DataOutput getDataOutput()
    {
	return _output;
    }

////////////////////////////////////////////////////////////////////////
// OutputStream overridden methods
////////////////////////////////////////////////////////////////////////

/***********************************************************************
 * Writes the next byte of data to this output stream.
 * @see java.io.OutputStream#write(int)
 * @throws IOException if the <code>DataOutput</code> does
 */
    public void write(int b) throws IOException
    {
	_output.write(b);
    }

/***********************************************************************
 * Writes an array of bytes to this output stream.
 * @see java.io.OutputStream
 * @throws IOException if the <code>DataOutput</code> does
 */
    public void write(byte[] b) throws IOException
    {
	_output.write(b);
    }

/***********************************************************************
 * Writes part of an array of bytes to this output stream.
 * @see java.io.OutputStream#write(byte[])
 * @throws IOException if the <code>DataOutput</code> does
 */
    public void write(byte[] b, int off, int len) throws IOException
    {
	_output.write(b, off, len);
    }

}

