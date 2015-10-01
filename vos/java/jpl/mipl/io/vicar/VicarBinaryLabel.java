package jpl.mipl.io.vicar;

import jpl.mipl.io.streams.*;
import java.io.*;

/**
 * Manages a piece of Vicar Binary Label data, either the entire header or
 * one line's worth of prefix.  From this class, <code>DataInput</code> or
 * <code>DataOutput</code> objects may be returned which allow you to read
 * and/or write the data in the correct host representation.
 * <p>
 * These <code>DataInput</code> and <code>DataOutput</code> objects all
 * access the <em>same</em> internal buffer... to which you can also get
 * direct access.  It is up to the caller to synchronize any accesses to
 * this buffer.  It is possible to read something from the
 * <code>DataInput</code>, modify it, and put it back via the
 * <code>DataOutput</code>.  However, each stream maintains its own pointer
 * so you must write the same number of bytes as you read for them to stay
 * in sync.  And if the write gets ahead, the modified data will be seen by
 * the read.  In short, be careful about mixing modes.
 */

public class VicarBinaryLabel
{

/** The buffer holding the data */
    protected byte _buf[];

/** Specifies the format of the data */
    VicarDataFormat _fmt;

/***********************************************************************
 * Creates a new Binary Label of the specified length.  The buffer is
 * allocated here, and is never re-allocated.
 */
    public VicarBinaryLabel(int size, VicarDataFormat fmt)
    {
	if (size < 0)
	    throw new IllegalArgumentException("Negative label size: " + size);

	_buf = new byte[size];

	_fmt = fmt;
    }

/***********************************************************************
 * Returns the <code>VicarDataFormat</code> object representing this buffer.
 */
    public VicarDataFormat getVicarDataFormat()
    {
	return _fmt;
    }

/***********************************************************************
 * Returns the size of the buffer
 */
    public int size()
    {
	return _buf.length;
    }

/***********************************************************************
 * Returns the buffer directly.  It is up to the caller to mediate any
 * accesses to this buffer, either directly or via the
 * <code>getDataInput()</code> and <code>getDataOutput()</code> methods, as
 * the all refer to the <em>same</em> underlying buffer.
 */
    public byte[] getBuffer()
    {
	return _buf;
    }

/***********************************************************************
 * Returns an object that implements <code>DataInput</code>, which will
 * provide access to the buffer via that interface.  Data is translated
 * according to the <code>VicarDataFormat</code> object, i.e. data in the
 * buffer is in the (possibly foreign) format, while data returned by the
 * <code>DataInput</code> object is always in Java format.
 */
    public DataInput getDataInput()
    {
	return _fmt.getDataInputWrapper(new ByteArrayInputStream(_buf));
    }

/***********************************************************************
 * Returns an object that implements <code>DataOutput</code>, which will
 * provide access to the buffer via that interface.  Data is translated
 * according to the <code>VicarDataFormat</code> object, i.e. data in the
 * buffer is in the (possibly foreign) format, while data supplied to the
 * <code>DataOutput</code> object must always be in Java format.
 */
    public DataOutput getDataOutput()
    {
	return _fmt.getDataOutputWrapper(new FixedByteArrayOutputStream(_buf));
    }

}

