package jpl.mipl.io.streams;

import java.io.*;
import javax.imageio.stream.*;

/**
 * This class implements the multi-element read functions of
 * <code>ImageInputStream</code> but with the addition of an extra
 * parameter, which specifies the pixel stride in the array.  This
 * allows one to read directly into an array (usually in a
 * <code>DataBuffer</code>) where the pixels are not contiguous in memory.
 * For example, a pixel interleaved array could be read from a band
 * sequential file by reading into every 3rd array element.
 * <p>
 * It would be nice to actually implement <code>ImageInputStream</code>
 * (or extend <code>ImageInputStreamImpl</code>) in this class.  However,
 * to maintain JDK 1.3 compatibility, we can't do this (yet).  That may
 * be a future enhancement.
 * <p>
 * An additional enhancement over ImageInputStream is that we can handle
 * VAX-format reals in addition to forward and reverse IEEE reals.  They
 * are converted to Java reals (IEEE) as they are read in.
 * <p>
 * This class tries to be as efficient as possible.  For example, if the
 * source object actually is an <code>ImageInputStream</code> and the
 * pixel stride is 1, we call the ImageInputStream methods directly.
 * <p>
 * Allowable input sources in order of preference are:
 * <ul>
 * <li>ImageInputStream</li>
 * <li>DataInput</li>
 * <li>InputStream</li>
 * </ul>
 * Note that <code>RandomAccessFile</code> is a <code>DataInput</code>
 * and thus is legal.
 * <p>
 * Large portions of this code were shamelessly ripped off from
 * <code>ImageInputStream</code>...
 */

public class ImageInputStreamStride
{
    /** Constant for big-endian byte order.  Corresponds to HIGH and IEEE
     * in VICAR. */
    public static final int HIGH_ORDER = 0;

    /** Constant for little-endian byte order.  Corresponds to LOW and RIEEE
     * in VICAR. */
    public static final int LOW_ORDER = 1;

    /** Constant for VAX format, which is little-endian for ints and its own
     * funky thing for reals.  */
    public static final int VAX_ORDER = 2;

    // Only one of these will be non-null at any given time...

    protected ImageInputStream _iis = null;
    protected DataInput _di = null;
    protected InputStream _is = null;

    protected int _int_order;
    protected int _float_order;

    // True iff _iis is non-null and the byte order agrees with what we have
    // (so we can do direct multi-pixel reads)

    protected boolean _direct_int_okay = false;
    protected boolean _direct_float_okay = false;

    // Buffer used to read bytes before converting and stuffing into arrays
    protected static final int BYTE_BUF_LENGTH = 8192;
    protected byte[] _byteBuf = new byte[BYTE_BUF_LENGTH];

/***********************************************************************
 * Constructor with just one order applying to both ints and floats.  The
 * Object argument is the stream (one of several types, see class comments)
 * that'll do the actual work.  
 */
    public ImageInputStreamStride(Object stream, int order)
    {
	this(stream, order, order);
    }

/***********************************************************************
 * Constructor with different byte orders for ints and floats.  99.9999%
 * of the time they'll be the same but since VICAR has different variables
 * holding the ordering, so do we...
 * <p>
 * A <code>null</code> stream is legal for use by subclasses like
 * <code>ImageOutputStreamStride</code>, but not too useful elsewhere since
 * there's no facility to re-set the stream object and any read call will
 * fail with a <code>NullPointerException</code>.
 */
    public ImageInputStreamStride(Object stream, int int_order, int float_order)
    {
	_int_order = int_order;
	if (_int_order == VAX_ORDER)
	    _int_order = LOW_ORDER;
	_float_order = float_order;

	if (stream == null)
	    return;			// special case for output streams

	if (stream instanceof ImageInputStream) {
	    _iis = (ImageInputStream)stream;

	    // Check to see if we can do direct I/O using the IIS.  The
	    // endian-ness must match what's set in the stream.

	    if (_iis.getByteOrder() == java.nio.ByteOrder.LITTLE_ENDIAN) {
		if (_int_order == LOW_ORDER)
		    _direct_int_okay = true;
		if (_float_order == LOW_ORDER)
		    _direct_float_okay = true;
	    } else {				// must be BIG_ENDIAN
		if (_int_order == HIGH_ORDER)
		    _direct_int_okay = true;
		if (_float_order == HIGH_ORDER)
		    _direct_float_okay = true;
	    }
	}
	else if (stream instanceof DataInput)
	    _di = (DataInput)stream;
	else if (stream instanceof InputStream)
	    _is = (InputStream)stream;
	else {
	    throw new IllegalArgumentException("Unrecognized stream type " +
		(stream == null ? "(null)" : stream.getClass().toString()) +
		" in ImageInputStreamStride constructor");
	}
    }

/***********************************************************************
 * Reads <code>length</code> bytes from the stream's current position into
 * the array <code>b</code> starting at index <code>offset</code>.  Only
 * every <code>pixelStride</code>th array element is filled in.  In other
 * words, consecutive bytes are read from the stream but they can be scattered
 * in the output array.
 * <p>
 * Pixels are converted to internal Java format as they are read.  (not
 * really applicable to bytes though).
 *
 * @throws IOException if the stream does
 * @see ImageInputStream.readFully(byte[], int, int)
 */

    public void readBytes(byte[] b, int off, int len, int pixelStride)
				throws IOException
    {
	if (pixelStride == 1) {		// special case for efficiency
	    readFully(b, off, len);
	    return;
	}

	while (len > 0) {
	    int nelts = Math.min(len, _byteBuf.length);
	    readFully(_byteBuf, 0, nelts);
	    for (int i=0; i < nelts; i++)
		b[off+(i*pixelStride)] = _byteBuf[i];
	    off += nelts*pixelStride;
	    len -= nelts;
	}
    }

/***********************************************************************
 * Reads <code>length</code> shorts from the stream's current position into
 * the array <code>s</code> starting at index <code>offset</code>.  Only
 * every <code>pixelStride</code>th array element is filled in.  In other
 * words, consecutive shorts are read from the stream but they can be scattered
 * in the output array.
 * <p>
 * Pixels are converted to internal Java format as they are read.
 *
 * @throws IOException if the stream does
 * @see ImageInputStream.readFully(byte[], int, int)
 */

    public void readShorts(short[] s, int off, int len, int pixelStride)
				throws IOException
    {
	if (pixelStride == 1 && _direct_int_okay) { // special case, must be IIS
	    _iis.readFully(s, off, len);
	    return;
	}

	while (len > 0) {
	    int nelts = Math.min(len, _byteBuf.length/2);
	    readFully(_byteBuf, 0, nelts*2);
	    toShorts(_byteBuf, s, off, nelts, pixelStride);
	    off += nelts*pixelStride;
	    len -= nelts;
	}
    }

/***********************************************************************
 * Reads <code>length</code> ushorts from the stream's current position into
 * the array <code>u</code> starting at index <code>offset</code>.  Only
 * every <code>pixelStride</code>th array element is filled in.  In other
 * words, consecutive ushorts are read from the stream but they can be scattered
 * in the output array.
 * <p>
 * Pixels are converted to internal Java format as they are read.
 * <p>
 * This is really the same as readShorts() since the UShorts are stored in
 * a short array anyway.  We just have a separate routine in case it matters
 * sometime.
 *
 * @throws IOException if the stream does
 * @see ImageInputStream.readFully(byte[], int, int)
 */

    public void readUShorts(short[] u, int off, int len, int pixelStride)
				throws IOException
    {
	readShorts(u, off, len, pixelStride);
    }

/***********************************************************************
 * Reads <code>length</code> ints from the stream's current position into
 * the array <code>i</code> starting at index <code>offset</code>.  Only
 * every <code>pixelStride</code>th array element is filled in.  In other
 * words, consecutive ints are read from the stream but they can be scattered
 * in the output array.
 * <p>
 * Pixels are converted to internal Java format as they are read.
 *
 * @throws IOException if the stream does
 * @see ImageInputStream.readFully(byte[], int, int)
 */

    public void readInts(int[] i, int off, int len, int pixelStride)
				throws IOException
    {
	if (pixelStride == 1 && _direct_int_okay) { // special case, must be IIS
	    _iis.readFully(i, off, len);
	    return;
	}

	while (len > 0) {
	    int nelts = Math.min(len, _byteBuf.length/4);
	    readFully(_byteBuf, 0, nelts*4);
	    toInts(_byteBuf, i, off, nelts, pixelStride);
	    off += nelts*pixelStride;
	    len -= nelts;
	}
    }

/***********************************************************************
 * Reads <code>length</code> longs from the stream's current position into
 * the array <code>l</code> starting at index <code>offset</code>.  Only
 * every <code>pixelStride</code>th array element is filled in.  In other
 * words, consecutive longs are read from the stream but they can be scattered
 * in the output array.
 * <p>
 * Pixels are converted to internal Java format as they are read.
 *
 * @throws IOException if the stream does
 * @see ImageInputStream.readFully(byte[], int, int)
 */

    public void readLongs(long[] l, int off, int len, int pixelStride)
				throws IOException
    {
	if (pixelStride == 1 && _direct_int_okay) { // special case, must be IIS
	    _iis.readFully(l, off, len);
	    return;
	}

	while (len > 0) {
	    int nelts = Math.min(len, _byteBuf.length/8);
	    readFully(_byteBuf, 0, nelts*8);
	    toLongs(_byteBuf, l, off, nelts, pixelStride);
	    off += nelts*pixelStride;
	    len -= nelts;
	}
    }

/***********************************************************************
 * Reads <code>length</code> floats from the stream's current position into
 * the array <code>f</code> starting at index <code>offset</code>.  Only
 * every <code>pixelStride</code>th array element is filled in.  In other
 * words, consecutive floats are read from the stream but they can be scattered
 * in the output array.
 * <p>
 * Pixels are converted to internal Java format as they are read.
 *
 * @throws IOException if the stream does
 * @see ImageInputStream.readFully(byte[], int, int)
 */

    public void readFloats(float[] f, int off, int len, int pixelStride)
				throws IOException
    {
	if (pixelStride == 1 && _direct_float_okay) { //special case,must be IIS
	    _iis.readFully(f, off, len);
	    return;
	}

	while (len > 0) {
	    int nelts = Math.min(len, _byteBuf.length/4);
	    readFully(_byteBuf, 0, nelts*4);
	    toFloats(_byteBuf, f, off, nelts, pixelStride);
	    off += nelts*pixelStride;
	    len -= nelts;
	}
    }

/***********************************************************************
 * Reads <code>length</code> doubles from the stream's current position into
 * the array <code>d</code> starting at index <code>offset</code>.  Only
 * every <code>pixelStride</code>th array element is filled in.  In other
 * words, consecutive doubles are read from the stream but they can be scattered
 * in the output array.
 * <p>
 * Pixels are converted to internal Java format as they are read.
 *
 * @throws IOException if the stream does
 * @see ImageInputStream.readFully(byte[], int, int)
 */

    public void readDoubles(double[] d, int off, int len, int pixelStride)
				throws IOException
    {
	if (pixelStride == 1 && _direct_float_okay) { //special case,must be IIS
	    _iis.readFully(d, off, len);
	    return;
	}

	while (len > 0) {
	    int nelts = Math.min(len, _byteBuf.length/8);
	    readFully(_byteBuf, 0, nelts*8);
	    toDoubles(_byteBuf, d, off, nelts, pixelStride);
	    off += nelts*pixelStride;
	    len -= nelts;
	}
    }

/***********************************************************************
 * Internal routine to read a bunch of bytes into an array.  Needed because
 * everyone's readFully() routine is different, and InputStream doesn't
 * even have one (read() can return fewer bytes).
 */
    protected void readFully(byte[] b, int off, int len) throws IOException
    {
	if (_iis != null) {
	    _iis.readFully(b, off, len);
	    return;
	}
	if (_di != null) {
	    _di.readFully(b, off, len);
	    return;
	}

	// We have an InputStream.  Gotta do a loop...
	// Shamelessly ripped off from DataInputStream.readFully()

	if (len < 0)
	    throw new IndexOutOfBoundsException();
	int n = 0;
	while (n < len) {
	    int count = _is.read(b, off + n, len - n);
	    if (count < 0)
		throw new EOFException();
	    n += count;
	}
    }

/***********************************************************************
 * Convert a byte array buffer to an array of shorts.
 */
    protected void toShorts(byte[] b, short[] s, int off, int len,
							int pixelStride)
    {
        int boff = 0;
        if (_int_order == HIGH_ORDER) {
            for (int j = 0; j < len; j++) {
                int b0 = b[boff];
                int b1 = b[boff + 1] & 0xff;
                s[off + j*pixelStride] = (short)((b0 << 8) | b1);
                boff += 2;
            }
        } else {
            for (int j = 0; j < len; j++) {
                int b0 = b[boff + 1];
                int b1 = b[boff] & 0xff;
                s[off + j*pixelStride] = (short)((b0 << 8) | b1);
                boff += 2;
            }
        }
    }

/***********************************************************************
 * Convert a byte array buffer to an array of ints.
 */
    protected void toInts(byte[] b, int[] i, int off, int len,
							int pixelStride)
    {
        int boff = 0;
        if (_int_order == HIGH_ORDER) {
            for (int j = 0; j < len; j++) {
                int b0 = b[boff];
                int b1 = b[boff + 1] & 0xff;
                int b2 = b[boff + 2] & 0xff;
                int b3 = b[boff + 3] & 0xff;
                i[off + j*pixelStride] =
				(b0 << 24) | (b1 << 16) | (b2 << 8) | b3;
                boff += 4;
            }
        } else {
            for (int j = 0; j < len; j++) {
                int b0 = b[boff + 3];
                int b1 = b[boff + 2] & 0xff;
                int b2 = b[boff + 1] & 0xff;
                int b3 = b[boff] & 0xff;
                i[off + j*pixelStride] =
				(b0 << 24) | (b1 << 16) | (b2 << 8) | b3;
                boff += 4;
            }
        }
    }

/***********************************************************************
 * Convert a byte array buffer to an array of longs.
 */
    protected void toLongs(byte[] b, long[] l, int off, int len,
							int pixelStride)
    {

        int boff = 0;
        if (_int_order == HIGH_ORDER) {
            for (int j = 0; j < len; j++) {
                int b0 = b[boff];
                int b1 = b[boff + 1] & 0xff;
                int b2 = b[boff + 2] & 0xff;
                int b3 = b[boff + 3] & 0xff;
                int b4 = b[boff + 4];
                int b5 = b[boff + 5] & 0xff;
                int b6 = b[boff + 6] & 0xff;
                int b7 = b[boff + 7] & 0xff;

                int i0 = (b0 << 24) | (b1 << 16) | (b2 << 8) | b3;
                int i1 = (b4 << 24) | (b5 << 16) | (b6 << 8) | b7;

                l[off + j*pixelStride] = ((long)i0 << 32) | (i1 & 0xffffffffL);
                boff += 8;
            }
        } else {
            for (int j = 0; j < len; j++) {
                int b0 = b[boff + 7];
                int b1 = b[boff + 6] & 0xff;
                int b2 = b[boff + 5] & 0xff;
                int b3 = b[boff + 4] & 0xff;
                int b4 = b[boff + 3];
                int b5 = b[boff + 2] & 0xff;
                int b6 = b[boff + 1] & 0xff;
                int b7 = b[boff]     & 0xff;

                int i0 = (b0 << 24) | (b1 << 16) | (b2 << 8) | b3;
                int i1 = (b4 << 24) | (b5 << 16) | (b6 << 8) | b7;

                l[off + j*pixelStride] = ((long)i0 << 32) | (i1 & 0xffffffffL);
                boff += 8;
            }
        }
    }

/***********************************************************************
 * Convert a byte array buffer to an array of floats.
 */
    protected void toFloats(byte[] b, float[] f, int off, int len,
							int pixelStride)
    {
        int boff = 0;
        if (_float_order == HIGH_ORDER) {
            for (int j = 0; j < len; j++) {
                int b0 = b[boff];
                int b1 = b[boff + 1] & 0xff;
                int b2 = b[boff + 2] & 0xff;
                int b3 = b[boff + 3] & 0xff;
                int i = (b0 << 24) | (b1 << 16) | (b2 << 8) | b3;
                f[off + j*pixelStride] = Float.intBitsToFloat(i);
                boff += 4;
            }
        } else if (_float_order == LOW_ORDER) {
            for (int j = 0; j < len; j++) {
                int b0 = b[boff + 3];
                int b1 = b[boff + 2] & 0xff;
                int b2 = b[boff + 1] & 0xff;
                int b3 = b[boff + 0] & 0xff;
                int i = (b0 << 24) | (b1 << 16) | (b2 << 8) | b3;
                f[off + j*pixelStride] = Float.intBitsToFloat(i);
                boff += 4;
            }

        } else {				// VAX_ORDER
            for (int j = 0; j < len; j++) {

		// first, read the bytes in rational order:

		int vax1 = b[boff + 0] & 0xff;
		int vax0 = b[boff + 1] & 0xff;
		int vax3 = b[boff + 2] & 0xff;
		int vax2 = b[boff + 3] & 0xff;

		int ieee0 = vax0;
		int ieee1 = vax1;
		int ieee2 = vax2;
		int ieee3 = vax3;

		int exp = ((vax0<<1) & 0xFE) | ((vax1>>7) & 0x01);

		if (exp == 0) {		// Zero or invalid pattern
		    if ((vax0 & 0x80) != 0) {  // Sign bit set (illegal for VAX)
			ieee0 = 0x7F;		// _IEEEFLOAT NaN
			ieee1 = 0xFF;
			ieee2 = 0xFF;
			ieee3 = 0xFF;
		    }
		    else {		// Zero
			ieee0 = ieee1 = ieee2 = ieee3 = 0;
		    }
		}

		else if (exp >= 3) {	// Normal case
		    exp -= 2;
		    ieee0 = (vax0 & 0x80) | ((exp>>1)&0x7F);
		    // remake sign + exponent
		}	// Low bit of exp can't change, so don't bother w/it

		else if (exp == 2) {	// Denormalize the number
		    // Shift right one
		    ieee3 = ((ieee3>>1) & 0x7F) | ((ieee2<<7) & 0x80);
		    ieee2 = ((ieee2>>1) & 0x7F) | ((ieee1<<7) & 0x80);
		    ieee1 = ((ieee1>>1) & 0x7F);
		    // Add suppressed most significant bit
		    ieee1 = (ieee1 & 0x3F) | 0x40;
		    // and set exponent to 0 (preserving sign)
		    ieee0 = vax0 & 0x80;
		}

		else {			// Exp==1, denormalize again
		    // Shift right two
		    ieee3 = ((ieee3>>2) & 0x3F) | ((ieee2<<6) & 0xC0);
		    ieee2 = ((ieee2>>2) & 0x3F) | ((ieee1<<6) & 0xC0);
		    ieee1 = ((ieee1>>2) & 0x7F);
		    ieee1 = (ieee1 & 0x1F) | 0x20;
		    ieee0 = vax0 & 0x80;
		}

		// package it up

                int i = (ieee0 << 24) | (ieee1 << 16) | (ieee2 << 8) | ieee3;

                f[off + j*pixelStride] = Float.intBitsToFloat(i);
                boff += 4;
            }
        }
    }

/***********************************************************************
 * Convert a byte array buffer to an array of doubles.
 */
    protected void toDoubles(byte[] b, double[] d, int off, int len,
							int pixelStride)
    {
        int boff = 0;
        if (_float_order == HIGH_ORDER) {
            for (int j = 0; j < len; j++) {
                int b0 = b[boff];
                int b1 = b[boff + 1] & 0xff;
                int b2 = b[boff + 2] & 0xff;
                int b3 = b[boff + 3] & 0xff;
                int b4 = b[boff + 4];
                int b5 = b[boff + 5] & 0xff;
                int b6 = b[boff + 6] & 0xff;
                int b7 = b[boff + 7] & 0xff;

                int i0 = (b0 << 24) | (b1 << 16) | (b2 << 8) | b3;
                int i1 = (b4 << 24) | (b5 << 16) | (b6 << 8) | b7;
                long l = ((long)i0 << 32) | (i1 & 0xffffffffL);

                d[off + j*pixelStride] = Double.longBitsToDouble(l);
                boff += 8;
            }
        } else if (_float_order == LOW_ORDER) {
            for (int j = 0; j < len; j++) {
                int b0 = b[boff + 7];
                int b1 = b[boff + 6] & 0xff;
                int b2 = b[boff + 5] & 0xff;
                int b3 = b[boff + 4] & 0xff;
                int b4 = b[boff + 3];
                int b5 = b[boff + 2] & 0xff;
                int b6 = b[boff + 1] & 0xff;
                int b7 = b[boff] & 0xff;

                int i0 = (b0 << 24) | (b1 << 16) | (b2 << 8) | b3;
                int i1 = (b4 << 24) | (b5 << 16) | (b6 << 8) | b7;
                long l = ((long)i0 << 32) | (i1 & 0xffffffffL);

                d[off + j*pixelStride] = Double.longBitsToDouble(l);
                boff += 8;
            }

        } else {				// VAX_ORDER
            for (int j = 0; j < len; j++) {

		// first, read the bytes in rational order:

		int vax1 = b[boff + 0] & 0xff;
		int vax0 = b[boff + 1] & 0xff;
		int vax3 = b[boff + 2] & 0xff;
		int vax2 = b[boff + 3] & 0xff;
		int vax5 = b[boff + 4] & 0xff;
		int vax4 = b[boff + 5] & 0xff;
		int vax7 = b[boff + 6] & 0xff;
		int vax6 = b[boff + 7] & 0xff;

		int ieee0 = vax0;
		int ieee1 = vax1;
		int ieee2 = vax2;
		int ieee3 = vax3;
		int ieee4 = vax4;
		int ieee5 = vax5;
		int ieee6 = vax6;
		int ieee7 = vax7;

		int exp = ((vax0<<1) & 0xFE) | ((vax1>>7) & 0x01);

		if (exp == 0) {		// Zero or invalid pattern
		    if ((vax0 & 0x80) != 0) {  // Sign bit set (illegal for VAX)
			ieee0 = 0x7F;		// _IEEEFLOAT NaN
			ieee1 = 0xFF;
			ieee2 = 0xFF;
			ieee3 = 0xFF;
			ieee4 = ieee5 = ieee6 = ieee7 = 0xFF;
		    }
		    else {		// Zero
			ieee0 = ieee1 = ieee2 = ieee3 = 0;
			ieee4 = ieee5 = ieee6 = ieee7 = 0;
		    }
		}

		else {				// Normal case
		    exp += 894;			// Change to IEEE bias
		    int exph = (exp>>4) & 0x7F; // convert to unsigned chars
		    int expl = (exp<<4) & 0xF0;

		    // Shift mantissa by 3 (rounds to 0)
		    ieee7 = ((ieee7>>3) & 0x1F) | ((ieee6<<5) & 0xE0);
		    ieee6 = ((ieee6>>3) & 0x1F) | ((ieee5<<5) & 0xE0);
		    ieee5 = ((ieee5>>3) & 0x1F) | ((ieee4<<5) & 0xE0);
		    ieee4 = ((ieee4>>3) & 0x1F) | ((ieee3<<5) & 0xE0);
		    ieee3 = ((ieee3>>3) & 0x1F) | ((ieee2<<5) & 0xE0);
		    ieee2 = ((ieee2>>3) & 0x1F) | ((ieee1<<5) & 0xE0);
		    ieee1 = ((ieee1>>3) & 0x1F);

		    // put back exponent and sign
		    ieee0 = (vax0 & 0x80) | exph;
		    ieee1 = (ieee1 & 0x0F) | expl;
		}

		// package it up

                long l = (ieee0 << 56) | (ieee1 << 48) |
			 (ieee2 << 40) | (ieee3 << 32) |
			 (ieee4 << 24) | (ieee5 << 16) |
			 (ieee6 <<  8) | (ieee7);

                d[off + j*pixelStride] = Double.longBitsToDouble(l);
                boff += 8;
            }
        }
    }
}

