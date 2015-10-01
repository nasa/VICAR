package jpl.mipl.io.streams;

import java.io.*;
import javax.imageio.stream.*;

/**
 * This class implements the multi-element write functions of
 * <code>ImageOutputStream</code> but with the addition of an extra
 * parameter, which specifies the pixel stride in the array.  This
 * allows one to write directly from an array (usually in a
 * <code>DataBuffer</code>) where the pixels are not contiguous in memory.
 * For example, a pixel interleaved array could be written to a band
 * sequential file by writing from every 3rd array element.
 * <p>
 * It would be nice to actually implement <code>ImageOutputStream</code>
 * (or extend <code>ImageOutputStreamImpl</code>) in this class.  However,
 * to maintain JDK 1.3 compatibility, we can't do this (yet).  That may
 * be a future enhancement.
 * <p>
 * An additional enhancement over ImageOutputStream is that we can handle
 * VAX-format reals in addition to forward and reverse IEEE reals.  They
 * are converted from Java reals (IEEE) as they are written out.
 * <p>
 * This class tries to be as efficient as possible.  For example, if the
 * source object actually is an <code>ImageOutputStream</code> and the
 * pixel stride is 1, we call the ImageOutputStream methods directly.
 * <p>
 * Allowable output sinks in order of preference are:
 * <ul>
 * <li>ImageOutputStream</li>
 * <li>DataOutput</li>
 * <li>OutputStream</li>
 * </ul>
 * Note that <code>RandomAccessFile</code> is a <code>DataOutput</code>
 * and thus is legal.
 * <p>
 * Although we extend <code>ImageInputStreamStride</code>, that's only
 * useful for objects that implement both an input and an output interface
 * (e.g. <code>ImageOutputStream</code>, <code>RandomAccessFile</code>).
 * Output-only objects will cause a <code>NullPointerException</code> to be
 * thrown from any of the input routines.
 * <p>
 * Large portions of this code were shamelessly ripped off from
 * <code>ImageOutputStream</code>...
 */

public class ImageOutputStreamStride extends ImageInputStreamStride
{
    // Only one of these will be non-null at any given time...

    protected ImageOutputStream _ios = null;
    protected DataOutput _do = null;
    protected OutputStream _os = null;

/***********************************************************************
 * Constructor with just one order applying to both ints and floats.  The
 * Object argument is the stream (one of several types, see class comments)
 * that'll do the actual work.  
 */
    public ImageOutputStreamStride(Object stream, int order)
    {
	this(stream, order, order);
    }

/***********************************************************************
 * Constructor with different byte orders for ints and floats.  99.9999%
 * of the time they'll be the same but since VICAR has different variables
 * holding the ordering, so do we...
 */
    public ImageOutputStreamStride(Object stream, int int_order,int float_order)
    {
	// Why the $#@%@#^@#^ does Java insist on super() being the first
	// bloody statement in the routine??!  It's annoying!!!  An if
	// statement here would be SOOO much clearer...  :-(

	super(
	    (stream instanceof ImageInputStream || stream instanceof DataInput)
	    ? stream : null,
	    int_order, float_order);

	if (stream instanceof ImageOutputStream) {
	    _ios = (ImageOutputStream)stream;

	    // The superclass sets up _direct_int/float_okay for us...
	}
	else if (stream instanceof DataOutput)
	    _do = (DataOutput)stream;
	else if (stream instanceof OutputStream)
	    _os = (OutputStream)stream;
	else {
	    throw new IllegalArgumentException("Unrecognized stream type " +
		(stream == null ? "(null)" : stream.getClass().toString()) +
		" in ImageOutputStreamStride constructor");
	}
    }

/***********************************************************************
 * Writes <code>length</code> bytes to the stream's current position from
 * the array <code>b</code> starting at index <code>offset</code>.  Only
 * every <code>pixelStride</code>th array element is written out.  In other
 * words, consecutive bytes are written to the stream but they can be
 * scattered in the input array.
 * <p>
 * Pixels are converted from internal Java format as they are written.  (not
 * really applicable to bytes though).
 *
 * @throws IOException if the stream does
 * @see ImageOutputStream.write(byte[], int, int)
 */

    public void writeBytes(byte[] b, int off, int len, int pixelStride)
				throws IOException
    {
	if (pixelStride == 1) {		// special case for efficiency
	    write(b, off, len);
	    return;
	}

	while (len > 0) {
	    int nelts = Math.min(len, _byteBuf.length);
	    for (int i=0; i < nelts; i++)
		_byteBuf[i] = b[off+(i*pixelStride)];
	    write(_byteBuf, 0, nelts);
	    off += nelts*pixelStride;
	    len -= nelts;
	}
    }

/***********************************************************************
 * Writes <code>length</code> shorts to the stream's current position from
 * the array <code>s</code> starting at index <code>offset</code>.  Only
 * every <code>pixelStride</code>th array element is written out.  In other
 * words, consecutive shorts are written to the stream but they can be
 * scattered in the input array.
 * <p>
 * Pixels are converted from internal Java format as they are written.
 *
 * @throws IOException if the stream does
 * @see ImageOutputStream.writeShorts(short[], int, int)
 */

    public void writeShorts(short[] s, int off, int len, int pixelStride)
				throws IOException
    {
	if (pixelStride == 1 && _direct_int_okay) { // special case, must be IOS
	    _ios.writeShorts(s, off, len);
	    return;
	}

	while (len > 0) {
	    int nelts = Math.min(len, _byteBuf.length/2);
	    fromShorts(_byteBuf, s, off, nelts, pixelStride);
	    write(_byteBuf, 0, nelts*2);
	    off += nelts*pixelStride;
	    len -= nelts;
	}
    }

/***********************************************************************
 * Writes <code>length</code> ushorts to the stream's current position from
 * the array <code>u</code> starting at index <code>offset</code>.  Only
 * every <code>pixelStride</code>th array element is written out.  In other
 * words, consecutive ushorts are written to the stream but they can be
 * scattered in the input array.
 * <p>
 * Pixels are converted from internal Java format as they are written.
 * <p>
 * This is really the same as writeShorts() since the UShorts are stored in
 * a short array anyway.  We just have a separate routine in case it matters
 * sometime.
 *
 * @throws IOException if the stream does
 * @see writeShorts(short[], int, int)
 */

    public void writeUShorts(short[] u, int off, int len, int pixelStride)
				throws IOException
    {
	writeShorts(u, off, len, pixelStride);
    }

/***********************************************************************
 * Writes <code>length</code> ints to the stream's current position from
 * the array <code>i</code> starting at index <code>offset</code>.  Only
 * every <code>pixelStride</code>th array element is written out.  In other
 * words, consecutive ints are written from the stream but they can be
 * scattered in the input array.
 * <p>
 * Pixels are converted from internal Java format as they are written.
 *
 * @throws IOException if the stream does
 * @see ImageOutputStream.writeInts(int[], int, int)
 */

    public void writeInts(int[] i, int off, int len, int pixelStride)
				throws IOException
    {
	if (pixelStride == 1 && _direct_int_okay) { // special case, must be IIS
	    _ios.writeInts(i, off, len);
	    return;
	}

	while (len > 0) {
	    int nelts = Math.min(len, _byteBuf.length/4);
	    fromInts(_byteBuf, i, off, nelts, pixelStride);
	    write(_byteBuf, 0, nelts*4);
	    off += nelts*pixelStride;
	    len -= nelts;
	}
    }

/***********************************************************************
 * Writes <code>length</code> longs from the stream's current position from
 * the array <code>l</code> starting at index <code>offset</code>.  Only
 * every <code>pixelStride</code>th array element is written out.  In other
 * words, consecutive longs are written to the stream but they can be
 * scattered in the input array.
 * <p>
 * Pixels are converted from internal Java format as they are written.
 *
 * @throws IOException if the stream does
 * @see ImageOutputStream.writeLongs(long[], int, int)
 */

    public void writeLongs(long[] l, int off, int len, int pixelStride)
				throws IOException
    {
	if (pixelStride == 1 && _direct_int_okay) { // special case, must be IIS
	    _ios.writeLongs(l, off, len);
	    return;
	}

	while (len > 0) {
	    int nelts = Math.min(len, _byteBuf.length/8);
	    fromLongs(_byteBuf, l, off, nelts, pixelStride);
	    write(_byteBuf, 0, nelts*8);
	    off += nelts*pixelStride;
	    len -= nelts;
	}
    }

/***********************************************************************
 * Writes <code>length</code> floats to the stream's current position from
 * the array <code>f</code> starting at index <code>offset</code>.  Only
 * every <code>pixelStride</code>th array element is written out.  In other
 * words, consecutive floats are written to the stream but they can be
 * scattered in the input array.
 * <p>
 * Pixels are converted from internal Java format as they are written.
 *
 * @throws IOException if the stream does
 * @see writeFloats(short[], int, int)
 * @see ImageOutputStream.writeFloats(float[], int, int)
 */

    public void writeFloats(float[] f, int off, int len, int pixelStride)
				throws IOException
    {
	if (pixelStride == 1 && _direct_float_okay) { //special case,must be IIS
	    _ios.writeFloats(f, off, len);
	    return;
	}

	while (len > 0) {
	    int nelts = Math.min(len, _byteBuf.length/4);
	    fromFloats(_byteBuf, f, off, nelts, pixelStride);
	    write(_byteBuf, 0, nelts*4);
	    off += nelts*pixelStride;
	    len -= nelts;
	}
    }

/***********************************************************************
 * Writes <code>length</code> doubles to the stream's current position from
 * the array <code>d</code> starting at index <code>offset</code>.  Only
 * every <code>pixelStride</code>th array element is written out.  In other
 * words, consecutive doubles are written to the stream but they can be
 * scattered in the input array.
 * <p>
 * Pixels are converted from internal Java format as they are written.
 *
 * @throws IOException if the stream does
 * @see ImageOutputStream.writeDoubles(double[], int, int)
 */

    public void writeDoubles(double[] d, int off, int len, int pixelStride)
				throws IOException
    {
	if (pixelStride == 1 && _direct_float_okay) { //special case,must be IIS
	    _ios.writeDoubles(d, off, len);
	    return;
	}

	while (len > 0) {
	    int nelts = Math.min(len, _byteBuf.length/8);
	    fromDoubles(_byteBuf, d, off, nelts, pixelStride);
	    write(_byteBuf, 0, nelts*8);
	    off += nelts*pixelStride;
	    len -= nelts;
	}
    }

/***********************************************************************
 * Internal routine to write a bunch of bytes from an array.  Needed because
 * everyone's write() routine is different.
 */
    protected void write(byte[] b, int off, int len) throws IOException
    {
	if (_ios != null) {
	    _ios.write(b, off, len);
	    return;
	}
	if (_do != null) {
	    _do.write(b, off, len);
	    return;
	}
	// We have an OutputStream.
	_os.write(b, off, len);
    }

/***********************************************************************
 * Convert an array of shorts to a byte array buffer.
 */
    protected void fromShorts(byte[] b, short[] s, int off, int len,
							int pixelStride)
    {
        int boff = 0;
        if (_int_order == HIGH_ORDER) {
            for (int j = 0; j < len; j++) {
		short v = s[off + j*pixelStride];
                b[boff++] = (byte)(v >>> 8);
                b[boff++] = (byte)(v >>> 0);
            }
        } else {
            for (int j = 0; j < len; j++) {
		short v = s[off + j*pixelStride];
                b[boff++] = (byte)(v >>> 0);
                b[boff++] = (byte)(v >>> 8);
            }
        }
    }

/***********************************************************************
 * Convert an array of ints to a byte array buffer.
 */
    protected void fromInts(byte[] b, int[] i, int off, int len,
							int pixelStride)
    {
        int boff = 0;
        if (_int_order == HIGH_ORDER) {
            for (int j = 0; j < len; j++) {
		int v = i[off + j*pixelStride];
		b[boff++] = (byte)(v >>> 24);
		b[boff++] = (byte)(v >>> 16);
		b[boff++] = (byte)(v >>> 8);
		b[boff++] = (byte)(v >>> 0);
            }
        } else {
            for (int j = 0; j < len; j++) {
		int v = i[off + j*pixelStride];
		b[boff++] = (byte)(v >>> 0);
		b[boff++] = (byte)(v >>> 8);
		b[boff++] = (byte)(v >>> 16);
		b[boff++] = (byte)(v >>> 24);
            }
        }
    }

/***********************************************************************
 * Convert an array of longs to a byte array buffer.
 */
    protected void fromLongs(byte[] b, long[] l, int off, int len,
							int pixelStride)
    {
        int boff = 0;
        if (_int_order == HIGH_ORDER) {
            for (int j = 0; j < len; j++) {
		long v = l[off + j*pixelStride];
		b[boff++] = (byte)(v >>> 56);
		b[boff++] = (byte)(v >>> 48);
		b[boff++] = (byte)(v >>> 40);
		b[boff++] = (byte)(v >>> 32);
		b[boff++] = (byte)(v >>> 24);
		b[boff++] = (byte)(v >>> 16);
		b[boff++] = (byte)(v >>> 8);
		b[boff++] = (byte)(v >>> 0);
            }
        } else {
            for (int j = 0; j < len; j++) {
		long v = l[off + j*pixelStride];
		b[boff++] = (byte)(v >>> 0);
		b[boff++] = (byte)(v >>> 8);
		b[boff++] = (byte)(v >>> 16);
		b[boff++] = (byte)(v >>> 24);
		b[boff++] = (byte)(v >>> 32);
		b[boff++] = (byte)(v >>> 40);
		b[boff++] = (byte)(v >>> 48);
		b[boff++] = (byte)(v >>> 56);
            }
        }
    }

/***********************************************************************
 * Convert an array of floats to a byte array buffer.
 */
    protected void fromFloats(byte[] b, float[] f, int off, int len,
							int pixelStride)
    {
        int boff = 0;
        if (_float_order == HIGH_ORDER) {
            for (int j = 0; j < len; j++) {
		int v = Float.floatToIntBits(f[off + j*pixelStride]);
		b[boff++] = (byte)(v >>> 24);
		b[boff++] = (byte)(v >>> 16);
		b[boff++] = (byte)(v >>> 8);
		b[boff++] = (byte)(v >>> 0);
            }
        } else if (_float_order == LOW_ORDER) {
            for (int j = 0; j < len; j++) {
		int v = Float.floatToIntBits(f[off + j*pixelStride]);
		b[boff++] = (byte)(v >>> 0);
		b[boff++] = (byte)(v >>> 8);
		b[boff++] = (byte)(v >>> 16);
		b[boff++] = (byte)(v >>> 24);
            }

        } else {				// VAX_ORDER
            for (int j = 0; j < len; j++) {

		int v = Float.floatToIntBits(f[off + j*pixelStride]);

		int vax0 = (int) (v >>> 24) & 0xFF;
		int vax1 = (int) (v >>> 16) & 0xFF;
		int vax2 = (int) (v >>> 8) & 0xFF;
		int vax3 = (int) (v >>> 0) & 0xFF;

		int ieee0 = vax0;
		int ieee1 = vax1;
		int ieee2 = vax2;
		int ieee3 = vax3;

		int exp = ((ieee0<<1) & 0xFE) | ((ieee1>>7) & 0x01);

		// Exponent 255 means NaN or Infinity, exponent 254 is too
		// big for VAX notation.  Either way, set to sign * highest
		// possible number

		if (exp == 255 || exp == 254) {
		    vax0 = 0x7F | (ieee0 & 0x80);
		    vax1 = 0xFF;
		    vax2 = 0xFF;
		    vax3 = 0xFF;
		}

		else if (exp != 0) {		// Normal case
		    exp += 2;
		    // remake sign and exponent
		    vax0 = (ieee0 & 0x80) | ((exp>>1) & 0x7F);
		}	// Low bit of exp can't change, so don't bother w/it

		else {			// exp == 0, zero or denormalized num
		    if ( (ieee1 == 0) &&
			 (ieee2 == 0) &&
			 (ieee3 == 0) ) {		// +/- 0
			vax0 = vax1 = vax2 = vax3 = 0;
		    }
		    else {		// denormalized number
			if ((ieee1 & 0x40) != 0) {	// hi bit set (0.1ffff)
			    // Shift left one to renormalize
			    vax1 = ((vax1<<1) & 0xFE) | ((vax2>>7) & 0x01);
			    vax2 = ((vax2<<1) & 0xFE) | ((vax3>>7) & 0x01);
			    vax3 = (vax3<<1) & 0xFE;

			    vax1 = vax1 & 0x7F;		// set vax exp to 2
			    vax0 = (ieee0 & 0x80) | 0x01;  // sign, exponent==2
			}
			else if ((ieee1 & 0x20) != 0) {
			    // next bit set (0.01ffff)
			    // Shift left two to renormalize
			    vax1 = ((vax1<<2) & 0xFC) | ((vax2>>6) & 0x03);
			    vax2 = ((vax2<<2) & 0xFC) | ((vax3>>6) & 0x03);
			    vax3 = (vax3<<2) & 0xFC;

			    vax1 = vax1 | 0x80;		// Set vax exponent to 1
			    vax0 = ieee0 & 0x80;	// sign, exponent==1
			}
			else {			// Number too small for VAX
			    vax0 = vax1 = vax2 = vax3 = 0;	// so set to 0
			}
		    }
		}

		// Write the bytes in rational order:

		b[boff++] = (byte) (vax1 & 0xFF);
		b[boff++] = (byte) (vax0 & 0xFF);
		b[boff++] = (byte) (vax3 & 0xFF);
		b[boff++] = (byte) (vax2 & 0xFF);
		// 24 = 0 ; 16 = 1 ; 8 = 2 ; 0 = 3
	    }
        }
    }

/***********************************************************************
 * Convert an array of doubles to a byte array buffer.
 */
    protected void fromDoubles(byte[] b, double[] d, int off, int len,
							int pixelStride)
    {
        int boff = 0;
        if (_float_order == HIGH_ORDER) {
            for (int j = 0; j < len; j++) {
		long v = Double.doubleToLongBits(d[off + j*pixelStride]);
		b[boff++] = (byte)(v >>> 56);
		b[boff++] = (byte)(v >>> 48);
		b[boff++] = (byte)(v >>> 40);
		b[boff++] = (byte)(v >>> 32);
		b[boff++] = (byte)(v >>> 24);
		b[boff++] = (byte)(v >>> 16);
		b[boff++] = (byte)(v >>> 8);
		b[boff++] = (byte)(v >>> 0);
            }
        } else if (_float_order == LOW_ORDER) {
            for (int j = 0; j < len; j++) {
		long v = Double.doubleToLongBits(d[off + j*pixelStride]);
		b[boff++] = (byte)(v >>> 0);
		b[boff++] = (byte)(v >>> 8);
		b[boff++] = (byte)(v >>> 16);
		b[boff++] = (byte)(v >>> 24);
		b[boff++] = (byte)(v >>> 32);
		b[boff++] = (byte)(v >>> 40);
		b[boff++] = (byte)(v >>> 48);
		b[boff++] = (byte)(v >>> 56);
            }

        } else {				// VAX_ORDER
            for (int j = 0; j < len; j++) {
		long v = Double.doubleToLongBits(d[off + j*pixelStride]);

		int vax0 = (int) (v >>> 56) & 0xFF;
		int vax1 = (int) (v >>> 48) & 0xFF;
		int vax2 = (int) (v >>> 40) & 0xFF;
		int vax3 = (int) (v >>> 32) & 0xFF;
		int vax4 = (int) (v >>> 24) & 0xFF;
		int vax5 = (int) (v >>> 16) & 0xFF;
		int vax6 = (int) (v >>> 8) & 0xFF;
		int vax7 = (int) (v >>> 0) & 0xFF;

		int ieee0 = vax0;
		int ieee1 = vax1;
		int ieee2 = vax2;
		int ieee3 = vax3;
		int ieee4 = vax4;
		int ieee5 = vax5;
		int ieee6 = vax6;

		int expl = (ieee1 >> 4) & 0x0F;
		int exph = ieee0 & 0x7F;
		int exp = ((exph << 4) & 0x07F0) | expl;

		// Exponent 2047 means NaN or Infinity, exponents 1150 to 2046
		// are too large for VAX notation.  In either case, set to
		// sign * highest possible number.

		if (exp >= 1150) {		// Infinity or NaN or too big
		    vax0 = 0x7F | (ieee0 & 0x80);
		    vax1 = vax2 = vax3 = 0xFF;
		    vax4 = vax5 = vax6 = vax7 = 0xFF;
		}

		else if (exp <= 894) {		// Too small or zero
		    vax0 = vax1 = vax2 = vax3 = 0;
		    vax4 = vax5 = vax6 = vax7 = 0;
		}

		else {				// Normal case
		    exp -= 894;			// Change to VAX bias
		    expl = exp;			// Now in range 1..255

		    // Shift mantissa left 3

		    vax1 = ((vax1<<3) & 0xF8) | ((vax2>>5) & 0x07);
		    vax2 = ((vax2<<3) & 0xF8) | ((vax3>>5) & 0x07);
		    vax3 = ((vax3<<3) & 0xF8) | ((vax4>>5) & 0x07);
		    vax4 = ((vax4<<3) & 0xF8) | ((vax5>>5) & 0x07);
		    vax5 = ((vax5<<3) & 0xF8) | ((vax6>>5) & 0x07);
		    vax6 = ((vax6<<3) & 0xF8) | ((vax7>>5) & 0x07);
		    vax7 = (vax7<<3) & 0xF8;

		    vax0 = (ieee0 & 0x80) | ((expl>>1) & 0x7F);
		    vax1 = (vax1 & 0x7F)  | ((expl<<7) & 0x80);
		}

		// Write the bytes in rational order:

		b[boff++] = (byte) (vax1 & 0xFF);
		b[boff++] = (byte) (vax0 & 0xFF);
		b[boff++] = (byte) (vax3 & 0xFF);
		b[boff++] = (byte) (vax2 & 0xFF);
		b[boff++] = (byte) (vax5 & 0xFF);
		b[boff++] = (byte) (vax4 & 0xFF);
		b[boff++] = (byte) (vax7 & 0xFF);
		b[boff++] = (byte) (vax6 & 0xFF);
	    }
        }
    }
}

