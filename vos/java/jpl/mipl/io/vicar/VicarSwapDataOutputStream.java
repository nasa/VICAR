package jpl.mipl.io.vicar;

import java.io.*;

/**
 * VicarSwapDataOutputStream lets an application write primitive
 * data types to an output stream; the data are converted from
 * Java's integer and real representations (HIGH, IEEE respectively)
 * to LOW/RIEEE int/real representations.
 * <p>
 */

public
class VicarSwapDataOutputStream extends VicarDataOutputStream 
implements DataOutput {

    /**
     * Creates a new stream to write data to the specified underlying output
     * stream. 
     * @param out    the output stream you're wrapping
     * @param format the data format to convert *to*
     */
    public VicarSwapDataOutputStream(OutputStream out, int intFmtCode,
				     int realFmtCode) {
        super(out, intFmtCode, realFmtCode);
    }

    /**
     * Writes a <code>short</code> to the underlying output stream as two
     * bytes. 
     *
     * @param      v   a <code>short</code> to be written.
     * @exception  IOException  if an I/O error occurs.
     * @see        java.io.FilterOutputStream#out
     */
    public final void writeShort(int v) throws IOException {
	out.write((v >>> 0) & 0xFF);
	out.write((v >>> 8) & 0xFF);
    }

    /**
     * Writes an <code>int</code> to the underlying output stream as four
     * bytes. 
     *
     * @param      v   an <code>int</code> to be written.
     * @exception  IOException  if an I/O error occurs.
     * @see        java.io.FilterOutputStream#out
     */
    public final void writeInt(int v) throws IOException {
	out.write((v >>>  0) & 0xFF);
	out.write((v >>>  8) & 0xFF);
	out.write((v >>> 16) & 0xFF);
	out.write((v >>> 24) & 0xFF);
    }

    /**
     * Writes a <code>long</code> to the underlying output stream as eight
     * bytes. 
     *
     * @param      v   a <code>long</code> to be written.
     * @exception  IOException  if an I/O error occurs.
     * @see        java.io.FilterOutputStream#out
     */

    public final void writeLong(long v) throws IOException {
	out.write((int)(v >>>  0) & 0xFF);
	out.write((int)(v >>>  8) & 0xFF);
	out.write((int)(v >>> 16) & 0xFF);
	out.write((int)(v >>> 24) & 0xFF);
	out.write((int)(v >>> 32) & 0xFF);
	out.write((int)(v >>> 40) & 0xFF);
	out.write((int)(v >>> 48) & 0xFF);
	out.write((int)(v >>> 56) & 0xFF);
    }

    /**
     * Converts the float argument to an <code>int</code> using the 
     * <code>floatToIntBits</code> method in class <code>Float</code>, 
     * and then writes that <code>int</code> value to the underlying 
     * output stream as a 4-byte quantity. 
     *
     * @param      v   a <code>float</code> value to be written.
     * @exception  IOException  if an I/O error occurs.
     * @see        java.io.FilterOutputStream#out
     * @see        java.lang.Float#floatToIntBits(float)
     */
    public final void writeFloat(float v) throws IOException {
      writeInt(Float.floatToIntBits(v));
    }
    
    /**
     * Converts the double argument to a <code>long</code> using the 
     * <code>doubleToLongBits</code> method in class <code>Double</code>, 
     * and then writes that <code>long</code> value to the underlying 
     * output stream as an 8-byte quantity. 
     *
     * @param      v   a <code>double</code> value to be written.
     * @exception  IOException  if an I/O error occurs.
     * @see        java.io.FilterOutputStream#out
     * @see        java.lang.Double#doubleToLongBits(double)
     */
    public final void writeDouble(double v) throws IOException {
      writeLong(Double.doubleToLongBits(v));
    }
}
