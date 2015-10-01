package jpl.mipl.io.vicar;

import java.io.*;

/**
 * VicarSwapDataInputStream lets an application read primitive
 * data types from an input stream; the data are converted to
 * Java's integer and real representations (HIGH, IEEE respectively)
 * from LOW/RIEEE int/real representations.
 * <p>
 */
public
class VicarSwapDataInputStream extends VicarDataInputStream
implements DataInput 
{
    /**
     * Creates a new VicarSwapDataInputStream to read data from the specified 
     * input stream. 
     *
     * @param  in     the input stream you`re wrapping
     * @param  format the data format to convert *from*
     */
    public VicarSwapDataInputStream(InputStream in, int intFmtCode,
				    int realFmtCode) {
      super(in, intFmtCode, realFmtCode);
    }

    /**
     * Reads a signed 16-bit number from this data input stream. The 
     * method reads two bytes from the underlying input stream.
     * <p>
     * This method blocks until the two bytes are read, the end of the 
     * stream is detected, or an exception is thrown. 
     *
     * @return     the next two bytes of this input stream, interpreted as a
     *             signed 16-bit number.
     * @exception  EOFException  if this input stream reaches the end before
     *               reading two bytes.
     * @exception  IOException   if an I/O error occurs.
     * @see        java.io.FilterInputStream#in
     */

    public final short readShort() throws IOException {
     
      // note that we`re reading *bytes* from the underlying stream, but we`re 
      // *storing* them as ints

      int ch1 = in.read();
      int ch2 = in.read();
      if ((ch1 | ch2) < 0)
        throw new EOFException();

      return (short)((ch2 << 8) + ch1);
    }

    /**
     * Reads an unsigned 16-bit number from this data input stream. The 
     * method reads two bytes from the underlying input stream.
     * <p>
     * This method blocks until the two bytes are read, the end of the 
     * stream is detected, or an exception is thrown. 
     *
     * @return     the next two bytes of this input stream, interpreted as a
     *             signed 16-bit number.
     * @exception  EOFException  if this input stream reaches the end before
     *               reading two bytes.
     * @exception  IOException   if an I/O error occurs.
     * @see        java.io.FilterInputStream#in
     */

    public final int readUnsignedShort() throws IOException {
     
      // note that we`re reading *bytes* from the underlying stream, but we`re 
      // *storing* them as ints

      int ch1 = in.read();
      int ch2 = in.read();
      if ((ch1 | ch2) < 0)
        throw new EOFException();

      return ((ch2 << 8) + ch1);
    }

    /**
     * Reads a signed 32-bit integer from this input stream. This 
     * method reads four bytes from the underlying input stream.
     * <p>
     * This method blocks until the four bytes are read, the end of the 
     * stream is detected, or an exception is thrown. 
     *
     * @return     the next four bytes of this input stream, interpreted as an
     *             <code>int</code>.
     * @exception  EOFException  if this input stream reaches the end before
     *               reading four bytes.
     * @exception  IOException   if an I/O error occurs.
     * @see        java.io.FilterInputStream#in
     */

    public final int readInt() throws IOException {

        int ch1 = in.read();
        int ch2 = in.read();
        int ch3 = in.read();
        int ch4 = in.read();
        if ((ch1 | ch2 | ch3 | ch4) < 0)
             throw new EOFException();
	return ((ch4 << 24) + (ch3 << 16) + (ch2 << 8) + ch1);
    }

    /**
     * Reads a signed 64-bit integer from this data input stream. This 
     * method reads eight bytes from the underlying input stream. 
     * <p>
     * This method blocks until the eight bytes are read, the end of the 
     * stream is detected, or an exception is thrown. 
     *
     * @return     the next eight bytes of this input stream, interpreted as a
     *             <code>long</code>.
     * @exception  EOFException  if this input stream reaches the end before
     *               reading eight bytes.
     * @exception  IOException   if an I/O error occurs.
     * @see        java.io.FilterInputStream#in
     */
  
    public final long readLong() throws IOException {

	long int1 = (long) readInt();
	long int2 = (long) readInt();
	
	if ((int1 | int2) < 0)
	  throw new EOFException();
	
	return (int2 << 32) + int1;
    }

    /**
     * Reads a <code>float</code> from this data input stream. This 
     * method reads an <code>int</code> value as if by the 
     * <code>readInt</code> method and then converts that 
     * <code>int</code> to a <code>float</code> using the 
     * <code>intBitsToFloat</code> method in class <code>Float</code>. 
     * This method blocks until the four bytes are read, the end of the 
     * stream is detected, or an exception is thrown. 
     *
     * @return     the next four bytes of this input stream, interpreted as a
     *             <code>float</code>.
     * @exception  EOFException  if this input stream reaches the end before
     *               reading four bytes.
     * @exception  IOException   if an I/O error occurs.
     * @see        java.io.DataInputStream#readInt()
     * @see        java.lang.Float#intBitsToFloat(int)
     */

    public final float readFloat() throws IOException {
      return Float.intBitsToFloat(readInt());
    }

    /**
     * Reads a <code>double</code> from this data input stream. This 
     * method reads a <code>long</code> value as if by the 
     * <code>readLong</code> method and then converts that 
     * <code>long</code> to a <code>double</code> using the 
     * <code>longBitsToDouble</code> method in class <code>Double</code>.
     * <p>
     * This method blocks until the eight bytes are read, the end of the 
     * stream is detected, or an exception is thrown. 
     *
     * @return     the next eight bytes of this input stream, interpreted as a
     *             <code>double</code>.
     * @exception  EOFException  if this input stream reaches the end before
     *               reading eight bytes.
     * @exception  IOException   if an I/O error occurs.
     * @see        java.io.DataInputStream#readLong()
     * @see        java.lang.Double#longBitsToDouble(long)
     */
  
    public final double readDouble() throws IOException {
      return Double.longBitsToDouble(readLong());
    }
}
