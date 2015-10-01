package jpl.mipl.io.vicar;

import java.io.*;

/**
 * VicarVaxDataInputDIWrapper lets an application read primitive
 * data types from <code>DataInput</code>; the data are converted 
 * (if necessary) from the int and real formats specified in 
 * the VicarDataFormat object that is passed in via the
 * constructor.
 * <p>
 * This is exactly like VicarVaxDataInputStream except the thing being wrapped
 * is a <code>DataInput</code> instead of a <code>InputStream</code>.
 * @see VicarVaxDataInputStream
 */
public
class VicarVaxDataInputDIWrapper extends VicarDataInputDIWrapper
{
    /**
     * Creates a new VicarVaxDataInputDIWrapper to read data from the specified 
     * <code>DataInput</code>. 
     *
     * @param  in     the input stream you`re wrapping
     * @param  format the data format to convert *from*
     */
    public VicarVaxDataInputDIWrapper(DataInput in,
				   int intFmtCode, int realFmtCode) {
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

      int ch1 = in.readUnsignedByte();
      int ch2 = in.readUnsignedByte();

      return (short)((ch2 << 8) + (ch1 << 0));
    }

    /**
     * Reads an unsigned 16-bit number from this data input stream. The 
     * method reads two bytes from the underlying input stream.
     * <p>
     * This method blocks until the two bytes are read, the end of the 
     * stream is detected, or an exception is thrown. 
     *
     * @return     the next two bytes of this input stream, interpreted as an
     *             unsigned 16-bit number.
     * @exception  EOFException  if this input stream reaches the end before
     *               reading two bytes.
     * @exception  IOException   if an I/O error occurs.
     * @see        java.io.FilterInputStream#in
     */

    public final int readUnsignedShort() throws IOException {
     
      // note that we`re reading *bytes* from the underlying stream, but we`re 
      // *storing* them as ints

      int ch1 = in.readUnsignedByte();
      int ch2 = in.readUnsignedByte();

      return ((ch2 << 8) + (ch1 << 0));
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

        int ch1 = in.readUnsignedByte();
        int ch2 = in.readUnsignedByte();
        int ch3 = in.readUnsignedByte();
        int ch4 = in.readUnsignedByte();
        
	return ((ch4 << 24) + (ch3 << 16) + (ch2 << 8) + (ch1 << 0));
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

	return (int2 << 32) + (int1 << 0);
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

      _rawFloat[0] = in.readUnsignedByte();
      _rawFloat[1] = in.readUnsignedByte();
      _rawFloat[2] = in.readUnsignedByte();
      _rawFloat[3] = in.readUnsignedByte();
      
      int exp;
      
      // first, read the bytes in rational order:
      
      _vaxFloat[1] = _ieeeFloat[1] = _rawFloat[0];
      _vaxFloat[0] = _ieeeFloat[0] = _rawFloat[1];
      _vaxFloat[3] = _ieeeFloat[3] = _rawFloat[2];
      _vaxFloat[2] = _ieeeFloat[2] = _rawFloat[3];
      
      exp = ((_vaxFloat[0]<<1)& 0xFE) | ((_vaxFloat[1]>>7)& 0x01);
      
      if (exp == 0) {              // Zero or invalid pattern
	if ((_vaxFloat[0]&0x80) != 0) {   // Sign bit set (illegal for VAX)
	  _ieeeFloat[0] = 0x7F;                // _IEEEFLOAT NaN
	  _ieeeFloat[1] = 0xFF;
	  _ieeeFloat[2] = 0xFF;
	  _ieeeFloat[3] = 0xFF;
	}
	else {                    // Zero
	  _ieeeFloat[0] = _ieeeFloat[1] = _ieeeFloat[2] = _ieeeFloat[3] = 0;
	}
      }
      
      else if (exp >= 3) {         // Normal case
	exp -= 2;
	_ieeeFloat[0] = (_vaxFloat[0]&0x80) | ((exp>>1)&0x7F);   
	// remake sign + exponent
      }    // Low bit of exp can't change, so don't bother w/it
      
      else if (exp == 2) {         // Denormalize the number
	_ieeeFloat = shift_right(_ieeeFloat);
	// Add suppressed most signif bit,
	_ieeeFloat[1] = (_ieeeFloat[1] & 0x3F) | 0x40;  
	// and set exponent to 0 (preserving sign)
	_ieeeFloat[0] = _vaxFloat[0] & 0x80; 
      }
      
      else {                       // Exp==1, denormalize again
	shift_right(_ieeeFloat);       // Like above but shift by 2
	shift_right(_ieeeFloat);
	_ieeeFloat[1] = (_ieeeFloat[1] & 0x1F) | 0x20;
	_ieeeFloat[0] = _vaxFloat[0] & 0x80;
      }
      
      // package it up and ship it out
      return Float.intBitsToFloat((_ieeeFloat[0] << 24) +
				  (_ieeeFloat[1] << 16) +
				  (_ieeeFloat[2] << 8) +
				  (_ieeeFloat[3] << 0));
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

      _rawDouble[0] = in.readUnsignedByte();
      _rawDouble[1] = in.readUnsignedByte();
      _rawDouble[2] = in.readUnsignedByte();
      _rawDouble[3] = in.readUnsignedByte();
      _rawDouble[4] = in.readUnsignedByte();
      _rawDouble[5] = in.readUnsignedByte();
      _rawDouble[6] = in.readUnsignedByte();
      _rawDouble[7] = in.readUnsignedByte();

      long exp, exph, expl;
      
      // first, read the bytes in rational order
      
      _vaxDouble[1] = _ieeeDouble[1] = _rawDouble[0];
      _vaxDouble[0] = _ieeeDouble[0] = _rawDouble[1];
      _vaxDouble[3] = _ieeeDouble[3] = _rawDouble[2];
      _vaxDouble[2] = _ieeeDouble[2] = _rawDouble[3];
      _vaxDouble[5] = _ieeeDouble[5] = _rawDouble[4];
      _vaxDouble[4] = _ieeeDouble[4] = _rawDouble[5];
      _vaxDouble[7] = _ieeeDouble[7] = _rawDouble[6];
      _vaxDouble[6] = _ieeeDouble[6] = _rawDouble[7];
      
      exp = ((_vaxDouble[0]<<1)&0xFE) | ((_vaxDouble[1]>>7)&0x01);
      
      if (exp == 0) {		// Zero or invalid pattern
	if ((_vaxDouble[0]&0x80) != 0) {  // Sign bit set -- illegal for VAX
	  _ieeeDouble[0] = 0x7F;	// IEEE NaN
	  _ieeeDouble[1] = 0xFF;
	  _ieeeDouble[2] = 0xFF;
	  _ieeeDouble[3] = 0xFF;
	  _ieeeDouble[4] = _ieeeDouble[5] = _ieeeDouble[6] =
	    _ieeeDouble[7] = 0xFF;
	}
	else {			// Zero
	  _ieeeDouble[0] = _ieeeDouble[1] = _ieeeDouble[2] = _ieeeDouble[3]= 0;
	  _ieeeDouble[4] = _ieeeDouble[5] = _ieeeDouble[6] = _ieeeDouble[7]= 0;
	}
      }
      
      else {				// Normal case
	exp += 894;			// Change to IEEE bias
	exph = (exp>>4) & 0x7F;	// convert to unsigned chars
	expl = (exp<<4) & 0xF0;
	
	// Shift mantissa (rounds to 0)
	_ieeeDouble = shift_right_3(_ieeeDouble, _vaxDouble);
	// put back exponent and sign
	_ieeeDouble[0] = (_vaxDouble[0]&0x80) | exph;
	_ieeeDouble[1] = (_ieeeDouble[1] & 0x0F) | expl;
      }
      
      return  Double.longBitsToDouble((_ieeeDouble[0] << 56) +
				      (_ieeeDouble[1] << 48) +
				      (_ieeeDouble[2] << 40) +
				      (_ieeeDouble[3] << 32) +
				      (_ieeeDouble[4] << 24) +
				      (_ieeeDouble[5] << 16) +
				      (_ieeeDouble[6] <<  8) +
				      (_ieeeDouble[7] <<  0));
    }
}
