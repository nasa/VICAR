package jpl.mipl.io.vicar;

import java.io.*;

/**
 * VicarDataInputDIWrapper lets an application read primitive
 * data types from a DataInput object; the data are converted 
 * (if necessary) from the int and real formats specified in 
 * the VicarDataFormat object that is passed in via the
 * constructor.
 * <p>
 * This is exactly like VicarDataInputStream except the thing being wrapped
 * is a <code>DataInput</code> instead of a <code>InputStream</code>.
 * @see VicarDataInputStream
 */
public
class VicarDataInputDIWrapper extends InputStream implements DataInput 
{
    DataInput in;

    // codes indicating the integer and real representations used by
    // the source machine
    protected int _intFmtCode;
    protected int _realFmtCode;

    protected int _rawFloat[];
    protected int _vaxFloat[];
    protected int _ieeeFloat[];

    protected long _rawDouble[];
    protected long _vaxDouble[];
    protected long _ieeeDouble[];
    
    /**
     * Creates a new VicarDataInputDIWrapper to read data from the specified 
     * <code>DataInput</code> object. 
     *
     * @param  in     the input stream you`re wrapping
     * @param  format the data format to convert *from*
     */
    public VicarDataInputDIWrapper(DataInput di_in,
				int intFmtCode, int realFmtCode) {
        super();
	in = di_in;
        _intFmtCode = intFmtCode;
        _realFmtCode = realFmtCode;

	_rawFloat = new int[4];
	_vaxFloat = new int[4];
	_ieeeFloat = new int[4];
	
	_rawDouble = new long[8];
	_vaxDouble = new long[8];
	_ieeeDouble = new long[8];
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

    public short readShort() throws IOException {
     
      // note that we`re reading *bytes* from the underlying stream, but we`re 
      // *storing* them as ints

      int ch1 = in.readUnsignedByte();
      int ch2 = in.readUnsignedByte();

      if (_intFmtCode == VicarDataFormat.INT_FMT_LOW)
        return (short)((ch2 << 8) + (ch1 << 0));
      else
        return (short)((ch1 << 8) + (ch2 << 0));
    }

    /**
     * Reads an unsigned 16-bit number from this data input stream. This 
     * method reads two bytes from the underlying input stream.
     * <p>
     * This method blocks until the two bytes are read, the end of the 
     * stream is detected, or an exception is thrown. 
     *
     * @return     the next two bytes of this input stream, interpreted as an
     *             unsigned 16-bit integer.
     * @exception  EOFException  if this input stream reaches the end before
     *               reading two bytes.
     * @exception  IOException   if an I/O error occurs.
     * @see        java.io.FilterInputStream#in
     */

    public int readUnsignedShort() throws IOException {
        int ch1 = in.readUnsignedByte();
        int ch2 = in.readUnsignedByte();

        if (_intFmtCode == VicarDataFormat.INT_FMT_LOW)
            return ((ch2 << 8) + (ch1 << 0));
        else
            return ((ch1 << 8) + (ch2 << 0));
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

    public int readInt() throws IOException {

        int ch1 = in.readUnsignedByte();
        int ch2 = in.readUnsignedByte();
        int ch3 = in.readUnsignedByte();
        int ch4 = in.readUnsignedByte();
        
        if (_intFmtCode == VicarDataFormat.INT_FMT_LOW)
          return ((ch4 << 24) + (ch3 << 16) + (ch2 << 8) + (ch1 << 0));
        else
          return ((ch1 << 24) + (ch2 << 16) + (ch3 << 8) + (ch4 << 0));
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
  
    public long readLong() throws IOException {

	long int1 = (long) readInt();
	long int2 = (long) readInt();

	if (_intFmtCode == VicarDataFormat.INT_FMT_LOW)
	  return (int2 << 32) + (int1 << 0);
	else
	  return (int1 << 32) + (int2 << 0);
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

    public float readFloat() throws IOException {

      _rawFloat[0] = in.readUnsignedByte();
      _rawFloat[1] = in.readUnsignedByte();
      _rawFloat[2] = in.readUnsignedByte();
      _rawFloat[3] = in.readUnsignedByte();
      
      if (_realFmtCode == VicarDataFormat.REAL_FMT_VAX) {
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
      else if (_realFmtCode == VicarDataFormat.REAL_FMT_RIEEE)
	return Float.intBitsToFloat((_rawFloat[3] << 24) +
				    (_rawFloat[2] << 16) +
				    (_rawFloat[1] <<  8) +
				    (_rawFloat[0] <<  0));
      else // it's _IEEEFLOAT
	return Float.intBitsToFloat((_rawFloat[0] << 24) +
				    (_rawFloat[1] << 16) +
				    (_rawFloat[2] <<  8) +
				    (_rawFloat[3] <<  0));
    }
  
      
    protected int[] shift_right(int[] x)
    {
        x[3] = ((x[3]>>1) & 0x7F) | ((x[2]<<7) & 0x80);
        x[2] = ((x[2]>>1) & 0x7F) | ((x[1]<<7) & 0x80);
        x[1] = (x[1]>>1) & 0x7F;
        return x;
    }

    protected long[] shift_right_3(long[] y, long[] x)
    {
      y[7] = ((x[7]>>3) & 0x1F) | ((x[6]<<5) & 0xE0);
      y[6] = ((x[6]>>3) & 0x1F) | ((x[5]<<5) & 0xE0);
      y[5] = ((x[5]>>3) & 0x1F) | ((x[4]<<5) & 0xE0);
      y[4] = ((x[4]>>3) & 0x1F) | ((x[3]<<5) & 0xE0);
      y[3] = ((x[3]>>3) & 0x1F) | ((x[2]<<5) & 0xE0);
      y[2] = ((x[2]>>3) & 0x1F) | ((x[1]<<5) & 0xE0);
      y[1] = ((x[1]>>3) & 0x1F);
      return y;
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
  
    public double readDouble() throws IOException {

      _rawDouble[0] = in.readUnsignedByte();
      _rawDouble[1] = in.readUnsignedByte();
      _rawDouble[2] = in.readUnsignedByte();
      _rawDouble[3] = in.readUnsignedByte();
      _rawDouble[4] = in.readUnsignedByte();
      _rawDouble[5] = in.readUnsignedByte();
      _rawDouble[6] = in.readUnsignedByte();
      _rawDouble[7] = in.readUnsignedByte();

      if (_realFmtCode == VicarDataFormat.REAL_FMT_VAX) {
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
	    _ieeeDouble[0] = _ieeeDouble[1] = _ieeeDouble[2] = _ieeeDouble[3] 
	      = 0;
	    _ieeeDouble[4] = _ieeeDouble[5] = _ieeeDouble[6] = _ieeeDouble[7] 
	      = 0;
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
      else if (_realFmtCode == VicarDataFormat.REAL_FMT_RIEEE)
        return Double.longBitsToDouble((_rawDouble[7] << 56) +
				       (_rawDouble[6] << 48) +
				       (_rawDouble[5] << 40) +
				       (_rawDouble[4] << 32) +
				       (_rawDouble[3] << 24) +
				       (_rawDouble[2] << 16) +
				       (_rawDouble[1] <<  8) +
				       (_rawDouble[0] <<  0));
      else // it's IEEE
	return Double.longBitsToDouble((_rawDouble[0] << 56)+
				       (_rawDouble[1] << 48) +
				       (_rawDouble[2] << 40) +
				       (_rawDouble[3] << 32) +
				       (_rawDouble[4] << 24) +
				       (_rawDouble[5] << 16) +
				       (_rawDouble[6] <<  8) +
				       (_rawDouble[7] <<  0));


    }

    /**
     * Reads <code>b.length</code> bytes from this data input stream 
     * into the byte array. This method reads repeatedly from the 
     * underlying stream until all the bytes are read. This method blocks 
     * until all the bytes are read, the end of the stream is detected, 
     * or an exception is thrown. 
     *
     * @param      b   the buffer into which the data is read.
     * @exception  EOFException  if this input stream reaches the end before
     *               reading all the bytes.
     * @exception  IOException   if an I/O error occurs.
     * @see        java.io.FilterInputStream#in
     */
    public final void readFully(byte b[]) throws IOException {
	in.readFully(b);
    }

    /**
     * Reads exactly <code>len</code> bytes from this data input stream 
     * into the byte array. This method reads repeatedly from the 
     * underlying stream until all the bytes are read. This method blocks 
     * until all the bytes are read, the end of the stream is detected, 
     * or an exception is thrown. 
     *
     * @param      b     the buffer into which the data is read.
     * @param      off   the start offset of the data.
     * @param      len   the number of bytes to read.
     * @exception  EOFException  if this input stream reaches the end before
     *               reading all the bytes.
     * @exception  IOException   if an I/O error occurs.
     * @see        java.io.FilterInputStream#in
     */
    public final void readFully(byte b[], int off, int len)throws IOException {
	in.readFully(b, off, len);
    }

    /**
     * Skips exactly <code>n</code> bytes of input in the underlying 
     * input stream. This method blocks until all the bytes are skipped, 
     * the end of the stream is detected, or an exception is thrown. 
     *
     * @param      n   the number of bytes to be skipped.
     * @return     the number of bytes skipped, which is always <code>n</code>.
     * @exception  EOFException  if this input stream reaches the end before
     *               skipping all the bytes.
     * @exception  IOException   if an I/O error occurs.
     */
    public final int skipBytes(int n) throws IOException {
	for (int i = 0 ; i < n ; i ++)
	    in.readUnsignedByte();
	return n;
    }

    /***********************************************************************
     * Reads the next byte of data from this input stream.
     * @see InputStream#read()
     * @throws IOException if the <code>DataInput</code> does
     */
    public int read() throws IOException
    {
        try {
            return in.readUnsignedByte();
        } catch (EOFException e) {
            return -1;
        }
    }


    /**
     * Reads a <code>boolean</code> from this data input stream. This 
     * method reads a single byte from the underlying input stream. A 
     * value of <code>0</code> represents <code>false</code>. Any other 
     * value represents <code>true</code>. This method blocks until 
     * either the byte is read, the end of the stream is detected, or an 
     * exception is thrown. 
     *
     * @return     the <code>boolean</code> value read.
     * @exception  EOFException  if this input stream has reached the end.
     * @exception  IOException   if an I/O error occurs.
     * @see        java.io.FilterInputStream#in
     */
    public final boolean readBoolean() throws IOException {
	return in.readBoolean();
    }

    /**
     * Reads a signed 8-bit value from this data input stream. This 
     * method reads a byte from the underlying input stream. If the byte 
     * read is <code>b</code>, where 
     * 0&nbsp;&lt;=&nbsp;<code>b</code>&nbsp;&lt;=&nbsp;255, then the 
     * result is:
     * <ul><code>
     *     (byte)(b)
     * </code></ul>
     * <p>
     * This method blocks until either the byte is read, the end of the 
     * stream is detected, or an exception is thrown. 
     *
     * @return     the next byte of this input stream as a signed 8-bit
     *             <code>byte</code>.
     * @exception  EOFException  if this input stream has reached the end.
     * @exception  IOException   if an I/O error occurs.
     * @see        java.io.FilterInputStream#in
     */
    public final byte readByte() throws IOException {
	return in.readByte();
    }

    /**
     * Reads an unsigned 8-bit number from this data input stream. This 
     * method reads a byte from this data input stream's underlying input 
     * stream and returns that byte. This method blocks until the byte is 
     * read, the end of the stream is detected, or an exception is thrown.
     *
     * @return     the next byte of this input stream, interpreted as an
     *             unsigned 8-bit number.
     * @exception  EOFException  if this input stream has reached the end.
     * @exception  IOException   if an I/O error occurs.
     * @see         java.io.FilterInputStream#in
     */
    public final int readUnsignedByte() throws IOException {
	return in.readUnsignedByte();
    }

    /**
     * Reads a Unicode character from this data input stream. This 
     * method reads two bytes from the underlying input stream. If the 
     * bytes read, in order, are <code>b1</code> and <code>b2</code>, 
     * where 0&nbsp;&lt;=&nbsp;<code>b1</code>, 
     * <code>b1</code>&nbsp;&lt;=&nbsp;255, then the result is equal to:
     * <ul><code>
     *     (char)((b1 &lt;&lt; 8) | b2)
     * </code></ul>
     * <p>
     * This method blocks until either the two bytes are read, the end of 
     * the stream is detected, or an exception is thrown. 
     *
     * @return     the next two bytes of this input stream as a Unicode
     *             character.
     * @exception  EOFException  if this input stream reaches the end before
     *               reading two bytes.
     * @exception  IOException   if an I/O error occurs.
     * @see        java.io.FilterInputStream#in
     */
    public final char readChar() throws IOException {
	return in.readChar();
    }

    private char lineBuffer[];

    /**
     * Reads the next line of text from this data input stream. This 
     * method successively reads bytes from the underlying input stream 
     * until it reaches the end of a line of text. 
     * <p>
     * A line of text is terminated by a carriage return character 
     * (<code>'&#92;r'</code>), a newline character (<code>'&#92;n'</code>), a 
     * carriage return character immediately followed by a newline 
     * character, or the end of the input stream. The line-terminating 
     * character(s), if any, are not returned as part of the string that 
     * is returned. 
     * <p>
     * This method blocks until a newline character is read, a carriage 
     * return and the byte following it are read (to see if it is a 
     * newline), the end of the stream is detected, or an exception is 
     * thrown.
     *
     * @deprecated This method does not properly convert bytes to characters.
     * As of JDK&nbsp;1.1, the preferred way to read lines of text is via the
     * <code>BufferedReader.readLine()</code> method.  Programs that use the
     * <code>DataInputStream</code> class to read lines can be converted to use
     * the <code>BufferedReader</code> class by replacing code of the form
     * <ul>
     *     <code>DataInputStream d =&nbsp;new&nbsp;DataInputStream(in);</code>
     * </ul>
     * with
     * <ul>
     *     <code>BufferedReader d
     *          =&nbsp;new&nbsp;BufferedReader(new&nbsp;InputStreamReader(in));
     *      </code>
     * </ul>
     *
     * @return     the next line of text from this input stream, or 
     *             <tt>null</tt> if no bytes are read before end-of-file 
     *             is reached.
     * @exception  IOException  if an I/O error occurs.
     * @see        java.io.BufferedReader#readLine()
     * @see        java.io.FilterInputStream#in
     */
    public final String readLine() throws IOException {
	return in.readLine();
    }

    /**
     * Reads in a string that has been encoded using a modified UTF-8 
     * format from this data input stream. This method calls 
     * <code>readUTF(this)</code>.
     * See <code>readUTF(java.io.DataInput)</code> for a more 
     * complete description of the format. 
     * <p>
     * This method blocks until all the bytes are read, the end of the 
     * stream is detected, or an exception is thrown. 
     *
     * @return     a Unicode string.
     * @exception  EOFException  if this input stream reaches the end before
     *               reading all the bytes.
     * @exception  IOException   if an I/O error occurs.
     * @see        java.io.DataInputStream#readUTF(java.io.DataInput)
     */
    public final String readUTF() throws IOException {
        return in.readUTF();
    }

}

