package jpl.mipl.io.vicar;

import java.io.*;

/**
 * VicarDataOutputDOWrapper lets an application write primitive
 * data types to a <code>DataOutput</code> object; the data are converted 
 * (if necessary) to the int and real formats specified in 
 * the VicarDataFormat object that is passed in via the
 * constructor.
 * <p>
 * This is exactly like VicarDataOutputStream except the thing being wrapped
 * is a <code>DataOutput</code> instead of a <code>OutputStream</code>.
 * @see VicarDataOutputStream
 */

public
class VicarDataOutputDOWrapper extends OutputStream implements DataOutput {

    DataOutput out;

    // codes indicating the integer and real representations used by
    // the destination machine
    protected int _intFmtCode;
    protected int _realFmtCode;

    protected int _vaxFloat[];
    protected int _ieeeFloat[];

    protected int  _vaxDouble[];
    protected int _ieeeDouble[];
    

    /**
     * Creates a new stream to write data to the specified underlying output
     * stream. 
     * @param output the data output you're wrapping
     * @param format the data format to convert *to*
     */
    public VicarDataOutputDOWrapper(DataOutput output, int intFmtCode,
				 int realFmtCode) {
        out = output;
        _intFmtCode = intFmtCode;
        _realFmtCode = realFmtCode;

	_vaxFloat = new int[4];
	_ieeeFloat = new int[4];
	
	_vaxDouble = new int[8];
	_ieeeDouble = new int[8];
    }

    /**
     * Writes a <code>short</code> to the underlying output stream as two
     * bytes. 
     *
     * @param      v   a <code>short</code> to be written.
     * @exception  IOException  if an I/O error occurs.
     * @see        java.io.FilterOutputStream#out
     */
    public void writeShort(int v) throws IOException {
        if (_intFmtCode == VicarDataFormat.INT_FMT_HIGH) {
            out.write((v >>> 8) & 0xFF);
            out.write((v >>> 0) & 0xFF);
        }
        else {
            out.write((v >>> 0) & 0xFF);
            out.write((v >>> 8) & 0xFF);
        }
    }

    /**
     * Writes an <code>int</code> to the underlying output stream as four
     * bytes. 
     *
     * @param      v   an <code>int</code> to be written.
     * @exception  IOException  if an I/O error occurs.
     * @see        java.io.FilterOutputStream#out
     */
    public void writeInt(int v) throws IOException {
        
        if (_intFmtCode == VicarDataFormat.INT_FMT_HIGH) {
            out.write((v >>> 24) & 0xFF);
            out.write((v >>> 16) & 0xFF);
            out.write((v >>>  8) & 0xFF);
            out.write((v >>>  0) & 0xFF);
        }
        else {
            out.write((v >>>  0) & 0xFF);
            out.write((v >>>  8) & 0xFF);
            out.write((v >>> 16) & 0xFF);
            out.write((v >>> 24) & 0xFF);
        }
    
    }

    /**
     * Writes a <code>long</code> to the underlying output stream as eight
     * bytes. 
     *
     * @param      v   a <code>long</code> to be written.
     * @exception  IOException  if an I/O error occurs.
     * @see        java.io.FilterOutputStream#out
     */

    public void writeLong(long v) throws IOException {
	
	if (_intFmtCode == VicarDataFormat.INT_FMT_HIGH) {
	  out.write((int)(v >>> 56) & 0xFF);
	  out.write((int)(v >>> 48) & 0xFF);
	  out.write((int)(v >>> 40) & 0xFF);
	  out.write((int)(v >>> 32) & 0xFF);
	  out.write((int)(v >>> 24) & 0xFF);
	  out.write((int)(v >>> 16) & 0xFF);
	  out.write((int)(v >>>  8) & 0xFF);
	  out.write((int)(v >>>  0) & 0xFF);
	}
	else {
	  out.write((int)(v >>>  0) & 0xFF);
	  out.write((int)(v >>>  8) & 0xFF);
	  out.write((int)(v >>> 16) & 0xFF);
	  out.write((int)(v >>> 24) & 0xFF);
	  out.write((int)(v >>> 32) & 0xFF);
	  out.write((int)(v >>> 40) & 0xFF);
   	  out.write((int)(v >>> 48) & 0xFF);
	  out.write((int)(v >>> 56) & 0xFF);
	}

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
    public void writeFloat(float v) throws IOException {
      int rawIntBits = Float.floatToIntBits(v);

      if ( _realFmtCode == VicarDataFormat.REAL_FMT_VAX) {
	int exp;
	
	_vaxFloat[0] = _ieeeFloat[0] = (int) (rawIntBits >>> 24) & 0xFF;
	_vaxFloat[1] = _ieeeFloat[1] = (int) (rawIntBits >>> 16) & 0xFF;
	_vaxFloat[2] = _ieeeFloat[2] = (int) (rawIntBits >>>  8) & 0xFF;
	_vaxFloat[3] = _ieeeFloat[3] = (int) (rawIntBits >>>  0) & 0xFF;
	
	exp = ((_ieeeFloat[0]<<1)&0xFE) | ((_ieeeFloat[1]>>7)&0x01);
	
	// Exponent 255 means NaN or Infinity, exponent 254 is too big for
	// VAX notation.  Either way, set to sign * highest possible number
        
	if (exp == 255 || exp == 254) {
	  _vaxFloat[0] = 0x7F | (_ieeeFloat[0]&0x80);
	  _vaxFloat[1] = 0xFF;
	  _vaxFloat[2] = 0xFF;
	  _vaxFloat[3] = 0xFF;
	}
	
	else if (exp != 0) {                // Normal case
	  exp += 2;
	  // remake sign and exponent
	  _vaxFloat[0] = (_ieeeFloat[0]&0x80) | ((exp>>1)&0x7F);
	}   // Low bit of exp can't change, so don't bother w/it 
	
	else {                      // exp == 0, zero or denormalized num
	  if ( (_ieeeFloat[1] == 0) &&
	       (_ieeeFloat[2] == 0) &&
	       (_ieeeFloat[3] == 0) ) {     // +/- 0
	    _vaxFloat[0] = _vaxFloat[1] = _vaxFloat[2] = _vaxFloat[3] = 0;
	  }
	  else {                  // denormalized number
	    if ((_ieeeFloat[1] & 0x40) != 0) {        // hi bit set (0.1ffff)
	      _vaxFloat = shift_left(_vaxFloat);         // Renormalize
	      _vaxFloat[1] = _vaxFloat[1] & 0x7F;        // Set vax exp to 2
	      _vaxFloat[0] = (_ieeeFloat[0]&0x80) | 0x01; // sign, exponent==2
	    }
	    else if ((_ieeeFloat[1] & 0x20) != 0) { 
	      // next bit set (0.01ffff)
	      _vaxFloat = shift_left(_vaxFloat);       // Renormalize
	      _vaxFloat = shift_left(_vaxFloat);
	      _vaxFloat[1] = _vaxFloat[1] | 0x80;    // Set vax exponent to 1
	      _vaxFloat[0] = _ieeeFloat[0]&0x80;      // sign, exponent==1
	    }
	    else {                      // Number too small for VAX
	      _vaxFloat[0] = _vaxFloat[1] = _vaxFloat[2] = _vaxFloat[3] = 0; 
	      // so set to 0
	    }
	  }
	}
            
	// write the bytes in rational order:
	
	out.write(_vaxFloat[1] & 0xFF);  
	out.write(_vaxFloat[0] & 0xFF);  
	out.write(_vaxFloat[3] & 0xFF);  
	out.write(_vaxFloat[2] & 0xFF);  
	// 24 = 0 ; 16 = 1 ; 8 = 2 ; 0 = 3
      }
      else if (_realFmtCode == VicarDataFormat.REAL_FMT_RIEEE) {
	out.write((rawIntBits >>>  0) & 0xFF);
	out.write((rawIntBits >>>  8) & 0xFF);
	out.write((rawIntBits >>> 16) & 0xFF);
	out.write((rawIntBits >>> 24) & 0xFF);
      }
      else { // it's IEEE
	out.write((rawIntBits >>> 24) & 0xFF);
	out.write((rawIntBits >>> 16) & 0xFF);
	out.write((rawIntBits >>>  8) & 0xFF);
	out.write((rawIntBits >>>  0) & 0xFF);
      }

    }
    
    protected int[] shift_left(int[] x)
    {
        x[1] = ((x[1]<<1) & 0xFE) | ((x[2]>>7) & 0x01);
        x[2] = ((x[2]<<1) & 0xFE) | ((x[3]>>7) & 0x01);
        x[3] = (x[3]<<1) & 0xFE;
        return x;
    }
    
    protected int[] shift_left_3(int[] y, int[] x)
    {
        y[1] = ((x[1]<<3) & 0xF8) | ((x[2]>>5) & 0x07);
        y[2] = ((x[2]<<3) & 0xF8) | ((x[3]>>5) & 0x07);
        y[3] = ((x[3]<<3) & 0xF8) | ((x[4]>>5) & 0x07);
        y[4] = ((x[4]<<3) & 0xF8) | ((x[5]>>5) & 0x07);
        y[5] = ((x[5]<<3) & 0xF8) | ((x[6]>>5) & 0x07);
        y[6] = ((x[6]<<3) & 0xF8) | ((x[7]>>5) & 0x07);
        y[7] = ((x[7]<<3) & 0xF8);
        return y;
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
    public void writeDouble(double v) throws IOException {
      
      long rawLongBits = Double.doubleToLongBits(v);
      
      if (_realFmtCode == VicarDataFormat.REAL_FMT_VAX) {
	int exp, expl, exph;

	_vaxDouble[0] = _ieeeDouble[0] = (int) (rawLongBits >>> 56) & 0xFF;
	_vaxDouble[1] = _ieeeDouble[1] = (int) (rawLongBits >>> 48) & 0xFF;
	_vaxDouble[2] = _ieeeDouble[2] = (int) (rawLongBits >>> 40) & 0xFF;
	_vaxDouble[3] = _ieeeDouble[3] = (int) (rawLongBits >>> 32) & 0xFF;
	_vaxDouble[4] = _ieeeDouble[4] = (int) (rawLongBits >>> 24) & 0xFF;
	_vaxDouble[5] = _ieeeDouble[5] = (int) (rawLongBits >>> 16) & 0xFF;
	_vaxDouble[6] = _ieeeDouble[6] = (int) (rawLongBits >>>  8) & 0xFF;
	_vaxDouble[7] = _ieeeDouble[7] = (int) (rawLongBits >>>  0) & 0xFF;

	expl = (_ieeeDouble[1] >> 4) & 0x0F;
	exph = _ieeeDouble[0] & 0x7F;
	exp = ((exph << 4) & 0x07F0) | expl;
	
	// Exponent 2047 means NaN or Infinity, exponents 1150 to 2046 are
	// too large for VAX notation.  In either case, set to sign *
	// highest possible number.

	if (exp >= 1150) {		      // Infinity or NaN or too big
	  _vaxDouble[0] = 0x7F | (_ieeeDouble[0]&0x80);
	  _vaxDouble[1] = 0xFF;
	  _vaxDouble[2] = 0xFF;
	  _vaxDouble[3] = 0xFF;
	  _vaxDouble[4] = _vaxDouble[5] = _vaxDouble[6] = _vaxDouble[7] = 0xFF;
	}

	else if (exp <= 894) {	// Too small or zero
	  _vaxDouble[0] = _vaxDouble[1] = _vaxDouble[2] = _vaxDouble[3] = 0;
	  _vaxDouble[4] = _vaxDouble[5] = _vaxDouble[6] = _vaxDouble[7] = 0;
	}

	else {				// Normal case
	  exp -= 894;			// Change to VAX bias
	  expl = exp;	                // Now in range 1..255
	  
	  _vaxDouble = shift_left_3(_vaxDouble, _ieeeDouble); // Shift mantissa
	  
	  _vaxDouble[0] = (_ieeeDouble[0]&0x80) | ((expl>>1) & 0x7F);
	  _vaxDouble[1] = (_vaxDouble[1]&0x7f)  | ((expl<<7) & 0x80);
	}

	// write the bytes in rational order:

	out.write(_vaxDouble[1] & 0xFF);
	out.write(_vaxDouble[0] & 0xFF);
	out.write(_vaxDouble[3] & 0xFF);
	out.write(_vaxDouble[2] & 0xFF);
	out.write(_vaxDouble[5] & 0xFF);
	out.write(_vaxDouble[4] & 0xFF);
	out.write(_vaxDouble[7] & 0xFF);
	out.write(_vaxDouble[6] & 0xFF);
      }
      else if (_realFmtCode == VicarDataFormat.REAL_FMT_RIEEE) {
	out.write((int)(rawLongBits >>>  0) & 0xFF);
	out.write((int)(rawLongBits >>>  8) & 0xFF);
	out.write((int)(rawLongBits >>> 16) & 0xFF);
	out.write((int)(rawLongBits >>> 24) & 0xFF);
	out.write((int)(rawLongBits >>> 32) & 0xFF);
	out.write((int)(rawLongBits >>> 40) & 0xFF);
	out.write((int)(rawLongBits >>> 48) & 0xFF);
	out.write((int)(rawLongBits >>> 56) & 0xFF);
      }
      else { // it's IEEE
	out.write((int)(rawLongBits >>> 56) & 0xFF);
	out.write((int)(rawLongBits >>> 48) & 0xFF);
	out.write((int)(rawLongBits >>> 40) & 0xFF);
	out.write((int)(rawLongBits >>> 32) & 0xFF);
	out.write((int)(rawLongBits >>> 24) & 0xFF);
	out.write((int)(rawLongBits >>> 16) & 0xFF);
	out.write((int)(rawLongBits >>>  8) & 0xFF);
	out.write((int)(rawLongBits >>>  0) & 0xFF);
	
      }

    }

    /**
     * Writes a <code>boolean</code> to the underlying output stream as 
     * a 1-byte value. The value <code>true</code> is written out as the 
     * value <code>(byte)1</code>; the value <code>false</code> is 
     * written out as the value <code>(byte)0</code>.
     *
     * @param      v   a <code>boolean</code> value to be written.
     * @exception  IOException  if an I/O error occurs.
     * @see        java.io.FilterOutputStream#out
     * @since      JDK1.0
     */
    public final void writeBoolean(boolean v) throws IOException {
	out.writeBoolean(v);
    }
    /**
     * Writes the specified byte to the underlying output stream. 
     *
     * @param      b   the <code>byte</code> to be written.
     * @exception  IOException  if an I/O error occurs.
     * @see        java.io.FilterOutputStream#out
     * @since      JDK1.0
     */
    public synchronized void write(int b) throws IOException {
	out.write(b);
    }

    /**
     * Writes <code>len</code> bytes from the specified byte array 
     * starting at offset <code>off</code> to the underlying output stream.
     *
     * @param      b     the data.
     * @param      off   the start offset in the data.
     * @param      len   the number of bytes to write.
     * @exception  IOException  if an I/O error occurs.
     * @see        java.io.FilterOutputStream#out
     * @since      JDK1.0
     */
    public synchronized void write(byte b[], int off, int len)
	throws IOException
    {
	out.write(b, off, len);
    }

    /**
     * Flushes this data output stream. This forces any buffered output 
     * bytes to be written out to the stream. 
     * <p>
     * Because there is no flush on the underlying <code>DataOutput</code>,
     * <em>this function is a no-op!!</code>
     *
     * @exception  IOException  if an I/O error occurs.
     * @see        java.io.FilterOutputStream#out
     * @see        java.io.OutputStream#flush()
     * @since      JDK1.0
     */
    public void flush() throws IOException {
    }

    /**
     * Writes out a <code>byte</code> to the underlying output stream as 
     * a 1-byte value. 
     *
     * @param      v   a <code>byte</code> value to be written.
     * @exception  IOException  if an I/O error occurs.
     * @see        java.io.FilterOutputStream#out
     * @since      JDK1.0
     */
    public final void writeByte(int v) throws IOException {
	out.writeByte(v);
    }

    /**
     * Writes a <code>char</code> to the underlying output stream as a 
     * 2-byte value, high byte first. 
     *
     * @param      v   a <code>char</code> value to be written.
     * @exception  IOException  if an I/O error occurs.
     * @see        java.io.FilterOutputStream#out
     * @since      JDK1.0
     */
    public final void writeChar(int v) throws IOException {
	out.writeChar(v);
    }

    /**
     * Writes out the string to the underlying output stream as a 
     * sequence of bytes. Each character in the string is written out, in 
     * sequence, by discarding its high eight bits. 
     *
     * @param      s   a string of bytes to be written.
     * @exception  IOException  if an I/O error occurs.
     * @see        java.io.FilterOutputStream#out
     * @since      JDK1.0
     */
    public final void writeBytes(String s) throws IOException {
	out.writeBytes(s);
    }

    /**
     * Writes a string to the underlying output stream as a sequence of 
     * characters. Each character is written to the data output stream as 
     * if by the <code>writeChar</code> method. 
     *
     * @param      s   a <code>String</code> value to be written.
     * @exception  IOException  if an I/O error occurs.
     * @see        java.io.DataOutputStream#writeChar(int)
     * @see        java.io.FilterOutputStream#out
     * @since      JDK1.0
     */
    public final void writeChars(String s) throws IOException {
	out.writeChars(s);
    }

    /**
     * Writes a string to the underlying output stream using UTF-8 
     * encoding in a machine-independent manner. 
     * <p>
     * First, two bytes are written to the output stream as if by the 
     * <code>writeShort</code> method giving the number of bytes to 
     * follow. This value is the number of bytes actually written out, 
     * not the length of the string. Following the length, each character 
     * of the string is output, in sequence, using the UTF-8 encoding 
     * for the character. 
     *
     * @param      str   a string to be written.
     * @exception  IOException  if an I/O error occurs.
     * @since      JDK1.0
     */
    public final void writeUTF(String str) throws IOException {
	out.writeUTF(str);
    }

}

