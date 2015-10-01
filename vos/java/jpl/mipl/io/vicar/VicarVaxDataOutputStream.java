package jpl.mipl.io.vicar;

import java.io.*;

/**
 * VicarVaxDataOutputStream lets an application write primitive
 * data types to an output stream; the data are converted 
 * (if necessary) to the int and real formats specified in 
 * the VicarDataFormat object that is passed in via the
 * constructor.
 * <p>
 */

public
class VicarVaxDataOutputStream extends VicarDataOutputStream
implements DataOutput {

    /**
     * Creates a new stream to write data to the specified underlying output
     * stream. 
     * @param out    the output stream you're wrapping
     * @param format the data format to convert *to*
     */
    public VicarVaxDataOutputStream(OutputStream out, int intFmtCode,
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
      int rawIntBits = Float.floatToIntBits(v);

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
    public final void writeDouble(double v) throws IOException {
      
      long rawLongBits = Double.doubleToLongBits(v);
      
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
}
