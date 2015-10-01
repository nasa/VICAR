/*
 * VicarBinarHeader.java
 * Created on Aug 24, 2004
 *
 * 
 * @author Steve Levoe
 */
package jpl.mipl.io.vicar;

import java.nio.ByteBuffer; // nio is new in 1.4.2

import jpl.mipl.io.util.DOMutils;


/**
 * 
 *
 * This class holds all the data for the binary header of a Vicar image.
 * This class is intended to be a part of the VicarMetadata. When the image data 
 * is read into a RenderedImage/PlanarImage/BufferedImage the Binary Prefix data 
 * is skipped. The data buffer in this class will be filled by the reader.
 * 
 * This class allows the binary header data to be attached to the image via the
 * VicarMetadata class. The writer can use the header data when writing out the image data.
 * How the header data is used will be up to the writer. An ImageWriteParam may contain
 * a flag to specify if the header data is to be included in the output file.
 * Transcoders have the option of adding the header in the input metadata to the metadata 
 * of the output image (IIOImage) which contains the image and metadata.
 * The binary header data is treated as binary data. No conversion or interpretaion of the bits 
 * is attempted. There are variables from the vicar label which may be used to help
 * interpret the data. They are BHOST, BLTYPE, BINTFMT, BREALFMT.
 * A VicarBinaryLabel may be obtained.
 * VicarOutputFile can use this Object dirctly
 *  to write the binary header data to a file.
 * 
 * 
 * 
 */
public class VicarBinaryHeader {

	// public static void main(String[] args) {}
	
	protected int _nlb ; // number of lines in this buffer
	protected int _recsize ; // number of prefix bytes in a record
	// the size of the buffer is recsize * nlb
	
	protected String _host ; // Host type, maybe used to determine endianess of the data
	
	protected String _bltype ; // Binary label type, taken from the input vicar file System label
	// passed here so it can be added to an output image
	protected String _bhost ;
	protected String _bintfmt; 
	protected String _brealfmt;
	
	
	protected String _org ; // organization of the file data 
	protected int _org_code ; // SystemLabel.ORG_BSQ SystemLabel.ORG_BIL SystemLabel.ORG_BIP
	// this is the org of this data NOT the imput file. 
	
	
	
	protected int _bufferSize ; // size in bytes
	protected byte[] _data; // buffer to hold all the prefix data [band][byte]
	// use a Raster ??? nah
	// howse about ByteBuffer or DataBufferByte 
	
	protected VicarDataFormat _binary_data_format;
	
	// boolean debug = true;
	boolean debug = false;
	
	// Constructor
	// the databuffer to hold all the prefix data is allocated here
	public VicarBinaryHeader(int recsize, int nlb) {
		
		_recsize = recsize;
		_nlb = nlb;
		_bufferSize = _nlb * _recsize ;
		_host = "NATIVE";
		// allocate the ByteBuffer ?? it is abstract
		// check JDK version to be sure ByteBuffer is available ???
		_data = new byte[_bufferSize];
	}
	
	public VicarBinaryHeader(int recsize, int nlb, byte[] data) {
		
			_recsize = recsize;
			_nlb = nlb;
			_bufferSize = _nlb * _recsize ;
			_host = "NATIVE";
			// allocate the ByteBuffer ?? it is abstract
			// check JDK version to be sure ByteBuffer is available ???
			_data = new byte[_bufferSize];
			// data.length must be the same as _bufferSize
			setData(data);
		}
	// accessors
	// don't allow setting, only set via a Constructor
	
	public int get_recsize() {
		return _recsize ; // number of lines in this buffer
	}
			
	public int get_nlb() {
			return _nlb ; // number of prefix bytes per line
	}
	
	
	/**
	 *  All of these MUST be transfered from the input to the output
	 * for the reader to have a chance of correctly interpreting the 
	 * prefix data
	 * 
	 */
	/**
	 * set this from the input
	 * May be used on output to reformat the data
	 * or at least help indicate what format it may be in.
	 */
	public void set_bltype(String bltype) {
		_bltype = bltype;
		}
	
	/**
	* May be used on output to reformat the data
	* or at least help indicate what format it may be in.
	*/	
	public String get_bltype() {
		return _bltype;
	}
	
		/**
		 * set this from the input
		 * May be used on output to reformat the data
		 * or at least help indicate what format it may be in.
		 */
		public void set_bhost(String bhost) {
			_bhost = bhost;
			}
	
		/**
		* May be used on output to reformat the data
		* or at least help indicate what format it may be in.
		*/	
		public String get_bhost() {
			return _bhost;
		}
		
		/**
		 * set this from the input
		 * May be used on output to reformat the data
		 * or at least help indicate what format it may be in.
		 */
		public void set_bintfmt(String bintfmt) {
			_bintfmt = bintfmt;
			}
	
		/**
		* May be used on output to reformat the data
		* or at least help indicate what format it may be in.
		*/	
		public String get_bintfmt() {
			return _bintfmt;
		}
		
		/**
		 * set this from the input
		 * May be used on output to reformat the data
		 * or at least help indicate what format it may be in.
		 */
		public void set_brealfmt(String brealfmt) {
			_brealfmt = brealfmt;
			}
	
		/**
		* May be used on output to reformat the data
		* or at least help indicate what format it may be in.
		*/	
		public String get_brealfmt() {
			return _brealfmt;
		}
	
	
	/**
		 * set this from the input
		 * May be used on output to reformat the data
		 * or at least help indicate what format it may be in.
		 */
		public void set_host(String host) {
			_host = host;
			}
	
		/**
		* May be used on output to reformat the data
		* or at least help indicate what format it may be in.
		*/	
		public String get_host() {
			return _host;
		}
	
	/**
	 * this is the Object that VicarOutputFile knows how to use to write the header.
	 * Create one from the data buffer.
	 * @return
	 * @throws IOException
	 */
	public  VicarBinaryLabel getVicarBinaryLabel() // throws IOException
		{
		if (_nlb == 0 || _data == null)	// no header!
			return null;

		

		if (_binary_data_format == null)
			_binary_data_format = new VicarDataFormat(_bhost,
					_bintfmt, _brealfmt);

		int size = _nlb * _recsize;

		VicarBinaryLabel vbl = new VicarBinaryLabel(size, _binary_data_format);

		// Read the entire header
		
		// byte[] vbl_buf = vbl._buf;
		// copy our data into it
		for (int i=0 ; i< size ; i++){
			vbl._buf[i] = _data[i];
			
		}
		

		return vbl;
		}	
	/**
	 * Returns the prefix for a line in a band
	 */
	public byte[] getData() {
		
		return _data;
			
	}
	
	/**
	* Sets the header data
	*/
	public void setData(byte[] b) {
	
		 int size = b.length;
		 try {
			
			if (size != _bufferSize) {
				throw new ArrayIndexOutOfBoundsException(
					"VicarBinaryHeader.setHeader() set data buffer does not match allocated size "+size+" "+_bufferSize);
				
			} else {
				// for (int i =0 ; i< _recsize ; i++) {
				for (int i =0 ; i< size ; i++) {
					_data[i] = b[i];
					}
			}
			
		  }
		  catch (ArrayIndexOutOfBoundsException e) {
				System.out.println("ArrayAccessOutOfBoundsException "+e);
				System.out.println("VicarBinaryHeader.setHeader _recsize="+_recsize+" size="+size);
				e.printStackTrace();
				throw e; // let it get caught above
		  }	
		  
		
		  if (debug) {
			  System.out.println("VicarBinaryHeader.setData  _recsize="+_recsize+" size="+size);
			  DOMutils domUtils = new DOMutils();
			  domUtils.dataDump(b, _recsize, 8, 5);
			  
			  domUtils.dataDump(_data, _recsize, 8, 5);
		  }
		  
	
	}
	
	
	/**
	 * toString
	 * create a String to print out and 
	 */
	public void print() {
		int bi ;
		byte b;
				
		System.out.println("VicarHeader");
		System.out.println("nlb="+_nlb+" recsize="+_recsize+" _bufferSize="+_bufferSize);		
				  
		// print out the contents for debug purposes
		
	}
	
}
