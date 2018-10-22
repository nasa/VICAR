/*
 * VicarBinaryLinePrefix.java
 * Created on Aug 24, 2004
 *
 * 
 * @author Steve Levoe
 */
package jpl.mipl.io.vicar;

import java.nio.ByteBuffer; // nio is new in 1.4.2


/**
 * 
 *
 * This class holds all the data for the binary prefixs of a Vicar image.
 * This class is intended to be a part of the VicarMetadata. When the image data 
 * is read into a RenderedImage/PlanarImage/BufferedImage the Binary Prefix data 
 * is skipped. The data buffer in this class will be filled by the reader.
 * 
 * This class allows the prefix data to be attached to the image via the
 * VicarMetadata class. The writer can use the prefix data when writing out the image data.
 * How the prefix data is used will be up to the writer. An ImageWriteParam may contain
 * a flag to specify if the prefix data is to be included in the output file.
 * Transcoders have the option of adding the prefix in the input metadata to the metadata 
 * of the output image (IIOImage) which contains the image and metadata.
 * The line prefix data is treated as binary data. No conversion or interpretaion of the bits 
 * is attempted. The _host variable in this Class carries the Host type. Currently that 
 * information is unused.
 * 
 * 
 * jConvertIIO is a conversion program which calls readers, writers transcoders and JAI
 * filter methods to perform image conversions.
 * Here is how the Line prefix data is used and transfered in the viar to PDS conversion
 * sequence.
 * For a Vicar image file:
 * VicarInputFile.readPrefixData() is called just after the image header is read if the 
 * image file contains prefix data. The value of NBB (line prefix bytes) indicate the presence of 
 * line prefix data.
 * VicarInputFile does the low level work for VicarImageReader. 	
 * I there is line prefix data a VicarBinaryLinePrefix Object is created and the data is added to it.
 * This Object is set into the VicarMetadata Object for the VicarImageReader.
 * If a PDS image output is requested the VicarToPDSTranscoder is called. If there is a 
 * VicarBinaryLinePrefix Object in the VicarMetadata it is set in the PDSDMetadata object of the 
 * trancoded metadata.
 * Then the PDSIMageWriter is called to write out the Image data and the transcoded PDSMetadata.
 * A PDSImageWriteParam Object is passed to the PDSIMageWriter to control the details of the image
 * write. The boolean flag addLinePrefix controls whether the line prefix data is written to the
 * output file. The default is true meaning the prefix will be written to the file. 
 * The command line argument USE_LINE_PREFIX=[true,false] can be used to overide the default.
 * This capabilitywas developed to support HRSC image files.
 * 
 * LINE_PREFIX_BYTES is the PDS keyword used to indicate the presence of line prefix data.
 * nbb is the vicar keyword.
 * 
 * 
 * 
 * 
 */
public class VicarBinaryLinePrefix {

	// public static void main(String[] args) {}
	
	protected int _nl ; // number of lines in this buffer
	protected int _nbb ; // number of prefix bytes (this is the prefix line size
	protected int _nb ; // number of bands in the image
	
	protected String _host ; // Host type, maybe used to determine endianess of the data
	
	protected String _bltype ; // Binary label type, taken from the input vicar file System label
	// passed here so it can be added to an output image
	protected String _bhost ;
	protected String _bintfmt; 
	protected String _brealfmt;
	
	
	protected String _org ; // organization of the file data 
	protected int _org_code ; // SystemLabel.ORG_BSQ SystemLabel.ORG_BIL SystemLabel.ORG_BIP
	// this is the org of this data NOT the imput file. 
	protected int _recsize ; // record size of the input file, may not be useful
	
	
	protected int _bufferSize ; // size in bytes
	protected byte[][] _data; // buffer to hold all the prefix data [band][byte]
	// use a Raster ??? nah
	// howse about ByteBuffer or DataBufferByte 
	protected ByteBuffer  _byteBuffer; // buffer to hold all the prefix data
	// ByteBuffer will do reformating so data could be retrieved as some 
	// other type like IEEE_REAL SHORT etc
	
	
	
	// Constructor
	// the databuffer to hold all the prefix data is allocated here
	public VicarBinaryLinePrefix(int nb, int nl, int nbb) {
		
		_nl = nl;
		_nb = nb;
		_nbb = nbb;
		_bufferSize = _nl * _nb * _nbb ;
		_host = "NATIVE";
		// allocate the ByteBuffer ?? it is abstract
		// check JDK version to be sure ByteBuffer is available ???
		_data = new byte[nb][_bufferSize];
	}
	
	
	// accessors
	// don't allow setting, only set via a Constructor
	
	public int get_nl() {
		return _nl ; // number of lines in this buffer
	}
	
	public int get_nb() {
			return _nb ; // number of bands in this buffer
	}
		
		
	public int get_nbb() {
			return _nbb ; // number of prefix bytes per line
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
	 * Returns the prefix for a line in a band
	 */
	public byte[] getPrefix(int band, int line) {
		byte[] b  = null; // set it to the data
		if (band > _nb) {
			throw new ArrayIndexOutOfBoundsException("band "+band+  " is > "+_nb);
		}
		
		b = new byte[_nbb];
		// return a pointer ??
		// band++ ; // can't multiply by band 0 
		// lines start at 0
		// check for some fancy array copy widget
		int start = line * _nbb ; 
		try {
		
		 for (int i = 0 ; i< _nbb ; i++) {
			b[i] = _data[band][start + i];
			}
			
		}
		catch (ArrayIndexOutOfBoundsException e) {
			System.out.println("ArrayAccessOutOfBoundsException "+e);
			System.out.println("VicarBinaryLinePrefix.getPrefix band="+(band-1)+" line="+line);
			e.printStackTrace();
			throw e; // let it get caught above
		}
		
		return b;
		
	
	}
	
	/**
	* Sets the prefix for a line in a band
	*/
	public void setPrefix(int band, int line, byte[] b) {
	// public void setPrefix(int band, int line, byte[] b, int size) {
		    // assume size is _nbb ???
			// set b into the data buffer at the correct location
			// how we use band is determined by the org
			// for now we will use BSQ always independant of how the data file 
			// was organized
			// band++ ; // can't multiply by band 0 
			if (band > _nb) {
					throw new ArrayIndexOutOfBoundsException("band "+band+  " is > "+_nb);
				}
			// lines start at 0
			// int start = line * band * _nbb ; 
			int start = line * _nbb ; 
			try {
			
			 for (int i =0 ; i< _nbb ; i++) {
				_data[band][start + i] = b[i];
			    }
			}
			catch (ArrayIndexOutOfBoundsException e) {
				System.out.println("ArrayAccessOutOfBoundsException "+e);
				System.out.println("VicarBinaryLinePrefix.setPrefix band="+(band-1)+" line="+line);
				e.printStackTrace();
				throw e; // let it get caught above
			}	
	
	}
	
	
	/**
	 * toString
	 * create a String to print out and 
	 */
	public void print() {
		int bi ;
		byte b;
				
		System.out.println("VicarBinaryLinePrefix");
		System.out.println("nbb="+_nbb+" nl="+_nl+" nb="+_nb);
		
		for (int band=0 ; band<_nb ; band++) {
		  System.out.println("band "+band+" *****************************");
		  for (int line=0 ; line<_nl ; line++) {
		  	bi = band;
			bi++ ; // can't multiply by band 0 
						// lines start at 0
			int start = line * _nbb ; 
			
			/**
			if ((line % 500 == 1) ||
				(line % 500 == 2) ||
				(line % 500 == 3) ){
				**/
			if ((line < 50)) {
			
			  System.out.print(band+" "+line+" "+start+" "+_nbb+" >");
			  for (int i =0 ; i< _nbb ; i++) {
				b = _data[band][start + i] ;
				Byte by = new Byte(b);
				System.out.print(" "+Byte.toString(b));
				}
			  System.out.println(" <*");
			}
		  }
			
			
		}
		// print out the contents for debug purposes
		
	}
	
}
