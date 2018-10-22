/*
 * Created on Sep 3, 2003
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package jpl.mipl.io.vicar;

/**
 * @author Steve Levoe
 *
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 * 
 * This Object has the same elemnts as the values returned by the OAL lib 
 * OaGetImageKeywords. These are the values the java PDS reader need to know 
 * about an image to construct ColorModels, SampleModels and SystemLabels
 * Methods to construct those Objects will be added. For now it is an object
 *  which will be passed to the native methods. ll the values will be filled 
 * in a once. Then this Object can be used on the java side.
 * Get all these values filled in by on Native call rather than individual calls
 * for each value.
 * Likely this will be filled in when the image is opened
 */
public class OaImageKeywords {
	
	// ODLTREE image_node // this is only on the native side. It is the input to 
	// the oal FUNCTION OaGetImageKeywords
	long _lines;
	long _line_samples;
	long _sample_bits;
	String _sample_type_str;
	String _PDSDataType;
	long _bands;
	int _band_storage_type;
	long _line_prefix_bytes;
	long _line_suffix_bytes;
	int _encoding_type; // not really needed or used
	
	// vicar equivalents
	String _org;
	int _org_code = 0;
	int _format_code; // data type
	String _format;
	
	// boolean _debug = true;
	boolean _debug = false;
	
	boolean _headerHasBeenRead = false; 
	// flag so we know if the native read of the header has been done and was successful
	// do we need an error code for failure? or will that happen in some other way??
	
	String _headerString;
	// if headerString isn't null it must have something usefull in it
	// a complete copy of the header as a String. Could be used by PDSLabelToDOM
	
	// band_storage_types from oal.h - JAVA enums
	// public static final int OA_UNKNOWN_BAND_STORAGE_TYPE = 0;
	public static final int OA_BAND_SEQUENTIAL = 0;
	public static final int OA_LINE_INTERLEAVED = 1;
	public static final int OA_SAMPLE_INTERLEAVED = 2;
	// will have a method to translate these to the vicar ORG Strings
	// BIL BIP BSQ
	
	// we will need to translate from sample_type_str to vicar "format"
	
		// protected int _org_code;
		public static final int ORG_BSQ = 0;
		public static final int ORG_BIL = 1;
		public static final int ORG_BIP = 2;
		
		String[] org_code_strings = {"BSQ","BIL","BIP"};
		

		/** Codes for Data Type, used for efficiency when we need to compare a lot*/

		// protected int _format_code;
		public static final int TYPE_BYTE = 0;
		public static final int TYPE_HALF = 1;
		public static final int TYPE_FULL = 2;
		public static final int TYPE_REAL = 3;
		public static final int TYPE_DOUB = 4;
		public static final int TYPE_COMP = 5;
		public static final int TYPE_USHORT = 6;	// not a real VICAR type
		
		String[] format_code_strings = {"BYTE","HALF","FULL","REAL","DOUB","COMP","USHORT"};
	
	// encoding_type refers to compression. Java shouldn't care. We should receive 
	// uncompressed image pixel values from the native OAL libraries
	
	// constructor
	public  OaImageKeywords()
		{
			if (_debug) System.out.println("OaImageKeywords Constructor");
		setDefaults();
		
		}

	public void setDefaults() {
			_lines = 0;
			_line_samples = 0;
			_sample_bits = 0;
			_sample_type_str = "-"; // this is very complicated - its the returned value
			// in OAL we will use OaStrtoPDSDataType(sample_type_str, OA_BINARY_INTERCHANGE_FORMAT);
			// this should be more familiar looking values???
			_PDSDataType = "-";
			_bands = 1;
			_band_storage_type = OA_BAND_SEQUENTIAL ; 
			_line_prefix_bytes = 0;
			_line_suffix_bytes = 0;
			// encoding_type; 
			_org = "BYTE";
			_org_code = TYPE_BYTE;
			_format = "BSQ";
			_format_code = ORG_BSQ;
	}
	
	/*
	public SampleModel createSampleModel() {
	}
	
	public ColorModel createColorModel() {
	}
	
	public SystemLabel createSystemLabel() {
	}
	*/
	
	// accessor methods
	public void set_lines(long lines) {
			_lines = lines;
		}		
	public long get_lines() {
		return _lines;
	}
	
	public void set_line_samples(long line_samples) {
			_line_samples = line_samples;
		}	
	public long get_line_samples() {
		return _line_samples;
	}
	
	public long get_sample_bits() {
			return _sample_bits;
		}
	public void set_sample_bits(long sample_bits) {
		_sample_bits = sample_bits;
	}
	
	public void set_sample_type_str(String sample_type_str) {
			_sample_type_str = sample_type_str;
		}
	public String get_sample_type_str() {
		return _sample_type_str;
	}
	
	/* no setter for this, it is set from sample_bits and sample_type_str
	 * 
	 */
	public String get_PDSDataType() {
		return _PDSDataType ;
	}
	
	public void set_bands(long b) {
			_bands = b;
		}
	public long get_bands() {
		return _bands;
	}
	
	public int get_band_storage_type () {
		return _band_storage_type;
	}		
	public void set_band_storage_type( int band_storage_type) {
		
		// band_storage_type
		_band_storage_type = band_storage_type;
		// set org_code and then org based on it
	}
	
	public String get_org() {
		/*	no matter how the image data is stored in the file
		* OAL gets a single band at a time
		* always return"BSQ"
		int org_code = get_org_code();
		return org_code_strings[org_code];
		* org is used to create the SampleModel
		**/
		return "BSQ";
		
	}
		
	public int get_org_code() {		
			_org_code = _band_storage_type ;
			// use BSQ always here too
			
		// return _org_code;	
		return ORG_BSQ;	
	}
		
	public String get_format() {
		
		int format_code = get_format_code();
		return format_code_strings[format_code];
		// return _format;
	}
	
	public int get_format_code() {
		
			_format_code = determine_format_code();
			return _format_code;
		}
	
	public void set_line_prefix_bytes(long lpb) {
			_line_prefix_bytes = lpb;
		}	
	public long get_line_prefix_bytes() {
		return _line_prefix_bytes;
	}
	
	public void set_line_suffix_bytes(long lsb) {
			_line_suffix_bytes = lsb;
		}
	public long get_line_suffix_bytes() {
		return _line_suffix_bytes;
	}

	public int get_encoding_type() {
			return _encoding_type;
		}
		
		public void set_encoding_type(int type) {
			
			_encoding_type = type;
		}
	/* this is a place to store this string. Oal doesn't seem to
	 * have an easy way to get the header's String. 
	 * It will proably be better to grab it on the java side.
	 */
	public String get_headerString() {
		return _headerString;
	}
	
	public int determine_format_code() {
		
		// NEED A COMPREHENSIVE LIST OF PDS values, should be in oal.h
		int format_code = TYPE_BYTE; 
		long bits = _sample_bits;
		
		if (_sample_type_str.equalsIgnoreCase("IEEE_REAL") && bits == 64) {
			format_code = TYPE_DOUB ;
			}
		
		if (_sample_type_str.equalsIgnoreCase("IEEE_REAL") && bits == 32) {
			format_code = TYPE_REAL ; 						
		}
				
		if (_sample_type_str.equalsIgnoreCase("PC_REAL") && bits == 64) {
			format_code = TYPE_DOUB ;
			}
			
		if (_sample_type_str.equalsIgnoreCase("PC_REAL") && bits == 32) {
			format_code = TYPE_REAL ;
			}
        
		if (_sample_type_str.equalsIgnoreCase("MSB_INTEGER") && bits == 16) {
				
				format_code = TYPE_HALF ; // "USHORT""
				
        	
		} else if ( bits == 16) {
						//	can there be LSB_INTEGER ???
				format_code = TYPE_HALF ; // "USHORT""
		}
        
		if (_sample_type_str.equalsIgnoreCase("MSB_UNSIGNED_INTEGER") || 
			_sample_type_str.equalsIgnoreCase("UNSIGNED_INTEGER"))  {
				// intFormat = "HIGH";  // "LOW"
				// unsignedFlag = true ;
			if (_sample_bits == 16) { // check sample size 
					format_code = TYPE_HALF; 
							// format = "USHORT" ; 
				}
		}
		else {
			// unsignedFlag = false;
		}
		
		System.out.println("determine_format_code "+format_code+" "+_sample_type_str+" "+bits);
		return format_code;
	}
	
	
	
	public String toString() {
		StringBuffer sb = new StringBuffer();
	
	// sb.append("OA_UNKNOWN_BAND_STORAGE_TYPE = "+OA_UNKNOWN_BAND_STORAGE_TYPE+"\n");
	sb.append("OA_BAND_SEQUENTIAL = "+OA_BAND_SEQUENTIAL+"\n");
	sb.append("OA_LINE_INTERLEAVED = "+OA_LINE_INTERLEAVED+"\n");
	sb.append("OA_SAMPLE_INTERLEAVED = "+OA_SAMPLE_INTERLEAVED+"\n");
	// will have a method to translate these to the vicar ORG Strings
	// BIL BIP BSQ
	
	// we will need to tramslate from sample_type_str to vicar "format"
	
		// protected int _org_code;
		sb.append("protected int _org_code"+"\n");
		sb.append("ORG_BSQ = "+ORG_BSQ+"  org_code_strings[]="+org_code_strings[ORG_BSQ]+"\n");
		sb.append("ORG_BIL = "+ORG_BIL+"  org_code_strings[]="+org_code_strings[ORG_BIL]+"\n");
		sb.append("ORG_BIP = "+ORG_BIP+"  org_code_strings[]="+org_code_strings[ORG_BIP]+"\n");
		
		// String[] org_code_strings = {"BSQ","BIL","BIP"};
		
		/** Codes for Data Type, used for efficiency when we need to compare a lot*/

		// protected int _format_code;
		sb.append("protected int _format_code"+"\n");
		sb.append("TYPE_BYTE = "+TYPE_BYTE+"  format_code_strings[]="+format_code_strings[TYPE_BYTE]+"\n");
		sb.append("TYPE_HALF = "+TYPE_HALF+"  format_code_strings[]="+format_code_strings[TYPE_HALF]+"\n");
		sb.append("TYPE_FULL = "+TYPE_FULL+"  format_code_strings[]="+format_code_strings[TYPE_FULL]+"\n");
		sb.append("TYPE_REAL = "+TYPE_REAL+"  format_code_strings[]="+format_code_strings[TYPE_REAL]+"\n");
		sb.append("TYPE_DOUB = "+TYPE_DOUB+"  format_code_strings[]="+format_code_strings[TYPE_DOUB]+"\n");
		sb.append("TYPE_COMP = "+TYPE_COMP+"  format_code_strings[]="+format_code_strings[TYPE_COMP]+"\n");
		sb.append("TYPE_USHORT = "+TYPE_USHORT+"  format_code_strings[]="+format_code_strings[TYPE_USHORT]+"\n");
		
		// String[] format_code_strings = {"BYTE","HALF","FULL","REAL","DOUB","COMP","USHORT"};
//		sb.append(" oal FUNCTION OaGetImageKeywords");
		sb.append("_lines="+_lines+"\n");
		sb.append("_line_samples="+_line_samples+"\n");
		sb.append("_sample_bits="+_sample_bits+"\n");
		sb.append("_sample_type_str="+_sample_type_str+"\n");
		sb.append("_PDSDataType="+_PDSDataType+"\n");
		sb.append("_bands="+_bands+"\n");
		sb.append("_band_storage_type="+_band_storage_type+"\n");
		sb.append("_line_prefix_bytes="+_line_prefix_bytes+"\n");
		sb.append("_line_suffix_bytes="+_line_suffix_bytes+"\n");
		sb.append("_encoding_type="+_encoding_type+" not really needed or used"+"\n");
	
		sb.append(" vicar equivalents"+"\n");
		sb.append(" _org="+get_org()+"\n");
		sb.append(" _org_code="+get_org_code()+"\n");
		sb.append(" _format_code="+get_format_code()+"\n");
		sb.append(" _format="+get_format()+"\n");
		
		
		return sb.toString();
	}
	
	/**************************************************************
		* Create a SystemLabel from the contents of OaImageKeywords
		* This is specific to a PDS image.
		*
		* 
		*****/
		public SystemLabel  createSystemLabel() {
    	
			if (_debug) {
				System.out.println("========================================================");
				System.out.println("*************** OaImageKeywords.createSystemLabel ******");
				System.out.println(toString());
				System.out.println("========================================================");
			}
    	
    
        
			// use an ISIS system label since it has some extra elemnts specific
			// to PDS and ISIS images
			IsisSystemLabel sys = new IsisSystemLabel();
        
			String format = "BYTE"; // BYTE HALF FULL REAL DOUB COMP
			String org = "BSQ"; // BSQ BIL BIP
			int nl = 0;
			int ns = 0;
			int nb = 1;
			int bits = 8; // Per sample
       
			String host = "JAVA";
			// String intFormat = "LOW";
			String intFormat = "HIGH";
			String realFormat = "VAX"; // this is the default
			boolean unsignedFlag = true;
        
			// PDS ISIS specific items
			int axes = 3; 
			double core_base = 0.0;
			double core_multiplier = 1.0;
			int core_valid_minimum = 0;
			int bandsToUse[] = {0,1,2};
			boolean isRGB = false;
			int bsb = 0; // band suffix bytes
			int lsb = 0; // line suffix bytes
			int ssb = 0; // sample suffix bytes
			int suffix_items[] = {0,0,0};
        
			int line_suffix_bytes = 0;
			int line_prefix_bytes = 0;
        
			int lpb = 0; // line prefix Bytes
        
			//------- set my values from the OaImageKeywords
        
			line_suffix_bytes = (int) _line_suffix_bytes;
			line_prefix_bytes = (int) _line_prefix_bytes;
			nl = (int) _lines;
			ns = (int) _line_samples;
			nb = (int) _bands;
			bits = (int) _sample_bits;
        
			// eventually we may know if this is an Image, Cube, table etc
			// assume only Images now
        
			// convert PDS SAMPLE_TYPE to vicar
			if (_sample_type_str.equalsIgnoreCase("IEEE_REAL") && bits == 32) {
				format = "REAL" ; 
				intFormat = "HIGH";  // "HIGH"
				realFormat = "IEEE";
				unsignedFlag = false;
				System.out.println("REAL");
			}
        
			if (_sample_type_str.equalsIgnoreCase("MSB_INTEGER") && bits == 16) {
				
				format = "HALF" ; // "USHORT""
				intFormat = "HIGH";  // "HIGH"
				unsignedFlag = false;
        	
				// System.out.println("MSB_INTEGER HALF");
        	
			} else if ( bits == 16) {
				//	can there be LSB_INTEGER ???
				format = "HALF" ; // "USHORT""
				intFormat = "HIGH";  // "HIGH"
				unsignedFlag = false;
        	
				System.out.println("HALF");
			}
        
			if (_sample_type_str.equalsIgnoreCase("MSB_UNSIGNED_INTEGER") || 
				_sample_type_str.equalsIgnoreCase("UNSIGNED_INTEGER"))  {
				intFormat = "HIGH";  // "HIGH"
				unsignedFlag = true ;
				// intFormat = "LOW";  // "HIGH"
				if (_sample_bits == 16) { // check sample size 
					format = "HALF"; 
					// format = "USHORT" ; 
				}
			}
			else {
				unsignedFlag = false;
			}
    
    
		sys.setLinePrefixBytes(line_prefix_bytes);
		sys.setLineSuffixBytes(line_suffix_bytes);
        
		sys.setOrg(get_org()); // BSQ BIL BIP
		// org is needed before these so N1 N2 N3 will be set correctly
		sys.setNL(nl);
		sys.setNS(ns);
		sys.setNB(nb);
    
		// USHORT  was added to support PDS, vicar doesn't have USHORT - HALF is signed
		sys.setFormat(get_format()); // BYTE HALF FULL REAL DOUB COMP USHORT
       
		sys.setHost(host) ;// JAVA is default

		// host IS ACTUALLY NOT USED .. this is the important one
		sys.setIntFmt(intFormat ) ;// LOW default , HIGH
		sys.setRealFmt(realFormat ) ;// IEEE default , RIEEE
    
		return sys;
        
        
		}
	
}
