package jpl.mipl.io.vicar;

import java.util.*;
import java.beans.*;
import java.lang.reflect.*;
import java.text.*;


/**
 * This class is a bean that maintains all VICAR system label information.
 * see SystemLabel.
 * This class adds some PDS4 specific items which are not a part of
 * a vicar label. The SystemLabel is used by VicarIO to control the reading of 
 * image data from the file and transfering it to the raster
 * Support for the following PDS4 items is added:
 * suffix pixels
 * <BR>selection of bands, since a cube may hold lots (>3)
 * <BR>core_base
 * <BR>core_multiplier
 * <BR>detachedFilename
 * We will also keep track of some PDS4 specific identifiers.
 * Some will be used to set the values needed by this class to read the data properly
 * Not all of the PDS4 system label features are implemented yet.
 * special pixels are NOT supported yet
 * 
 * Adding information about the Header of the detached file
 */

public class PDS4SystemLabel extends SystemLabel {
    
    // PDS4 specific items, these are added to the ones in SystemLabel
    // all suffix items are 4 bytes, the suffix bytes are the number of bytes of suffix
    // if there were 2 suffix items that would be 8 bytes of suffix
    int _bsb = 0; // band suffix
    int _lsb = 0; // line suffix
    int _ssb = 0; // sample suffix
    
    int _lpb = 0; // line prefix bytes - added for PDS, PDS4 has no prefix ?!?
    // no need identified for band or sample prefix variables
    
    int _line_prefix_bytes = 0; // PDS only
    // this is the same as Vicar nbb
    int _line_suffix_bytes = 0; // PDS only
    
    /* we could add another item to indicate how many bytes of the suffix are used
    * for the case where we want to use the suffix as data.
    * data type core base and multiplier are also available for suffixs
    * the above numbers are used so we know how many bytes to 
    * skip/ignore while reading in the image data
    */
    
    // all PDS4 data items are real (float) values even if they are stored as 
    // bytes, half or int
    // to obtain the actual float value the stored item is
    // "real_value" = core_base + (core_multiplier * (float)(stored_value) )
    double _core_base = 0.0; 
    double _core_multiplier = 1.0;
    // a version of VicarDataInputDIWrapper will be used to read the data
    
    // flag to indicate if the actual value stored in the file is returned (false)
    // true indicates the above equation using core_base, core_multiplier will be
    // applied and the float "real_value" will be returned
    boolean _returnFloat = true; 
    
    boolean _isRGB = false; // if true 3 bands will be returned as an RGB image
    // if false a single band will be returned as a gray scale image or ALL bands
    // in the file will be returned. Then it will be up to the display program to 
    // transform the image to a viewable form
    int _bandsToUse[] = {0,1,2}; // the bands to use as rgb
    
    // the band at [0] will be used as Red
    // the band at [1] will be used as Green
    // the band at [2] will be used as Blue
    // for a single band (gray scale) []0] will be the band returned
    
    // new PDS4 specific items
    String _detachedFilename ="";
    String _data_type = "";
    boolean _detachedFile = true; // always true for P{DS4 ???
    
    HashMap<String, String> data_type2format = new HashMap<String,String>();
	HashMap<String, Boolean>  data_type2unsignedFlag = new HashMap<String,Boolean>();	
	HashMap<String, String>  data_type2intFormat = new HashMap<String,String>();		
	HashMap<String, String>  data_type2realFormat = new HashMap<String,String>();	
	
	// PDS4 specific values.
	// these values are easily found at the same time the SystemLabel is created
	// <File> and <Header>
	int[] _header_object_length_bytes = {0};
	int[] _header_offset_bytes = {0};
	// offset in bytes of the data file where the Header starts
	// if the label size is used this should be OK since the skip starts from 0
	// raw indicates there was no header on the data file. PDS3, VICAR, 
	// a file with no <Haeader> object indicate the data (detached) file is "RAW"
	// data will start at byte 0, offset_bytes = 0
	String[] _parsing_standard_id = {"RAW"}; 
	
	// this is the value from Array_2D_Image/offset or Array_3D_Image/offset or Array_3D_Spectrum/offset
	int _offset_bytes = 0; 
	
	int _filesize_bytes = 0;
	int _file_records = 0;
	String _checksum =  "00000000000000000000000000000000" ;
					// 	"d220dac0d1fe312f3f3b9c824f6ac294" ;
	
	// a RAW image may not have a header
	int _header_ct = 0;
	
	boolean debug = false;
	
	/***
    String[] header_offset_str =  new String[header_ct];
    int[] header_offset = {0};
    String[] header_offset_units = new String[header_ct];
    String[] header_object_length_str = new String[header_ct];
    int[] header_object_length = {0};
    String[] header_object_length_units = new String[header_ct];
    ****/
    
    
/***********************************************************************
 * Create an empty (default) SystemLabel.
 */
    public PDS4SystemLabel()
    {
	    super();
	    // itilialize the extra items for PDS4 
	    setPDS4Defaults();
	    initHashMaps();
    }
    
    
    /***********************************************************************
 * Initialize the only the PDS4 specific label items to the default values.
 */
    public void setPDS4Defaults()
    {
	_bsb = 0; // band suffix
    _lsb = 0; // line suffix
    _ssb = 0; // sample suffix
    
    _line_prefix_bytes = 0; // PDS only
    _line_suffix_bytes = 0; // PDS only
    
    _core_base = 0.0; 
    _core_multiplier = 1.0;
    
    _returnFloat = false; // true; 
    
    _isRGB = false; 
    // _bandsToUse = {0,1,2};

    // redo these if the PDS4 values force us to override these methods
	// calcPixelSize();
	// calcRecsize();
    // _offset_bytes = {0};
	// _parsing_standard_id = {"RAW"}; 
    }
    
    /** should overide this so PDS4 specific items are added
    public String toString()
    {
    }
    **/
  


		/********************************
		 * * initializes global TreeMaps used to determine the vicar value for PDS4 keywords
		 * There is no inline way to set the values into a TreeMap
		 * create a few hash tables to convert data_type to vicar:
		 * format, intFormat, realFormat, unsignedFlag
		 */
private void initHashMaps() {
		
	/* all the TreeMap objects are global. Initialized here */
	// HashMap data_type2format = new HashMap<String,String>();
	// HashMap data_type2unsignedFlag = new HashMap<String,Boolean>();	
	// HashMap data_type2intFormat = new HashMap<String,String>();		
	// HashMap data_type2realFormat = new HashMap<String,String>();		
    
    
    /**
     * The attribute pds:data_type must be equal to one of the following values 
     * 'ComplexLSB16', 'ComplexLSB8', 'ComplexMSB16', 'ComplexMSB8', 'IEEE754LSBDouble', 
     * 'IEEE754LSBSingle', 'IEEE754MSBDouble', 'IEEE754MSBSingle', 'SignedBitString', 
     * 'SignedByte', 'SignedLSB2', 'SignedLSB4', 'SignedLSB8', 'SignedMSB2', 'SignedMSB4', 
     * 'SignedMSB8', 'UnsignedBitString', 'UnsignedByte', 'UnsignedLSB2', 'UnsignedLSB4', 
     * 'UnsignedLSB8', 'UnsignedMSB2', 'UnsignedMSB4', 'UnsignedMSB8'.</sch:assert>
     * This is the complete set of allowed values. Many do not make sense for binary images
     * The value for those will be some sort of default or Error value
     *****/
	
	// vicar options: 
	// "NS" for not supported ??
	 // BYTE HALF FULL REAL DOUB COMP ??? USHORT
	data_type2format.put("ComplexLSB16","COMP"); 	
	data_type2format.put("ComplexLSB8","COMP"); 
	data_type2format.put("ComplexMSB16", "COMP");
	data_type2format.put("ComplexMSB8", "COMP");
	data_type2format.put("IEEE754LSBDouble", "DOUB");
    data_type2format.put("IEEE754LSBSingle", "REAL");
    data_type2format.put("IEEE754MSBDouble", "DOUB"); 
    data_type2format.put("IEEE754MSBSingle", "REAL"); 
    data_type2format.put("SignedBitString", "NS");
    data_type2format.put("SignedByte", "BYTE");
    data_type2format.put("SignedLSB2", "HALF");
    data_type2format.put("SignedLSB4", "FULL");
    data_type2format.put("SignedLSB8", "NS");
    data_type2format.put("SignedMSB2", "HALF");
    data_type2format.put("SignedMSB4", "FULL"); 
    data_type2format.put("SignedMSB8", "NS"); 
    data_type2format.put("UnsignedBitString","NS"); 
    data_type2format.put("UnsignedByte", "BYTE");
    data_type2format.put("UnsignedLSB2", "USHORT"); // "USHORT"
    data_type2format.put("UnsignedLSB4", "FULL");
    data_type2format.put("UnsignedLSB8", "NS");
    data_type2format.put("UnsignedMSB2", "HALF");
    data_type2format.put("UnsignedMSB4", "FULL");
    data_type2format.put("UnsignedMSB8", "NS");
    
    data_type2realFormat.put("ComplexLSB16","RIEEE"); 	
	data_type2realFormat.put("ComplexLSB8","RIEEE"); 
	data_type2realFormat.put("ComplexMSB16", "IEEE");
	data_type2realFormat.put("ComplexMSB8", "IEEE");
	data_type2realFormat.put("IEEE754LSBDouble", "RIEEE");
    data_type2realFormat.put("IEEE754LSBSingle", "RIEEE");
    data_type2realFormat.put("IEEE754MSBDouble", "IEEE"); 
    data_type2realFormat.put("IEEE754MSBSingle", "IEEE"); 
    data_type2realFormat.put("SignedBitString", "NS");
    // those don't really matter since they won't be used internally 
    data_type2realFormat.put("SignedByte", "IEEE");
    data_type2realFormat.put("SignedLSB2", "IEEE");
    data_type2realFormat.put("SignedLSB4", "IEEE");
    data_type2realFormat.put("SignedLSB8", "NS");
    data_type2realFormat.put("SignedMSB2", "IEEE");
    data_type2realFormat.put("SignedMSB4", "IEEE"); 
    data_type2realFormat.put("SignedMSB8", "NS"); 
    data_type2realFormat.put("UnsignedBitString","NS"); 
    data_type2realFormat.put("UnsignedByte", "IEEE");
    data_type2realFormat.put("UnsignedLSB2", "IEEE");
    data_type2realFormat.put("UnsignedLSB4", "IEEE");
    data_type2realFormat.put("UnsignedLSB8", "NS");
    data_type2realFormat.put("UnsignedMSB2", "IEEE");
    data_type2realFormat.put("UnsignedMSB4", "IEEE");
    data_type2realFormat.put("UnsignedMSB8", "NS");
    
	data_type2intFormat.put("ComplexLSB16","LOW"); 	
	data_type2intFormat.put("ComplexLSB8","LOW"); 
	data_type2intFormat.put("ComplexMSB16", "HIGH");
	data_type2intFormat.put("ComplexMSB8", "HIGH");
	data_type2intFormat.put("IEEE754LSBDouble", "LOW");
    data_type2intFormat.put("IEEE754LSBSingle", "LOW");
    data_type2intFormat.put("IEEE754MSBDouble", "HIGH"); 
    data_type2intFormat.put("IEEE754MSBSingle", "HIGH"); 
    data_type2intFormat.put("SignedBitString", "HIGH");
    data_type2intFormat.put("SignedByte", "LOW");
    data_type2intFormat.put("SignedLSB2", "LOW");
    data_type2intFormat.put("SignedLSB4", "LOW");
    data_type2intFormat.put("SignedLSB8", "LOW");
    data_type2intFormat.put("SignedMSB2", "HIGH");
    data_type2intFormat.put("SignedMSB4", "HIGH"); 
    data_type2intFormat.put("SignedMSB8", "HIGH"); 
    data_type2intFormat.put("UnsignedBitString","HIGH"); 
    data_type2intFormat.put("UnsignedByte", "HIGH");
    data_type2intFormat.put("UnsignedLSB2", "LOW");
    data_type2intFormat.put("UnsignedLSB4", "LOW");
    data_type2intFormat.put("UnsignedLSB8", "LOW");
    data_type2intFormat.put("UnsignedMSB2", "HIGH");
    data_type2intFormat.put("UnsignedMSB4", "HIGH");
    data_type2intFormat.put("UnsignedMSB8", "HIGH");
    
    
    data_type2unsignedFlag.put("ComplexLSB16", new Boolean("false")); 	
	data_type2unsignedFlag.put("ComplexLSB8", new Boolean("false")); 
	data_type2unsignedFlag.put("ComplexMSB16", new Boolean("false"));
	data_type2unsignedFlag.put("ComplexMSB8", new Boolean("false"));
	data_type2unsignedFlag.put("IEEE754LSBDouble", new Boolean("false"));
    data_type2unsignedFlag.put("IEEE754LSBSingle", new Boolean("false"));
    data_type2unsignedFlag.put("IEEE754MSBDouble", new Boolean("false")); 
    data_type2unsignedFlag.put("IEEE754MSBSingle", new Boolean("false")); 
    data_type2unsignedFlag.put("SignedBitString", new Boolean("false"));
    data_type2unsignedFlag.put("SignedByte", new Boolean("false"));
    data_type2unsignedFlag.put("SignedLSB2", new Boolean("false"));
    data_type2unsignedFlag.put("SignedLSB4", new Boolean("false"));
    data_type2unsignedFlag.put("SignedLSB8", new Boolean("false"));
    data_type2unsignedFlag.put("SignedMSB2", new Boolean("false"));
    data_type2unsignedFlag.put("SignedMSB4", new Boolean("false")); 
    data_type2unsignedFlag.put("SignedMSB8", new Boolean("false")); 
    data_type2unsignedFlag.put("UnsignedBitString", new Boolean("true")); 
    data_type2unsignedFlag.put("UnsignedByte", new Boolean("true"));
    data_type2unsignedFlag.put("UnsignedLSB2", new Boolean("true"));
    data_type2unsignedFlag.put("UnsignedLSB4", new Boolean("true"));
    data_type2unsignedFlag.put("UnsignedLSB8", new Boolean("true"));
    data_type2unsignedFlag.put("UnsignedMSB2", new Boolean("true"));
    data_type2unsignedFlag.put("UnsignedMSB4", new Boolean("true"));
    data_type2unsignedFlag.put("UnsignedMSB8", new Boolean("true"));
		

}
    
    
 /***********************************************************************
 * Property <code>Recsize</code>: The size in bytes of each record in the
 * VICAR file.  It may be calculated with the formula NBB + N1 * pixel_size,
 * where pixel_size is the size of each pixel computed using FORMAT (for the
 * pixel type) and the INTFMT or REALFMT (for the host representation) labels.
 */
    public int getRecsize() {		return _recsize; }
    public void setRecsize(int i) {	_recsize= i;  _bufsiz = _recsize; }
    
 /** This routine recalculates the recsize based on the other values */
    public void calcRecsize() {
	_recsize = getNBB() + getN1() * getPixelSize();
	
	// BB and _line_prefix_bytes are the same thing,only add them once	
	// _recsize += _line_prefix_bytes ; // add in line prefix bytes // may want this only for PDS and not PDS4
	
	_recsize += _line_suffix_bytes ; // add in line suffix bytes
	// PDS4 uses _lsb, _ssb, _bsb // they are added in PDS4InputFile.calcFilePos
	
	// this may all depend on organization where stuff lives
	// need a switch on "BSQ" "BIL" "BIP"
	// _recsize += _ssb ; // add in line suffix bytes
	_bufsiz = _recsize;
	
	
	// add in prefix and suffix bytes to calculate this value
    }
    
 /***********************************************************************
 * Pixel size is a completely synthetic (get-only) property.  It is affected
 * by <code>Format</code>, <code>IntFmt</code>, and <code>RealFmt</code>
 * properties, and is updated whenever one of those changes.
 */
    public int getPixelSize() {		return _pixel_size; }
    protected void calcPixelSize()
    {
	_pixel_size = VicarDataFormat.getPixelSize(_format, _intfmt, _realfmt);
	// _pixel_size += _ssb; // add in bytes of suffix per sample
	// sample suffix actually is per line
	// we should use the IsisDataFormat which extends VicarDataFormat
	// _pixel_size = VicarDataFormat.getPixelSize(_format, _intfmt, _realfmt);
	
	calcRecsize();
	// should pixel size include suffix??
    }
    
    
    //-------- bean getters and setters
    
    /* this is calculated from all of the labels */
    public int getHeadersSize() {
    	int totalHeaderSize = 0;
    	int x;
    	// look at all of the headers.
    	// the last header will be the largest offset_bytes
    	for (int i = 0 ; i< _header_ct ; i++) {
    		x = _header_offset_bytes[i] +  _header_object_length_bytes[i];
    		if (x > totalHeaderSize) {
    			totalHeaderSize = x;    			
    		}
    	}
    	// add in value of _offset_bytes??
    	// or do we use it instead??
    	// return totalHeaderSize;
    	// these 2 values should be the same
    	
    	return _offset_bytes;
    }
    
    // _object_length_bytes
    public void setHeader_object_length_bytes(int[] object_length_bytes) {  // offset_bytes. should be set 
    	_header_object_length_bytes = object_length_bytes;
    }
    public int[] getHeader_object_length_bytes() {  
    	return _header_offset_bytes ;
    }
    public int getHeader_object_length_bytes(int i) {  
    	if (_header_ct > i) {
    		return _header_object_length_bytes[i] ;
    	} else {
    		return 0;
    	}
    }
    
    public int getAxes() {
    	int axes = 3;
    	if (this.getNB() == 1) {
    		axes = 2;
    	}
    	return axes;
    }
    
    public void setHeader_offset_bytes(int[] offset_bytes) {  
    	_header_offset_bytes = offset_bytes;
    }
    
    public int[] getHeader_offset_bytes() {  
    	return _header_offset_bytes ;
    }
   
    public int getHeader_offset_bytes(int i) {  
    	if (_header_ct > i) {
    		return _header_offset_bytes[i] ;
    	} else {
    		return 0;
    	}
    }
    
    public void setHeader_ct(int header_ct) {   
    	_header_ct = header_ct;
    }
    public int getHeader_ct() {  
    	return _header_ct ;
    }
	
    // Image offset, NOT Header 
    public void setOffset_bytes(int offset_bytes) {  
    	_offset_bytes = offset_bytes;
    }
    public int getOffset_bytes() {  
    	return _offset_bytes ;
    }
    
 // Image File,bytes (size)
    public void setFilesize_bytes(int file_bytes) {  
    	_filesize_bytes = file_bytes;
    }
    public int getFilesize_bytes() {  
    	return _filesize_bytes ;
    }
    
 // Image File,bytes (size)
    public void setFile_records(int file_records) {  
    	_file_records = file_records;
    }
    public int getFile_records() {  
    	// use 
    	return _file_records ;
    }
    
    public void setParsing_standard_id(String[] parsing_standard_id) {  // "PDS3","VICAR", "RAW" ?? "ODL3", "FITS" ??
    	_parsing_standard_id = parsing_standard_id;
    }
    public String[] getParsing_standard_id() {  // "PDS3","VICAR", "RAW" ?? "ODL3", "FITS" ??
    	return _parsing_standard_id;
    }
    
    public String getParsing_standard_id(int i) {  // "PDS3","VICAR", "RAW" ?? "ODL3", "FITS" ??
    	if (_header_ct > i) {
    		return _parsing_standard_id[i];
    	} else {
    		return "RAW";
    	}
    		
    }
    
    public void setChecksum(String c) {  // detached file name
    	_checksum = c;
    }
    public String getChecksum() {  // detached file name
        return _checksum;
    }
    
    public void setDetachedFilename(String fn) {  // detached file name
    	_detachedFilename = fn;
    	_detachedFile = true;
    }
    public String getDetachedFilename() {  // detached file name
        return _detachedFilename;
    }
    
    public void setDetachedFile(boolean b) {  // detached file name
    	_detachedFile = b; 	
    }
    public boolean getDetachedFile() {  
        return _detachedFile;
    }
    public boolean isDetachedLabel() { 
        return _detachedFile;
    }
    
    public void setData_type(String dt) { 
    	_data_type = dt;
    	
    	if (debug) {
    		System.out.println("PDS4SystemLabel setData_type "+_data_type);
    	}
    	// set all the other values derived from this value.
    	String format = data_type2format.get(_data_type);
    	// if format == null
    	this.setFormat(format);
    	
		Boolean _unsignedFlag = data_type2unsignedFlag.get(_data_type);    		
		String intFormat =  data_type2intFormat.get(_data_type);
		this.setIntFmt(intFormat);
		String realFormat = data_type2realFormat.get(_data_type);
		this.setRealFmt(realFormat);
    	
    }
    public String getData_type() {  // detached file name
        return _data_type;
    }
    
    
    
    public String getDateNowZ() {
    	// <start_date_time>1997-07-07T23:48:33.442Z</start_date_time>
    	// <creation_date_time>2009-05-04T13:46:30.1158Z</creation_date_time>
    	
    	Date dNow = new Date( );
        SimpleDateFormat ft = 
        new SimpleDateFormat ("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'");
       
        return ft.format(dNow);
    }
    
    public void setLinePrefixBytes(int lpb) {  // line prefix PDS only
        _line_prefix_bytes = lpb;
    }
    public int getLinePrefixBytes() {  // line prefix PDS only
        return _line_prefix_bytes;
    }
    
    public void setLineSuffixBytes(int lsb) {  // line prefix PDS only
        _line_suffix_bytes = lsb;
    }
    public int getLineSuffixBytes() {  // line prefix PDS only
        return _line_suffix_bytes;
    }
    
    public void setBSB(int bsb) {  // band suffix
        _bsb = bsb;
    }
    public int getBSB() {
        return _bsb;
    }
    
    public void setLSB(int lsb) {  // line suffix
        _lsb = lsb;
        calcRecsize() ; // forces recalc using this value
    }
    public int getLSB() {
        return _lsb;
    }
    
    public void setLPB(int lpb) {  // line prefix
        _lpb = lpb;
        calcRecsize() ; // forces recalc using this value
    }
    public int getLPB() {
        return _lpb;
    }
    
    public void setSSB(int ssb) {  // sample suffix
        _ssb = ssb;
        calcPixelSize() ; // forces recalc using this value
    }
    public int getSSB() {
        return _ssb;
    }
    
    // double _core_base = 0.0; 
    public void setCore_base(double core_base) {
        _core_base = core_base;
    }
    public double getCore_base() {
        return _core_base;
    }
    
    // double _core_multiplier = 1.0;
    public void setCore_multiplier(double core_multiplier) {
        _core_multiplier = core_multiplier;
    }
    public double getCore_multiplier() {
        return _core_multiplier;
    }
    
    // boolean _returnFloat = true; 
    public void setReturnFloat(boolean b) {
        _returnFloat = b;
    }
    public boolean getReturnFloat() {
        return _returnFloat;
    }
     
    
    // boolean isRGB = false; 
    public void setIsRGB(boolean b) {
        _isRGB = b;
    }
    public boolean getIsRGB() {
        return _isRGB;
    }
    
    public boolean isRGB() {
        return _isRGB;
    }
    
    // int bandsToUse[] = {0,1,2};
    // make sure the right color model is in use
    public void setBandsToUse(int[] bands) {
        if (bands.length == 1) {
            _bandsToUse = new int[1];
            _bandsToUse[0] = bands[0];
        }
        else if (bands.length == 3) {
            // check or set _isRGB ???
            for (int i=0 ; i<3 ; i++ ) {
                _bandsToUse[i] = bands[i];
                }
        }
    }
    public int[] getBandsToUse() {
        return _bandsToUse;
    }
}