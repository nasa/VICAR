package jpl.mipl.io.vicar;

import java.util.*;
import java.beans.*;
import java.lang.reflect.*;

/**
 * This class is a bean that maintains all VICAR system label information.
 * see SystemLabel.
 * This class adds some ISIS specific items which are not a part of
 * a vicar label. The SystemLabel is used by VicarIO to control the reading of 
 * iamage data from the file and transfering it to the raster
 * Support for the following ISIS items is added:
 * suffix pixels
 * <BR>selection of bands, since a cube may hold lots (>3)
 * <BR>core_base
 * <BR>core_multiplier
 * <BR>
 * Not all of the ISIS system label features are implemented yet.
 * special pixels are NOT supported yet
 */

public class IsisSystemLabel extends SystemLabel {
    
    // ISIS specific items, these are added to the ones in SystemLabel
    // all suffix items are 4 bytes, the suffix bytes are the number of bytes of suffix
    // if there were 2 suffix items that would be 8 bytes of suffix
    int _bsb = 0; // band suffix
    int _lsb = 0; // line suffix
    int _ssb = 0; // sample suffix
    
    int _lpb = 0; // line prefix bytes - added for PDS, ISIS has no prefix ?!?
    // no need identified for band or sample prefix variables
    
    int _line_prefix_bytes = 0; // PDS only
    // this is the same as Vicar nbb
    int _line_suffix_bytes = 0; // PDS only
    
    /* we could add another item to indicate how many bytes of the suffix are used
    * for the case where we want to use the suffix as data.
    * data type core base and multiplier are also availale for suffixs
    * the above numbers are used so we know how many bytes to 
    * skip/ignore while reading in the image data
    */
    
    // all ISIS data items are real (float) values even if they are stored as 
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
    
    
/***********************************************************************
 * Create an empty (default) SystemLabel.
 */
    public IsisSystemLabel()
    {
	    super();
	    // itilialize the extra items for ISIS 
	    setIsisDefaults();
    }
    
    
    /***********************************************************************
 * Initialize the only the ISIS specific label items to the default values.
 */
    public void setIsisDefaults()
    {
	int _bsb = 0; // band suffix
    int _lsb = 0; // line suffix
    int _ssb = 0; // sample suffix
    
    int _line_prefix_bytes = 0; // PDS only
    int _line_suffix_bytes = 0; // PDS only
    
    double _core_base = 0.0; 
    double _core_multiplier = 1.0;
    
    boolean _returnFloat = false; // true; 
    
    boolean isRGB = false; 
    int bandsToUse[] = {0,1,2};

    // redo these if the ISIS values force us to override these methods
	// calcPixelSize();
	// calcRecsize();
    }
    
    /** should overide this so ISIS specific items are added
    public String toString()
    {
    }
    **/
    
    
    
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
	// _recsize += _line_prefix_bytes ; // add in line prefix bytes // may want this only for PDS and not ISIS
	
	_recsize += _line_suffix_bytes ; // add in line suffix bytes
	// ISIS uses _lsb, _ssb, _bsb // they are added in ISISInputFile.calcFilePos
	
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