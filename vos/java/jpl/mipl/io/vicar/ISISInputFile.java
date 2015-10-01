/*
 * @(#)ISISInputFile.java	
 * @version 1.01 6-3-2002 JAI codec version
 *
 * @author Steve Levoe NASA/JPL
 * 8-22-2002 jdk1.4 ImageIO and codec combined version
 *
 */
 


package jpl.mipl.io.vicar;



import java.io.*;
import com.sun.media.jai.codec.*;
import java.lang.reflect.*;
import java.beans.*;
import java.awt.image.*;
import javax.media.jai.*;
import java.util.Vector;
import java.util.Hashtable;
import java.util.*;

import jpl.mipl.io.codec.*;
import jpl.mipl.io.vicar.*;
import jpl.mipl.io.plugins.*;
import jpl.mipl.io.util.DOMutils;

// perl utilities for parsing
// import org.apache.oro.text.perl.*;

// import jpl.mipl.io.plugins.vicar.*;

/*$$$$ Enable for IIO */
// 9-28-01 commented out
// import javax.media.imageio.stream.*; // EA1
import javax.imageio.stream.*; 
/*$$$$*/

// import com.sun.media.jai.codec.SeekableStream;
// import com.sun.media.jai.codec.ImageCodec;

import org.w3c.dom.*;
import org.xml.sax.SAXException;

/** 
 * This class manages a single ISIS input image file.
 * <p>
 * All accesses to the ISIS file are thread-safe, assuming that nobody else
 * tries to access the underlying stream directly.  Thus, multiple threads
 * can issue simultaneous <code>readRecord()</code> or <code>readTile()</code>
 * requests, although each request is handled one at a time via synchronization
 * on the <code>ISISInputFile</code> object.  However, if you have a
 * sequential-only stream, all accesses must still be strictly sequential...
 * meaning that the use of multiple threads with sequential streams will not
 * work (the request order would be non-deterministic).  For random-hard
 * streams, threads will work but could cause performance hits depending on
 * the ordering of the requests.  Random-easy streams should be fine.
 * <p>
 * This reader is capable of reading embedded Vicar labels. If one is encountered 
 * a VicarLabel Object will be created and filled. This Object will be placed
 * in properties as "vicar_label" so other programs may access the information
 * contained in the label.
 * <br>
 * @see VicarInputImage
 * @see VicarInput
 */


public class ISISInputFile extends VicarInputFile
{
    // VicarInputFile implements VicarInput

    // protected VicarLabel _label;
    public VicarLabel _embeddedVicarLabel;
    public boolean _hasEmbeddedVicarLabel = false;
    int _embedded_label_start = -1;
    
    // VicarPdsIsisImageDecodeParam _imageDecodeParam = null;
    // this should be in VicarInputFile
    ImageDecodeParam _imageDecodeParam = null;
    
    Document _ISIS_document; // this is the holder of the ISIS metadata
    Document _Vicar_document; // this is the holder of the Vicar metadata
    // ISISMetadata pdsMetadata = null;
    boolean gotMetadata = false;
    
    IsisSystemLabel _isisSystem = null; // ISIS and ISIS have the same extra stuff
    
    // for testing
    private int _readMin, _readMax, _readCt;
    
    boolean debug = false;
    // boolean debug = true;
    
    int _lsb = 0;
    int _ssb = 0;
    int _bsb = 0;
    
    int _lpb = 0;
    
   
////////////////////////////////////////////////////////////////////////

/***********************************************************************
 * Dummy constructor (for now). Need to add good ones that call open().
 */
    public ISISInputFile()
    {
	super();
	if (debug) System.out.println("%%%%%%% ISISInputFile constructor $$$$$$$$$$$$$$$");
    }

/***********************************************************************
 * Dummy constructor (for now). Need to add good ones that call open().
 */
    // public ISISInputFile(VicarPdsIsisImageDecodeParam imageDecodeParam)
    public ISISInputFile(ImageDecodeParam imageDecodeParam)
    {
	super();
	_imageDecodeParam = imageDecodeParam;
	// this will later be passed on to the super. 
	// Not now because other changes to that class will be implemented at the same time
	// super(imageDecodeParam) ; 
	// VicarPdsIsisImageDecodeParam _imageDecodeParam = null;
    // ImageDecodeParam imageDecodeParam
	
	// if (debug) System.out.println("%%%%%%% ISISInputFile constructor $$$$$ with VicarPdsIsisImageDecodeParam");
	if (debug) System.out.println("%%%%%%% ISISInputFile constructor $$$$$ with ImageDecodeParam");
    }


/***********************************************************************
 * Constructor that calls <code>open(String)</code>.
 *
 * @param fn name of the file to open
 * @throws IOException
 */
    public ISISInputFile(String fn) throws IOException
    {
	this();
	open(fn);
	
    
    }
    
	public void setDebug(boolean d) {
		debug = d;
	}



/***********************************************************************
 * Does the actual work of opening the file.  Reads in the first part
 * of the label, and sets up the SystemLabel object.
 *
 * @throws IOException
 */
/**
    protected void openInternal() throws IOException
    {
	// Make sure we're at the beginning of the file/stream.
	// Only does anything if random-access.
	seekToLocation(0);

	_lblsize_front = 0;
	_lblsize_eol = 0;
*********/
/*
 * 
 * openInternal() has been eliminated.
 * setupLabels() does the ISIS specific part and is called from openInternal()
 * in VicarInputFile
 */
 
	protected void setupLabels() {
	// need to decide where DOMutils will live package wise
    // ImageIO is the issue
    // jpl.mipl.util.DOMutils domUtils = new jpl.mipl.util.DOMutils();
    DOMutils domUtils = new DOMutils();
	// BufferedReader input = null;
	// _input_stream
	
	if (debug) {
	    System.out.println("ISISInputFile.setupLabels()"); 
	    System.out.println("input type: "+_input_stream); 
	}
	
     if (_input_stream instanceof ImageInputStream) {

        if (debug) System.out.println("calling ISISLabelToDOM  instanceof ImageInputStream"); 
        	// ISISLabelToDOM pdsLabel2Dom = new ISISLabelToDOM((DataInput) _input_stream, null); 
            ISISLabelToDOM isisLabel2Dom = new ISISLabelToDOM((ImageInputStream) _input_stream, null);
            if (debug) System.out.println("after calling ISISLabelToDOM"); 
            _ISIS_document = isisLabel2Dom.getDocument();
            if (debug) {
            	System.out.println("_ISIS_document: "+_ISIS_document); 
            	domUtils.serializeDocument( _ISIS_document, "ISIS_LABEL.xml", "xml");
            
            	int x = 2;
            	if (x == 1) return;
            }
            
            gotMetadata = true;
            // create a SystemLabel from the values in the document
	        _system = createSystemLabel(_ISIS_document ) ;
	        _isisSystem = (IsisSystemLabel) _system;
	        
	        
	        /***
	        // put things into the properties of the image
	        setProperty("ISIS_document", (Object) _ISIS_document);
	        setProperty("ImageDecodeParam", (Object) imageDecodeParam);
	        setProperty("ImageDecodeParam_ClassName", (Object) imageDecodeParam.getClass().getName() );
	        setProperty("ImageFormatName", (Object) "ISIS");
	        setProperty("SystemLabel", (Object) _system);
	        setProperty("SystemLabel_ClassName", (Object) _system.getClass().getName() );
	        ***/
	        // getLabelSize assumes a ISIS compliant label
	        // hopefully ISIS counts the embedded Vicar label in its values for label size
	        // _lblsize_front = getLabelsize(_ISIS_document ) ;
	        _lblsize_front = getImageStart(_ISIS_document ) ;
            // return ;
       } else if (_input_stream instanceof DataInput) {
        // System.out.println("calling ISISLabelToDOM"); 
        	ISISLabelToDOM isisLabel2Dom = new ISISLabelToDOM((DataInput) _input_stream, null);
        	// BufferedReader br = new BufferedReader(new InputStreamReader((DataInput) _input_stream));
            // ISISLabelToDOM pdsLabel2Dom = new ISISLabelToDOM(br, null);
            
            
            // System.out.println("after calling ISISLabelToDOM"); 
            _ISIS_document = isisLabel2Dom.getDocument();
            if (debug) System.out.println("_ISIS_document: "+_ISIS_document); 
            // pdsMetadata is only for the ImageIO version
            // but the JAI codec will put the document into the properties
            // pdsMetadata = new ISISMetadata(_document);
            // serialize to see the XML. View in Internet Explorer
            domUtils.serializeDocument( _ISIS_document, "ISIS_LABEL.xml", "xml");
            
            int x = 1;
            if (x == 1) return;
            
            // should check for "dualie" ISIS AND Vicar label
            // if it is a "dualie" get the vicarLabel AND _Vicar_document
            gotMetadata = true;
            // create a SystemLabel from the values in the document
	        _system = createSystemLabel(_ISIS_document ) ;
	        _isisSystem = (IsisSystemLabel) _system;
	        
	        
	        /***
	        // put things into the properties of the image
	        setProperty("ISIS_document", (Object) _ISIS_document);
	        setProperty("ImageDecodeParam", (Object) imageDecodeParam);
	        setProperty("ImageDecodeParam_ClassName", (Object) imageDecodeParam.getClass().getName() );
	        setProperty("ImageFormatName", (Object) "ISIS");
	        setProperty("SystemLabel", (Object) _system);
	        setProperty("SystemLabel_ClassName", (Object) _system.getClass().getName() );
	        ***/
	        // getLabelSize assumes a ISIS compliant label
	        // hopefully ISIS counts the embedded Vicar label in its values for label size
	        // _lblsize_front = getLabelsize(_ISIS_document ) ;
	        _lblsize_front = getImageStart(_ISIS_document ) ;
            // return ;
       } else {
            System.out.println("Improper input type: "+_input_stream); 
            System.out.println("can't read the header ");
            return;
       }
	
	
    if (debug) {
    
    	System.out.println("  _lblsize_front "+_lblsize_front );
    
    }
    
    
	// Set up defaults for missing label items.
	// Note that the host format defaults for input files are VAX, because
	// that was the only kind of file in existence before the host type
	// labels were added.  Output files default to Java.

	if (!_system.isHostValid()) {
	    _system.setHost("VAX-VMS");
	}
	if (!_system.isIntFmtValid()) {
	    // _system.setIntFmt("LOW");
	}
	if (!_system.isRealFmtValid()) {
	    // _system.setRealFmt("VAX");
	}
	_image_size_bytes = (_system.getNLB() +
					(_system.getN2() * _system.getN3()))
			* _system.getRecsize();

	_bsb = _isisSystem.getBSB();
	_lsb = _isisSystem.getLSB();
	_ssb = _isisSystem.getSSB();
	
	System.out.println("  ***** _bsb= "+_bsb+"  _lsb="+_lsb+"  _ssb="+_ssb );

    // _lblsize_front MUST take into account the embedded Vicar label
    // it really is the start of the image data
	_current_file_pos = _lblsize_front;
	
	}
	
	protected void openInternalLast() {
	
	// provided for subclasses to put good stuff here
	// this is needed to correctly read some datasets
	/**
	System.out.println("openInternalLast *************************************");
	System.out.println("_line_prefix_bytes "+_line_prefix_bytes);
	System.out.println("_line_suffix_bytes "+_line_suffix_bytes);
	
	_input_stream_wrap.setLinePrefixBytes(_line_prefix_bytes);
	_input_stream_wrap.setLineSuffixBytes(_line_suffix_bytes);
	**/
}
	

    /******************************************************
    * 
    * @param Properties Object
    * intended to be called with the properties of the RenderedImage so 
    * some usefull things are
    * put in the images properies. The Application which receives this 
    * image can use these properies. This doesn't seem to work as expected.
    * Some Objects seem to go out os scope and become null.
    * PropertyGenerator is the probable answer.
    *************/
    public void setToProperties(Hashtable properties) {
        
        if (debug) System.out.println("ISISInputFile.setToProperties $$$$$$$$$$$$$$$$$$$$$$");
        
        if (properties != null) {
            String n = "ISIS";
            if (debug) System.out.println("put ImageFormatName "+n);
            // properties.put("ImageFormatName", (Object) "n);
            
            // System.out.println("properties: ");
            // System.out.println(properties.toString());
            
         if (_hasEmbeddedVicarLabel) {
            // vif.getHasEmbeddedVicarLabel()
            // putting the label into the images properties allows us to gain access to the
            // image label thru the properies
   
            properties.put("vicar_label", (Object)_embeddedVicarLabel);
         }
        
	     // put things into the properties of the image
	     if (_ISIS_document != null) {
	        properties.put("ISIS_document", (Object) _ISIS_document);
	     }
	     if (_imageDecodeParam != null) {
	       properties.put("ImageDecodeParam", (Object) _imageDecodeParam);
	       properties.put("ImageDecodeParam_ClassName", (Object) _imageDecodeParam.getClass().getName() );
	     }
	     
	     properties.put("SystemLabel", (Object) _system);
	     properties.put("SystemLabel_ClassName", (Object) _system.getClass().getName() );
	     
	     if (debug) System.out.println("properties.size() "+ properties.size());
	    }
	    else {
	       if (debug) System.out.println("properties is NULL");
	    }
	        
        
    }

    /**************************************************************
    * Create a SystemLabel from the contents of the Document
    *
    * @param Document this is specific to a Document filled from a ISIS image label
    * 
    *****/
    public SystemLabel  createSystemLabel(Document doc) {
    
    	if (debug) {
    		System.out.println("ISISInputFile.createSystemLabel");
    	}
        DOMutils domUtils = new DOMutils();
        // avoid grabbing the IIO enabled version
        // DOMutils domUtils = new DOMutils();
        
        // use an ISIS system label since it has some extra elemnts specific
        // to ISIS and ISIS images
        IsisSystemLabel sys = new IsisSystemLabel();
    
        String format = "BYTE"; // BYTE HALF FULL REAL DOUB COMP
        String org = "BSQ"; // BSQ BIL BIP
        int nl = 0;
        int ns = 0;
        int nb = 1;
        int bits = 8; // Per sample
       
        String core_item_type = "";
        String host = "JAVA";
        // String intFormat = "LOW";
        String intFormat = "HIGH";
		String realFormat = "VAX"; // this is the default
        boolean unsignedFlag = true;
        
        // PDS ISIS specific items
        int axes = 3; 
        int suffix_bytes = 4;
        double core_base = 0.0;
        double core_multiplier = 1.0;
        String core_valid_minimum = "";
        int bandsToUse[] = {0,1,2};
        boolean isRGB = false;
        int bsb = 0; // band suffix bytes
        int lsb = 0; // line suffix bytes
        int ssb = 0; // sample suffix bytes
        int suffix_items[] = {0,0,0};
        
        // get stuff out of the Document
        Node root = doc.getDocumentElement();
        Node result, node;
        NodeList nlist;
        String nodeValue;
        
        // first verify that this is the correct Document type
        
        // ISISMetadata.nativeMetadataFormatName
        
        // result = XPathAPI.selectSingleNode(root, ISISMetadata.nativeMetadataFormatName);
        // String xPath = "//"+ISISMetadata.nativeMetadataFormatName ;
        // remove depnedancy on ImageIO
        String nativeMetadataFormatName = "ISIS_LABEL";
        // 
        String xPath = "//"+nativeMetadataFormatName ;
        if (debug) System.out.println("ISISInputFile.createSystemLabel() " +xPath);
       
        result = domUtils.getResultNode(root,xPath);
        // String nodeValue = getItemValue(root, xPath);
        // get the needed data from the result node
        if (debug) System.out.println("result) " +result);
        
        if (result == null) {
            if (debug) System.out.println("ISISInputFile.createSystemLabel() incompatable Document");
            return (SystemLabel) null;
        }
    
		
            if (debug) System.out.println("createSystemLabel LOOKING for QUBE ****************");
            node = domUtils.getSingleNode(_ISIS_document,"//OBJECT[@name='QUBE']");    
            if (node != null) { // ^QUBE
                // serialize the Node returned to see if it's what was expected
                if (debug) {
                	System.out.println("createSystemLabel for QUBE");
                	domUtils.serializeNode(node, "QUBE.xml", "xml");
                	System.out.println("serialized to QUBE.xml");
                }
                
            
                xPath = "//item[@key='AXES']" ;
                nodeValue = domUtils.getItemValue(node, xPath);
                axes = Integer.parseInt(nodeValue);
                // can axes be other than 3 ??? ignore for now
                
                xPath = "//item[@key='AXIS_NAME']" ; // also check for AXES_NAME ???
                nodeValue = domUtils.getItemValue(node, xPath);
                // convert ISIS AXIS_NAME to vicar
                if (nodeValue != null ) {
                	if (debug) {
                		System.out.println("AXIS_NAME "+nodeValue);
                	}
                    if (nodeValue.equalsIgnoreCase("(SAMPLE,LINE,BAND)") )  {
                        org = "BSQ";
                    }
                    else if (nodeValue.equalsIgnoreCase("(SAMPLE,BAND,LINE)") )  {
                        org = "BIL";
                        // org = "BIP";
                    }
                    else if (nodeValue.equalsIgnoreCase("(BAND,SAMPLE,LINE)") )  {
                        org = "BIP";
                    }
                }
                String[] axes_names = getItemStringArray ( nodeValue );
                
                
                xPath = "//item[@key='CORE_ITEMS']" ;
                nodeValue = domUtils.getItemValue(node, xPath);
                int[] core_items = getItemIntArray ( nodeValue );
                
                xPath = "//item[@key='CORE_ITEM_BYTES']" ;
                nodeValue = domUtils.getItemValue(node, xPath);
                bits = Integer.parseInt(nodeValue);
                                
                xPath = "//item[@key='CORE_ITEM_TYPE']" ;
                core_item_type = domUtils.getItemValue(node, xPath);
                // check for null String !!!
                // convert isis/pds CORE_ITEM_TYPE to vicar
                
				if (core_item_type.equalsIgnoreCase("IEEE_REAL") && bits == 32) {
							format = "REAL" ; 
							intFormat = "HIGH";  // "HIGH"
							realFormat = "IEEE";
							unsignedFlag = false;
							if (debug) { System.out.println("REAL"); }
						}
				if (core_item_type.equalsIgnoreCase("PC_REAL") && bits == 32) {
											format = "REAL" ; 
											intFormat = "LOW";  // "HIGH"
											realFormat = "RIEEE"; // "IEEE";
											unsignedFlag = false;
											if (debug) { System.out.println("PC_REAL RIEEE REAL"); }
										}
						
                if (core_item_type.equalsIgnoreCase("MSB_UNSIGNED_INTEGER") || 
                    core_item_type.equalsIgnoreCase("UNSIGNED_INTEGER") ||
                    core_item_type.equalsIgnoreCase("PC_UNSIGNED_INTEGER"))  {
                    intFormat = "HIGH";  // "HIGH"
                    
                    unsignedFlag = true;
                    }
                  else { // SUN_UNSIGNED_INTEGER
                    intFormat = "LOW";  // "HIGH"
                    unsignedFlag = false;
                    }
                
                
                xPath = "//item[@key='CORE_VALID_MINIMUM']" ;
                nodeValue = domUtils.getItemValue(node, xPath);
                if (debug) 
                 System.out.println("CORE_VALID_MINIMUM "+nodeValue);
                core_valid_minimum = nodeValue; // store as a String, figure out how to use the value later
                // its a bit count and hex mask 16#FF7FFFFA# or something sinmilar
                
                xPath = "//item[@key='CORE_BASE']" ;
                nodeValue = domUtils.getItemValue(node, xPath);
                core_base = Double.parseDouble(nodeValue);
                
                xPath = "//item[@key='CORE_MULTIPLIER']" ;
                nodeValue = domUtils.getItemValue(node, xPath);
                core_multiplier = Double.parseDouble(nodeValue);
                
                xPath = "//item[@key='SUFFIX_BYTES']" ;
                nodeValue = domUtils.getItemValue(node, xPath);
                suffix_bytes = Integer.parseInt(nodeValue);
                
                xPath = "//item[@key='SUFFIX_ITEMS']" ;
                nodeValue = domUtils.getItemValue(node, xPath);
                suffix_items = getItemIntArray ( nodeValue );
                /***
                String[] getItemStringArray ( String array )
                int[] getItemIntArray ( String array )
                double[] getItemDoubleArray ( String array )
                ****/
                // get an array of values from the returned string
                
                //  Determine the storage order from the AXIS_NAME and CORE_ITEMS values.
                //  The USGS documentation states that there are really only three storage
                //  orders (though 6 are possible):
                //      (SAMPLE, LINE, BAND) - Band Sequential (BSQ)
                //      (SAMPLE, BAND, LINE) - Band Interleaved by Line (BIL)
                //      (BAND, SAMPLE, LINE) - Band Interleaved by Pixel (BIP)
                //  and the words: SAMPLE, LINE, BAND will be used exactly
                //  
                if (debug) System.out.println("########################################################");
                // axes_names core_items suffix_items
                for ( int i = 0; i < axes_names.length; i++ )  {
                    if (debug) System.out.println("axes="+axes_names[i]+"  "+core_items[i]+"   suffix="+suffix_items[i]);
                    if ( axes_names[i].indexOf ( "SAMP" ) >= 0 )  {   //  if value at axes[i] resembles "SAMP"
                        if ( i == 1 )  org = "BIP";             //  according to the above, this is unique enough
                        ns = core_items[i];
                        ssb = suffix_items[i] * suffix_bytes;
                        
                    }  else if ( axes_names[i].indexOf ( "BAND" ) >= 0 )  {
                        if ( i == 1 )  org = "BIL";             //  according to the above, this is unique enough
                        nb = core_items[i];
                        bsb = suffix_items[i] * suffix_bytes;
                        
                    }  else if ( axes_names[i].indexOf ( "LINE" ) >= 0 )  {
                        if ( i == 1 )  org = "BSQ";             //  according to the above, this is unique enough
                        nl = core_items[i];
                        lsb = suffix_items[i] * suffix_bytes;
                        
                    }  else  {
                        //      ???  Better not get here
                    } 
                }
        
                
                // if (debug) System.out.println("ssb "+ssb+"   bsb "+bsb+"   lsb "+lsb);
                // ssb = 0;
                if (debug) { 
                	System.out.println("ssb "+ssb+"   bsb "+bsb+"   lsb "+lsb);
                	System.out.println("bytes per pixel "+bits+"   CORE__ITEM_TYPE "+core_item_type);
                	System.out.println("format "+format);
                }
                
                switch (bits ) { // actually bytes per pixel
            
                    case 1 :
                        format = "BYTE";
                    break;
                    case 2:
                    // check CORE_VALID_MINIMUM = -32752 to see if this is signed or unsigned
                        if (core_item_type.indexOf("UNSIGNED") == -1) {
                            format = "HALF"; // HALF is signed
                        }
                        else {
                            format = "USHORT" ;
                        }
                        // which does ISIS support ???
                        if (core_item_type.indexOf("SUN_") == -1) {
                        	intFormat = "LOW";
                        	host = "SUN";
                        }
                        else {
                        	intFormat = "HIGH";
                        }
                    break;
                    case 4:
                    	if (core_item_type.indexOf("REAL") == -1) {
                            format = "INT"; // HALF is signed
                        }
                        else {
                            format = "REAL" ;
                        }
                    	
                    // could be int or REAL, need more information
                    break;
                    default:
                    if (debug) System.out.println("format we don't handle "+bits+" bits yet");
                    break;
                        // add cases later for other data types
                        // need more info to determine REAL types
                    }
                
            //set ISIS/PDS specific systemLabel items
            sys.setBandsToUse(bandsToUse);
            sys.setBSB(bsb);
            sys.setLSB(lsb);
            sys.setSSB(ssb);
            sys.setCore_base(core_base);
            sys.setCore_multiplier(core_multiplier);
            
            if (nb == 3) isRGB = true;
            else isRGB = false;
            sys.setIsRGB(isRGB);
            }
            
    
    sys.setOrg(org); // BSQ BIL BIP
    
    sys.setNL(nl);
    sys.setNS(ns);
    sys.setNB(nb);
    
    // USHORT  was added to support PDS, vicar doesn't have USHORT - HALF is signed
    sys.setFormat(format); // BYTE HALF FULL REAL DOUB COMP USHORT
    
     
    sys.setHost(host) ;// JAVA is default

    // host IS ACTUALLY NOT USED .. this is the important one
    sys.setIntFmt(intFormat ) ;// LOW default , HIGH
	sys.setRealFmt(realFormat ) ;// IEEE default , RIEEE
	// sys.setRealFmtValid(true);

    /* maybees
    since PDS is now including some ISIS keywords we need to merge the ISIS and PDSsystemLabel
    to include
    CORE_BASE CORE_MULTIPLIER and SUFFIX's
    *
    setN1(int n1) fastest varying dimension
    defaults - don't need to set
    setType(String type) // IMAGE is the only one we'll use
    **/

    return sys;
    }
    
    /***************************************************************
    * convert a text item which is a list to an array of the values 
    * in the list
    * Assumes the item may have () parens around it and the list is comma seperated
    * allow for space in the items but don't copy the spaces
    ***********/
    private String[] getItemStringArray ( String array )  {
        
        array = array.substring ( array.indexOf ( "(" ) + 1, array.indexOf ( ")" ) );
        array.replace ( '"', ' ' );
        StringTokenizer st = new StringTokenizer ( array, "," );
        int i = 0;
        String[] stringArray = new String[st.countTokens()];
    
        while ( st.hasMoreTokens()  )  {
            stringArray[i] = new String ( st.nextToken().trim() );
            i++;
        }
        return stringArray;
    }
    
    
    private int[] getItemIntArray ( String array )  {
        
        if (debug) System.out.println("getItemIntArray "+array);
        int start = array.indexOf ( "(" );
        int end = array.indexOf ( ")" );
        if (debug) System.out.println(" start="+start+"  end="+end);
        if (start == -1 || end == -1) { 
            int[] intArray = new int[0];
            // return intArray;
            return null;
            }
        
        array = array.substring ( start + 1, end );
        StringTokenizer st = new StringTokenizer ( array, "," );
        int i = 0;
        int[] intArray = new int[st.countTokens()];
    
        while ( st.hasMoreTokens()  )  {
            intArray[i] = new Integer ( st.nextToken() ).intValue();
            i++;
        }
        return intArray;
    }
    
    private double[] getItemDoubleArray ( String array )  {
        
        if (debug) System.out.println("getItemIntArray "+array);
        int start = array.indexOf ( "(" );
        int end = array.indexOf ( ")" );
        if (debug) System.out.println(" start="+start+"  end="+end);
        if (start == -1 || end == -1) { 
            double[] doubleArray = new double[0];
            // return intArray;
            return null;
            }
        
        array = array.substring ( start + 1, end );
        StringTokenizer st = new StringTokenizer ( array, "," );
        int i = 0;
        double[] doubleArray = new double[st.countTokens()];
    
        while ( st.hasMoreTokens()  )  {
            doubleArray[i] = new Double ( st.nextToken() ).doubleValue();
            // float version would use
            // intArray[i] = new Double ( st.nextToken() ).floatValue();
            i++;
        }
        return doubleArray;
    }
    

    /**
    *
    * @return the Document derived from the ISIS label
    */
    public Document getISISDocument() {
            return _ISIS_document;
    }
    
    /**
    *
    * @return the Document derived from the embedded vicar label
    */
    public Document getVicarDocument() {
            return _Vicar_document;
    }

    
 
    /**
    * 
    * This is specific to a Document filled from a ISIS image label
    * calculate the size of the image's label in bytes. This is how much must 
    * be skipped to start reading image data.
    * This method use the XPathAPI to search a Document for a Node.
    * if we get back a node then we extract a value.
    **/ 
    // public int  getLabelsize(Document doc) {
    
    public int getImageStart(Document doc) {
        
    
        // jpl.mipl.util.DOMutils domUtils = new jpl.mipl.util.DOMutils();
        DOMutils domUtils = new DOMutils();
        SystemLabel sys = new SystemLabel();
        // Perl5Util perl = new Perl5Util();
    
        int imageStart = 0;
        int record_bytes = 0;
        int qube_starting_record = 0;
        
        String value = "";
        String units = "";
        
        // get stuff out of the Document
        Node root = doc.getDocumentElement();
        Node result;
        String nodeValue, nodeKey;
        NodeList nl;
        String xPath = null;
        
        // first verify that this is the correct Document type
        // This may go away, I'm not sure I want to include this in the document
        
        // ISISMetadata.nativeMetadataFormatName
        // xPath = "//"+ISISMetadata.nativeMetadataFormatName ;
        String nativeMetadataFormatName = "ISIS_LABEL"; // put this in the ISISCodec ???
        xPath = "//"+nativeMetadataFormatName ;
        // nodeValue = getItemValue(root, xPath);
        result = domUtils.getResultNode(root, xPath );
        // get the needed data from the result node
        if (result == null) {
            System.out.println("ISISInputFile.getLabelSize() incompatable Document");
            return 0;
        }
    
        
        xPath = "//item[@key='RECORD_BYTES']" ;
        nodeValue = domUtils.getItemValue(root, xPath);
        record_bytes = Integer.parseInt(nodeValue);
        if (debug) System.out.println("ISISInputFile.getLabelSize() RECORD_BYTES="+nodeValue);
        
        // ^QUBE may not be legal XML ???
        xPath = "//item[@key='^QUBE']" ;
        nodeValue = domUtils.getItemValue(root, xPath);
        qube_starting_record = Integer.parseInt(nodeValue);
        if (debug) {
        	System.out.println("ISISInputFile.getLabelSize() ^QUBE="+nodeValue);
        }
        
        imageStart = record_bytes * (qube_starting_record - 1) ;
        
    if (debug) System.out.println("ISISInputFile.getImageStart() imageStart="+imageStart);
        
    return imageStart;
    }




////////////////////////////////////////////////////////////////////////

/***********************************************************************
 * Internal routine for calculating the position to seek to given the
 * record address (n2/n3) and the offset within the record.  The binary
 * headers and prefixes are taken into account, i.e. position 0 is the first
 * pixel of the record (past the prefix), and line/band 0 is the first
 * line/band of pixel data (past the headers).
 * @throws IOException if start, n2 or n3 exceed the bounds of the image
 */
    protected long calcFilePos(int start, int n2, int n3) throws IOException
    {
        /***
        System.out.print("ISISInputFile.calcFilePos: "+_system.getOrg());
        System.out.print(" start="+start+" N1="+_system.getN1() );
        System.out.print(" n2="+n2+" N2="+_system.getN2() );
        System.out.print(" n3="+n3+" N3="+_system.getN3() );
        System.out.print(" pixelSize="+_system.getPixelSize() );
        ***/
    // where do we add band and line suffixs in???
    // getPixelSize() takes byte suffix into account
	if (start < 0 || start >= _system.getN1())
	    throw new IOException(
		"Attempt to read past edge of image for dimension 1: N1=" +
		_system.getN1() + ", read position=" + start);
	if (n2 < 0 || n2 >= _system.getN2())
	    throw new IOException(
		"Attempt to read past edge of image for dimension 2: N2=" +
		_system.getN2() + ", read position=" + n2);
	if (n3 < 0 || n3 >= _system.getN3()) {
		
		
				
	    throw new IOException( 
		"Attempt to read past edge of image for dimension 3: N3=" +
		_system.getN3() + ", read position=" + n3);
	}

	long filePos = _lblsize_front +
		(_system.getNLB() + (long)n3 * _system.getN2() + (long)n2)
							* _system.getRecsize() +
		(long)start * _system.getPixelSize() +
		_system.getNBB();
	
		// add suffixs in to set the file postion correctly
	int org_code = _system.getOrgCode();
	int suffix = 0; 
	int samples = 0;
	int lines = 0;
	int bands = 0;
    switch (org_code) {

	    case SystemLabel.ORG_BSQ:
	        // (SAMPLE,LINE,BAND)
	        // calculate suffix
	        // start is x (sample) 
	        // n2 is current line number
	        // n3 is current band number
	        samples = _system.getN1();
	        lines = _system.getN2();
	        bands = _system.getN3();
	        
	        
	        
	        // line suffix
	        suffix = n3 * (samples * _lsb);
	        
	        // sample suffix
	        suffix += lines * _ssb * n3; // for all previous bands
	        
	        suffix += n2 * _ssb ; // for this LINE
	        if (_ssb != 0 && _lsb != 0) {
	            // I'm not sure why this works but it does
	            suffix += n3 * _lsb ;
	        }
	        
	        // suffix += n3 * _ssb; // for this band
	        /***
	        if ( n2 == 0 || n2 == 299 || (n3 % 100 == 0 && n2 %100 == 0) || (n3 % 100 == 1 && n2 %100 == 1)) {
	          System.out.print("case SystemLabel.ORG_BSQ  start="+start+" samp="+samples+" bands="+bands+" lines="+lines+" s-n2="+n2);
	          System.out.println(" l-n3="+n3+" _ssb="+_ssb+" _lsb="+_lsb+" suff="+suffix );
	        } ***/
	        
	    break;
	    
	    case SystemLabel.ORG_BIP:
	        // (BAND,SAMPLE,LINE)
	        // calculate suffix
	        // start is current band
	        // n2 is current sample number
	        // n3 is current line number
	        bands = _system.getN1();
	        samples = _system.getN2();
	        lines = _system.getN3();
	        	        	        
	        suffix = n3 * _bsb * samples;
	        
	        suffix += (n2 * _bsb); // suffix for this line
	        
	        // line suffix
	        if (_bsb != 0 && _ssb != 0) {
	            // another mystery, it works but I don't know why
	            bands++;
	        }
	        
	        suffix += (n3 * _ssb * bands);
	        
	        
	        
	        
	        // each line contains all bands
	        /**
	        if ( n2 == 0 || n2 == 299 || (n3 % 100 == 0 && n2 %100 == 0) || (n3 % 100 == 1 && n2 %100 == 1)) {
	          System.out.print("BIP start="+start+" samp="+samples+" bands="+bands+" lines="+lines+" s-n2="+n2);
	          System.out.println(" l-n3="+n3+" _ssb="+_ssb+" _bsb="+_bsb+" suff="+suffix );
	        } **/
	        
	    break;
	    
	    case SystemLabel.ORG_BIL:
	        // (SAMPLE,BAND,LINE)
	        // start is current sample
	        // n2 is current band number
	        // n3 is current line number
	        samples = _system.getN1();
	        bands = _system.getN2();
	        lines = _system.getN3();
	        
	        // start is current sample
	        // n2 is current band number
	        // n3 is current line number 
	        suffix = samples * n3 * _bsb;
	        
	        if (_bsb != 0 && _ssb != 0) {
	            // another mystery, it works but I don't know why
	            bands++;
	        }
	        suffix += n3 * _ssb * bands;
	        suffix += _ssb * n2; // for previous bands in this line
	        
	        
	        /**
	        if (n3 % 100 == 0 && n2 %100 == 0) {
	          System.out.print("BIL start="+start+" lines="+lines+" n2="+n2+" n3="+n3+" _ssb="+_ssb );
	          System.out.println(" _bsb="+_bsb+" bands="+bands+" suffix="+suffix );
	        } **/
	    break;
    }	
		
		
	filePos += (long) suffix;	
	
	/***
	if ( n2 == 0 || n2 == 299 || (n3 % 100 == 0 && n2 %100 == 0) || (n3 % 100 == 1 && n2 %100 == 1)) {
	    System.out.print("  n2="+n2+" n3="+n3+"       filePos "+filePos );
	    System.out.println(" getRecsize() "+ _system.getRecsize()+ " getPixelSize() "+_system.getPixelSize() );
	} ***/
	// System.out.println(" suffix="+suffix+" filePos="+filePos );
	return filePos;
    }


/***********************************************************************
 * Returns a deep copy of the <code>VicarLabel</code> object for this file.
 * This routine should <em>not</em> be used to retrieve the system label
 * bean; use <code>getSystemLabel()</code> instead.
 * <p>
 * Note that requesting the label via this routine will cause the EOL labels
 * to be read if needed.  On a sequential-only stream, this could get you
 * into trouble and thus this should be done only after the image data has
 * been read.  If random access is possible but not easy, this can be done
 * before reading the data but you could take a significant performance hit.
 * If it is easy, then this can be called any time.
 * <p>
 * @see #getSystemLabel()
 * @throws IOException if an EOL read is required, and fails.  Will not occur
 * for random-easy streams.
 */
 
 
    public synchronized VicarLabel getVicarLabel() throws IOException
    {
        /*** we must implement this method to satisfy the VicarInput interface
	// The second condition is redundant, but just in case...
	if (!_label.isReadComplete() && _system.getEOL() != 0) {
	    seekToLocation(_lblsize_front + _image_size_bytes);
	    // _lblsize_eol = _label.readLabelChunk((InputStream) _input_stream);
	    _lblsize_eol = _label.readLabelChunk((Object) _input_stream);

	    // We do not seek back because of sequential or random-hard files.
	    // The next file access will go wherever it needs to; let's
	    // minimize work here.
	}
	_label.setReadComplete(true);

	return (VicarLabel)_label.clone();
	********************/
	    if (_embeddedVicarLabel != null) {
	        return (VicarLabel)_embeddedVicarLabel.clone();
	    }
	    else {
	        return null;
	    }
    }

/***********************************************************************
 * Indicates whether or not the <code>ISISLabel</code> has been completely
 * read.  This can be used with sequential or random-hard streams to determine
 * whether or not <code>getVicarLabel()</code> will do bad things.  The label
 * will always be completely read for random-easy streams, or for any stream
 * if there are no EOL labels.
 * @see #getVicarLabel()
 */
    public synchronized boolean isLabelComplete()
    {
	// return _label.isReadComplete();
	if (_ISIS_document != null)
	    return true;
	else return false;
    }

    


////////////////////////////////////////////////////////////////////////

}

