/*
 * @(#)PDSInputFile.java	
 * @version 1.01 6-3-2002 JAI codec version
 *
 * @author Steve Levoe NASA/JPL
 * 8-22-2002 jdk1.4 ImageIO and codec combined version
 * 
 * 12-2003
 * This is the JAVA only version
 * There is a subclass PDSNativeInputfile which handles native  access
 * using the OAL libraries
 * 9-02-03 added JNI interface to the PDS OAL (Object Access Library)
 * which is C. The goal is that OAL should handle more image types than 
 * the java version.
 * If the OAL library is NOT found on the system the java only version 
 * will work on the file.
 * 8-05 thru 11-05 PDS detached label support added
 * all PIRL removed - see PDSInputFilePIRL.java
 * import version -> moved to workspace
 * 2-2015 SRL fixed ^IMAGE handing for a 0 start byte
 */
 


package jpl.mipl.io.vicar;




import java.io.*;

import com.sun.media.jai.codec.*;

// import java.lang.reflect.*;
// import java.beans.*;
import java.awt.image.*;
// import javax.media.jai.*;
// import java.util.Vector;
import java.util.*;

// import jpl.mipl.io.codec.*;
// import jpl.mipl.io.vicar.*;
import jpl.mipl.io.plugins.*;
import jpl.mipl.io.util.DOMutils;

// perl utilities for parsing
// import org.apache.oro.text.perl.*;

// import jpl.mipl.io.plugins.vicar.*;


import javax.imageio.ImageIO;
/*$$$$ Enable for IIO */
// 9-28-01 commented out
// import javax.media.imageio.stream.*; // EA1
import javax.imageio.stream.*; 

/*$$$$*/
import jpl.mipl.io.streams.*;
import jpl.mipl.io.ImageUtils;

// import com.sun.media.jai.codec.SeekableStream;
// import com.sun.media.jai.codec.ImageCodec;
/** InputStreamWrapper **
import jpl.mipl.mars.viewer.image.RenderedOpLoaderGetImageInputStreamWrapper;
** InputStreamWrapper **/

import org.apache.xpath.XPathAPI;
import org.w3c.dom.*;
import org.w3c.dom.traversal.NodeIterator;
// import org.xml.sax.SAXException;



import javax.media.jai.*;
import javax.xml.transform.TransformerException;

import java.awt.Rectangle;
import java.net.URL;
import java.nio.ByteOrder;



/** 
 * This class manages a single PDS input image file.
 * <p>
 * All accesses to the PDS file are thread-safe, assuming that nobody else
 * tries to access the underlying stream directly.  Thus, multiple threads
 * can issue simultaneous <code>readRecord()</code> or <code>readTile()</code>
 * requests, although each request is handled one at a time via synchronization
 * on the <code>PDSInputFile</code> object.  However, if you have a
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
 * <p> Currently only supports 8 and 16 bit image types, and 32 bit IEEE_REAL
 * <br>
 * @see VicarInputImage
 * @see VicarInput
 */


public class PDSInputFile extends VicarInputFile
{
	
	
	
    // VicarInputFile implements VicarInput
    
    


    // protected VicarLabel _label;
    public VicarLabel _embeddedVicarLabel;
    public boolean _hasEmbeddedVicarLabel = false;
    int _embedded_label_start = -1;
    
    
    
    // VicarPdsIsisImageDecodeParam _imageDecodeParam = null;
    // this should be in VicarInputFile
    ImageDecodeParam _imageDecodeParam = null;
    
    Document _PDS_document; // this is the holder of the PDS metadata
    Document _Vicar_document; // this is the holder of the Vicar metadata
    // PDSMetadata pdsMetadata = null;
    boolean gotMetadata = false;
    
    IsisSystemLabel _isisSystem = null; // PDS and ISIS have the same extra stuff
    
    // for testing
    private int _readMin, _readMax, _readCt;
    int _line_prefix_bytes = 0;
    int _line_suffix_bytes = 0;
    
    int _ssb = 0;
    int _lsb = 0;
    int _bsb = 0;
    
    int _lpb = 0;
    
   boolean debug = false;
   // boolean debug = true;
   
   boolean detachedLabel = false;
   String detachedFilename = null;
   
   ImageInputStream detachedImageInputStream = null;
   Object detachedLabelStream = null;
   
  
  // on linux ?? /usr/local/lib/liboaljni.so.1
  String oalNativeLibName = "oaljni";
  // String oalNativeLibName = "hellop";
  
   boolean goNative = false; // this will be set to true if the OAL C native library can
   // be loaded. The flag will then be used to determine if native interface calls 
   // should be used.
    
   OaImageKeywords oaImageKeywords = null;
   
   // size of all labels in bytes, used as the start byte to begin reading data for a detached label
   int front_label_size = 0;
   

    // 20110907, xing
    PDSImageReadParam pdsImageReadParam;
   
////////////////////////////////////////////////////////////////////////

/***********************************************************************
 * Dummy constructor (for now). Need to add good ones that call open().
 */
    public PDSInputFile()
    {
	super();
	if (debug) System.out.println("%%%%%%% PDSInputFile constructor $$$$$$$$$$$$$$$");
    }

    // 20110709, xing
    public PDSInputFile(PDSImageReadParam pdsImageReadParam) {
        super();
        this.pdsImageReadParam = pdsImageReadParam;
        if (debug) System.out.println("%%%%%%% PDSInputFile constructor $$$$$ with PDSImageReadParam");
    }

/***********************************************************************
 * Dummy constructor (for now). Need to add good ones that call open().
 */
    // public PDSInputFile(VicarPdsIsisImageDecodeParam imageDecodeParam)
    public PDSInputFile(ImageDecodeParam imageDecodeParam)
    {
	super();
	_imageDecodeParam = imageDecodeParam;
	// this will later be passed on to the super. 
	// Not now because other changes to that class will be implemented at the same time
	// super(imageDecodeParam) ; 
	// VicarPdsIsisImageDecodeParam _imageDecodeParam = null;
    // ImageDecodeParam imageDecodeParam
	
	// if (debug) System.out.println("%%%%%%% PDSInputFile constructor $$$$$ with VicarPdsIsisImageDecodeParam");
	if (debug) System.out.println("%%%%%%% PDSInputFile constructor $$$$$ with ImageDecodeParam");
    }


/***********************************************************************
 * Constructor that calls <code>open(String)</code>.
 *
 * @param fn name of the file to open
 * @throws IOException
 */
    public PDSInputFile(String fn) throws IOException
    {
	this();
	if (debug) System.out.println("%%%%%%% PDSInputFile constructor $$$$$ with String fn");
	open(fn);
	
    
    }
    
	public void setDebug(boolean d) {
		debug = d;
	}

	/***********************************************************************
	 * Opens a file given a filename.
	 * What about URL's?
	 * @throws IOException
	 */
		public synchronized void open(String fn) throws IOException
		{
		filename = fn;
		if (debug) System.out.println("PDSInputFile.open("+fn+")");
		
		open(new RandomAccessFile(fn, "r"));
			
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
 * setupLabels() does the PDS specific part and is called from openInternal()
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
	    System.out.println("PDSInputFile.setupLabels()"); 
	    System.out.println("input type: "+_input_stream); 
	}
	
	if (_input_stream instanceof ImageInputStream) { 
		if (debug) {
		    System.out.println("PDSInputFile.setupLabels()  _input_stream instanceof ImageInputStream"); 
		}
	
        // System.out.println("calling PDSLabelToDOM"); 
        	// PDSLabelToDOM pdsLabel2Dom = new PDSLabelToDOM((DataInput) _input_stream, null); 
            PDSLabelToDOM pdsLabel2Dom = new PDSLabelToDOM((ImageInputStream) _input_stream, null);
            // System.out.println("after calling PDSLabelToDOM"); 
            _PDS_document = pdsLabel2Dom.getDocument();
            if (debug) System.out.println("_PDS_document: "+_PDS_document); 
            // pdsMetadata is only for the ImageIO version
            // but the JAI codec will put the document into the properties
            // pdsMetadata = new PDSMetadata(_document);
            // serialize to see the XML. View in Internet Explorer
            // domUtils.serializeDocument( _PDS_document, "PDS_file.xml", "xml");
	

	  
            // should check for "dualie" PDS AND Vicar label
            // if it is a "dualie" get the vicarLabel AND _Vicar_document
            gotMetadata = true;
            // create a SystemLabel from the values in the document?
            // probably should be done in native eventually OR createSystemLabel will call native
	        _system = createSystemLabel(_PDS_document ) ;
	        _isisSystem = (IsisSystemLabel) _system;
	        if (debug) System.out.println("ImageInputStream _isisSystem = "+_isisSystem );
	        
	        _line_suffix_bytes = _isisSystem.getLineSuffixBytes();
	        _line_prefix_bytes = _isisSystem.getLinePrefixBytes(); 
	        
	        // _lsb = _line_suffix_bytes ;
	        // _lpb = _line_prefix_bytes ;
	        
	        /***
	        // put things into the properties of the image
	        setProperty("PDS_document", (Object) _PDS_document);
	        setProperty("ImageDecodeParam", (Object) imageDecodeParam);
	        setProperty("ImageDecodeParam_ClassName", (Object) imageDecodeParam.getClass().getName() );
	        setProperty("ImageFormatName", (Object) "PDS");
	        setProperty("SystemLabel", (Object) _system);
	        setProperty("SystemLabel_ClassName", (Object) _system.getClass().getName() );
	        ***/
	        // getLabelSize assumes a PDS compliant label
	        // hopefully PDS counts the embedded Vicar label in its values for label size
	        // _lblsize_front = getLabelsize(_PDS_document ) ;
	        _lblsize_front = getImageStart(_PDS_document ) ;
	        
	        // PIRL test went here -- see PDSInputFilePIRL.java
			
		
       } else if (_input_stream instanceof DataInput) {
    	   if (debug) {
   		    System.out.println("PDSInputFile.setupLabels()  _input_stream instanceof DataInput"); 
   		    // _input_stream.
   		    // e.printStackTrace();
            Exception e = new RuntimeException( "StackTrace _input_stream instanceof DataInput");
            e.printStackTrace();
   			}
        // System.out.println("calling PDSLabelToDOM"); 
        	PDSLabelToDOM pdsLabel2Dom = new PDSLabelToDOM((DataInput) _input_stream, null);
        	// BufferedReader br = new BufferedReader(new InputStreamReader((DataInput) _input_stream));
            // PDSLabelToDOM pdsLabel2Dom = new PDSLabelToDOM(br, null);
            
        	
            
            // System.out.println("after calling PDSLabelToDOM"); 
            _PDS_document = pdsLabel2Dom.getDocument();
            if (debug) {
            	System.out.println("_PDS_document: "+_PDS_document); 
            	domUtils.serializeDocument( _PDS_document, "PDS_document.xml", "xml");
            }
            // pdsMetadata is only for the ImageIO version
            // but the JAI codec will put the document into the properties
            // pdsMetadata = new PDSMetadata(_document);
            // serialize to see the XML. View in Internet Explorer
            // domUtils.serializeDocument( _PDS_document, "PDS_file.xml", "xml");
            
            // should check for "dualie" PDS AND Vicar label
            // if it is a "dualie" get the vicarLabel AND _Vicar_document
            gotMetadata = true;
            // create a SystemLabel from the values in the document
	        _system = createSystemLabel(_PDS_document ) ;
	        _isisSystem = (IsisSystemLabel) _system;
	        if (debug) System.out.println("DataInput _isisSystem = "+_isisSystem );
	        
	        _line_suffix_bytes = _isisSystem.getLineSuffixBytes();
	        _line_prefix_bytes = _isisSystem.getLinePrefixBytes(); 
	        
	        // _line_suffix_bytes = _isisSystem.getLSB();
	        // _line_prefix_bytes = _isisSystem.getLPB();  
	        /***
	        // put things into the properties of the image
	        setProperty("PDS_document", (Object) _PDS_document);
	        setProperty("ImageDecodeParam", (Object) imageDecodeParam);
	        setProperty("ImageDecodeParam_ClassName", (Object) imageDecodeParam.getClass().getName() );
	        setProperty("ImageFormatName", (Object) "PDS");
	        setProperty("SystemLabel", (Object) _system);
	        setProperty("SystemLabel_ClassName", (Object) _system.getClass().getName() );
	        ***/
	        // getLabelSize assumes a PDS compliant label
	        // hopefully PDS counts the embedded Vicar label in its values for label size
	        // _lblsize_front = getLabelsize(_PDS_document ) ;
	        _lblsize_front = getImageStart(_PDS_document ) ;
            // return ;
       } else {
            System.out.println("Improper input type: "+_input_stream); 
            System.out.println("can't read the header ");
            return;
       }
	
  
	
	// check _lblsize_front if it is -1 ERROR and exit read??
	if (debug) {
	    System.out.println("SEARCHING FOR EMBEDDED VICAR LABEL ****************");
	    System.out.println("   _lblsize_front "+_lblsize_front); 
	}
   
    // determine if there is an embedded vicar label
    // where is it ??
    int imageHeader = 0;
    int recordBytes = 0;
    
    String value = "";
    String units = "";
    
    value = domUtils.getNodeValue(_PDS_document,"//item[@key='RECORD_BYTES']");
    if (value == null) {
        value = domUtils.getNodeValue(_PDS_document,"//RECORD_BYTES");
    }
    
    if (value == null) {
    	recordBytes = 1;
    } else {
    	recordBytes = Integer.parseInt(value);
    }
    
    if (debug) System.out.println("openInternal recordBytes "+recordBytes+"  "+value); 
    
    Node node = domUtils.getSingleNode(_PDS_document,"//item[@key='^IMAGE_HEADER']");
    _hasEmbeddedVicarLabel = false;
    if (debug) System.out.println("  find single node for ^IMAGE_HEADER "+node);
    if (node != null) {
        if (debug) System.out.println("   getting value ");
        value = domUtils.getNodeValue(node);
        if (debug) System.out.println("   value >"+value+"<");
        // value may be in the attributes also -- above method finds the value in either spot
        if (value == null || value == "") {
        	// value may be in subitems
        	Vector imageSubitemVec = new Vector();
        	String xPath = "//item[@key='^IMAGE_HEADER']/subitem[@key='^IMAGE_HEADER']";
        	  
        	 // xPath = "//subitem[@key='^IMAGE']" ;
        	 String subitemValue = null;
        	 int ii=0;
        	 NodeList inList = domUtils.getNodeList(_PDS_document, xPath); 
        	 for (int x=0 ;x < inList.getLength() ; x++) {
     			Node n = inList.item(x);
     			subitemValue = domUtils.getNodeValue(n);
    	  		if (debug) System.out.println("^IMAGE_HEADER ["+ii+"]  subitemValue=>"+subitemValue+"<");
    	  		imageSubitemVec.add(subitemValue);
    	  		ii++;
        	 }
        	 
        	 /***
        	 NodeIterator  ni = domUtils.getNodeIterator(_PDS_document, xPath);
        	 Node n;
        	 // create a 
        	 int ii=0;
        	 while ((n = ni.nextNode())!= null) {
        	  		
        	 	// subitemValue = domUtils.getSubitemValue(root, xPath);
        	  		subitemValue = domUtils.getNodeValue(n);
        	  		if (debug) System.out.println("^IMAGE_HEADER ["+ii+"]  subitemValue=>"+subitemValue+"<");
        	  		imageSubitemVec.add(subitemValue);
        	  		ii++;
        	  }
        	  ***/
             int vLen =   imageSubitemVec.size();
             value="";
             if (vLen == 0) {
             	if (debug) System.out.println("^IMAGE_HEADER   has no subitems !");
             } else if (vLen == 1) {
             	if (debug) System.out.println("^IMAGE_HEADER   has 1 subitems !");
             	// assume it is the offset?? but it could be the filename??
             	// we should never get here since there wouldn't be a subitem
             	// it would just be the value of the item
             } else if (vLen == 2) {
             	if (debug) System.out.println("^IMAGE_HEADER   has 2 subitems !");
             	// assume filename first, offset 2nd
             	detachedFilename = (String)imageSubitemVec.get(0);
             	detachedLabel = true;
             	_hasEmbeddedVicarLabel = true;
             	// offset may contain <BYTES>
             	value = (String) imageSubitemVec.get(1);
             	if (debug) System.out.println("^IMAGE_HEADER  detachedFilename="+detachedFilename+"  value="+value);
             } else {
             	if (debug) System.out.println("^IMAGE_HEADER  has "+vLen+" subitems !");
             }       
        	
        if (debug) System.out.println("^IMAGE_HEADER   v="+value+"<");
        }
    
        if (value != null && value != "") {
        	 if (value.indexOf ("<BYTES>") != -1) {
              		if (debug) System.out.println("<BYTES>   v="+value+"~");
            		String v = value.replaceAll("<BYTES>",""); // remove <BYTES>
            		v = v.trim();
            		if (debug) System.out.println("<BYTES>   v="+v+"~");
            		_embedded_label_start = Integer.parseInt(v);
            		_embedded_label_start--;
            	}
            else if (value.indexOf ("<RECORDS>") != -1) {
            		String v = value.replaceAll("<RECORDS>",""); // remove <RECORD>
            		v = v.trim();          		
            		imageHeader = Integer.parseInt(v);
            		_embedded_label_start = (imageHeader - 1) * recordBytes;
            	}
        	 else {    	
         			 imageHeader = Integer.parseInt(value.trim());
         			
         			 _embedded_label_start = (imageHeader - 1) * recordBytes;
         			if (debug) System.out.println("imageHeader="+imageHeader+
         					" _embedded_label_start="+_embedded_label_start);
        	 }
          _hasEmbeddedVicarLabel = true;
        } 
        	
        
        // find out if the imageHeader value is in BYTES or RECORDS
        Hashtable attrs = domUtils.getNodeAttributesHash(node);
        // _embedded_label_start = imageHeader * recordBytes;
        if (debug) System.out.println(" walk thru attributes hashtable"); 
          if (attrs != null) {
            value = (String) attrs.get("value"); // try capitalized too ???
            
            units = (String) attrs.get("units"); // try capitalized too ???
            if (debug) System.out.println(" *** value="+value+"   units="+units); 
            if (units != null) { // found "units" in attributes
                if (units.equalsIgnoreCase("BYTES") ) {
                    // byte value is first byte of data
                    _embedded_label_start = imageHeader - 1 ;
                }
                else if (units.equalsIgnoreCase("RECORDS") ) {
                    _embedded_label_start = (imageHeader -1) * recordBytes;
                }
            }
          }
        
    }
    
    if (debug) {
    System.out.println("  recordBytes "+recordBytes+"  _hasEmbeddedVicarLabel "+_hasEmbeddedVicarLabel); 
    System.out.println("  _embedded_label_start = "+_embedded_label_start);
    System.out.println("  _lblsize_front "+_lblsize_front+" really the image data start byte" );
    System.out.println("  units "+units);
    System.out.println("detachedFilename = "+detachedFilename);
    System.out.println("detachedLabel = "+detachedLabel);
    }
  
  
  
    /***/
    // hasEmbeddedVicarLabel = pdsLabel2Dom.hasEmbeddedVicarLabel ;
    // pdsLabel2Dom should leave _input_stream at the beginning of the VicarLabel !!!
    
    if (_hasEmbeddedVicarLabel) {
    	readEmbeddedVicarLabel();	    
    }
    
    if (debug) {
        System.out.println("  AFTER readEmbeddedVicarLabel()  _hasEmbeddedVicarLabel "+_hasEmbeddedVicarLabel); 
        System.out.println("  _hasEmbeddedVicarLabel "+_hasEmbeddedVicarLabel+"  _embedded_label_start = "+_embedded_label_start);
        System.out.println("  _embeddedVicarLabel = "+_embeddedVicarLabel+"  ");
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

    // _lblsize_front MUST take into account the embedded Vicar label
    // it really is the start of the image data
	_current_file_pos = _lblsize_front;	
	}
	
	/**
	 * readPdsLabelString
	 *
	 * read from the stream and get a String containing the PDS label
	 * This method assumes the PDS label's end is signaled by "END"
	 */
	public String readPdsLabelString(ImageInputStream iis) {
		String label = null;
		String s = null;
		String trim = null;
		StringBuffer sb = new StringBuffer();
		// read till we find "END"
		try {
		iis.seek(0) ; // make sure we are the start of the file
		 while ((s = iis.readLine()) != null) {
			trim = s.trim();
			sb.append(s+"\n");
			if (trim.equalsIgnoreCase("END")) {
				label = sb.toString();
				return label;
			}
		 } 
		}
		catch (IOException ioe) {
			System.out.println("IOException reading PDS label ");
			System.out.println(" "+ioe);
			return null;
		}
		
		return label;
	}
	/**
	 * 
	 */
	public void readEmbeddedVicarLabel() {
		if (_hasEmbeddedVicarLabel) {
		DOMutils domUtils = new DOMutils();
	    _embeddedVicarLabel = new VicarLabel();
	    // _embedded_lblsize_front = _label.readLabelChunk((InputStream) _input_stream);
	    
        if (debug) System.out.println("PDSInputFile.openInternal calling readLabelChunk(Object)"+_input_stream);
    
        try {
            if (debug) System.out.println(" * seek to: _embedded_label_start "+ _embedded_label_start );
            if (debug) System.out.println(" ******** _current_file_pos "+ _current_file_pos );
            seekToLocation(_embedded_label_start) ;
            if (debug) System.out.println(" ******** _current_file_pos "+ _current_file_pos );
            int _embedded_lblsize_front = _embeddedVicarLabel.readLabelChunk((Object) _input_stream);
            if (debug) System.out.println(" ******** PDSInputFile.openInternal  read vicar label "+
                _embedded_lblsize_front);
           
            _embeddedVicarLabel.setReadComplete(true);
            
           if (debug) {
            System.out.println(" ******** _embeddedVicarLabel.isReadComplete() "+  
                        _embeddedVicarLabel.isReadComplete() );
           
            String vicStr = _embeddedVicarLabel.toString();
            System.out.println(" ******** ");
            System.out.println(vicStr);
            System.out.println(" ******** ");
            Document doc = domUtils.getNewDocument();
            Node lbl = _embeddedVicarLabel.toXML(doc);
            doc.appendChild(lbl);
            domUtils.serializeDocument(doc,"vicar_lbl.xml","xml");
            System.out.println(" ******** serialized to vicar_lbl.xml");
           }
        }
        catch (IOException ioe) {
        	if (debug) {
        		System.out.println("IOException attempting to read embedded vicar label "+ioe);
        		ioe.printStackTrace();
        	}
            
            
            _hasEmbeddedVicarLabel = false;
            _embeddedVicarLabel = null;
            
        }
	   }
    }

	/***********************************************************************
	 * Does the actual work of opening the file.  Reads in the first part
	 * of the label, and sets up the SystemLabel object.
	 * @throws IOException
	 */
		protected void openInternal() throws IOException
		{
		// Make sure we're at the beginning of the file/stream.
		// Only does anything if random-access.
		
		// do the native check here instead of setupLabels()
		//		check for the existance of a native library to read PDS images
			  // library isn't loaded until we actually try to read the file
		// loadOalLib(); // sets goNative flag
			 // all oal native methods will be oal_method()
			 
		if (goNative) {
			// use the native OAL routines to set everything up
			if (debug) System.out.println("PDSInputFile.openInternal() going native");
			setupLabels();
			
			_data_format = new VicarDataFormat(_system.getHost(),
							_system.getIntFmt(), _system.getRealFmt());
			// do all the other stuff below ???
			// probably NOT needed since all the IO is done on the native side
			_file_opened = true;
		} 
		else {
				
		seekToLocation(0);

		_lblsize_front = 0;
		_lblsize_eol = 0;

		setupLabels();
	
		_data_format = new VicarDataFormat(_system.getHost(),
				_system.getIntFmt(), _system.getRealFmt());
		
		if (debug) {
			System.out.println("PDSInputFile.openInternal()");
			System.out.println("_system "+_system.toString());
			System.out.println("detachedLabel="+detachedLabel+"  detachedFilename="+detachedFilename);
			System.out.println("data_format="+_data_format);

			// Get the host and data formats and set up the VicarDataFormat object.
			System.out.println("_system.getHost() = "+_system.getHost());
			System.out.println("_system.getIntFmt() = "+_system.getIntFmt());
			System.out.println("_system.getRealFmt() = "+_system.getRealFmt());
			System.out.println("pdsImageReadParam = "+ pdsImageReadParam);
		}
		
		
		if (detachedLabel == true ) {
			detachedLabelStream = _input_stream;
			// keep the label stream around (we might use it ???)
			// get a new input_stream for the data file pointed to by the detachedLabel
			// save the original one in case we need it for something
			// do we need to close it at some point??
			if (debug) {
				System.out.println("detachedLabel opening the image data file now");
				System.out.println("PDSInputFile.open("+detachedFilename+")");
				System.out.println("this.pdsImageReadParam = "+this.pdsImageReadParam);
			}
			// 20110709, xing, 20140319, srl
			String directoryPath = ".";
			if (this.pdsImageReadParam != null) {
					
               directoryPath = this.pdsImageReadParam.getDirectoryPath();
               if (directoryPath == null) {
                   directoryPath = ".";
                }
               if (debug) {
                    System.out.println("PDSInputFile: directory path is "+directoryPath);
               }
			}
			
			String separator = java.io.File.separator;
			// check if this is a url ( look for ':' ) 
			if (directoryPath.indexOf(':') != -1) {
				separator = "/";
				if (debug) {
	         		System.out.println("This is a URL since it contains ':' separator "+separator+"   ");
				}
			} else {
			
				int forwardSlashI = directoryPath.indexOf('/');
				int backSlashI = directoryPath.indexOf('\\');
				separator = "/";
				if (forwardSlashI >= 0) {
					separator = "/";
				} else {
					separator = "\\";
				}
				if (debug) {
	         		System.out.println("forwardSlashI "+forwardSlashI+"  backSlashI "+backSlashI);
	         		System.out.println("seperator "+separator+"    java.io.File.separator = "+java.io.File.separator);    
				}
			}
         	
			// String fullPath = directoryPath+java.io.File.separator+detachedFilename;
			// look at directoryPath and see if it is using "\" or "/" use the same things
			String fullPath = directoryPath+"/"+detachedFilename;
			if (debug) { 
				System.out.println("PDSInputFile.open("+detachedFilename+") fullPath = "+fullPath);
			}
			
			ImageUtils imUtil = new ImageUtils() ;
			imUtil.setDebug(debug);
			// This will handle opening local file, URL
			// we may still need to add something to configure web authentication: certificates or CSS
			// if (imUtil.hasGetInputStreamCallback()) {
			// this is set in the imageReadParam
			
			/** InputStreamWrapper **
			if (pdsImageReadParam != null && pdsImageReadParam.getUseMarsviewerCallback()) {
				if (debug) { 
					System.out.println("******************************************************");
					System.out.println("*** PDSInputFile                               *******");
					System.out.println("******************************************************");
					System.out.println("PDSInputFile.open() imUtil.getUseMarsviewerCallback()");					
				}
				try {
					// this could be a URL
					// set up the callback. We just created a new instance imUtil so it isn't setup by default
					// import jpl.mipl.mars.viewer.image.RenderedOpLoaderGetImageInputStreamWrapper;			
					// check if this class is available ( it is contained in the marsviewer.jar 
					// avoid throwing an unrecoverable NoClassDefFoundError if the marsviewer.jar isn't on the classpath
					// The error happens even though this piece of code is never called when 
					// the PDSInputFile constructor is called 
					String cname = "jpl.mipl.mars.viewer.image.RenderedOpLoaderGetImageInputStreamWrapper";
					Class c = Class.forName(cname, true, this.getClass().getClassLoader());
					//  old code which throws an error
					// RenderedOpLoaderGetImageInputStreamWrapper ropWrapper = new RenderedOpLoaderGetImageInputStreamWrapper();
					// imUtil.addGetInputStreamCallback(ropWrapper);
					
					if (debug) {  
						System.out.println("The class of " + c + " is " + c.getClass().getName());
					}
					RenderedOpLoaderGetImageInputStreamWrapper ropWrapper = (RenderedOpLoaderGetImageInputStreamWrapper) c.newInstance();
					imUtil.addGetInputStreamCallback(ropWrapper);
				
					_input_stream = imUtil.getImageInputStreamCallback(fullPath);
					if (debug) {
						System.out.println("PDSInputFile.openInternal _input_stream "+_input_stream);
					}
					
				}
				catch (NoClassDefFoundError ee) {
		            System.out.println("NoClassDefFoundError getImageInputStreamCallback "+ee);
		            ee.printStackTrace();
		        }
				catch (Exception e) {
		            System.out.println("Exception getImageInputStreamCallback "+e);
		            e.printStackTrace();
		        }
			} else 
			** InputStreamWrapper **/
			{
				if (debug) { 
					System.out.println("PDSInputFile.open() imUtil.getImageInputStream(()");					
				}
				_input_stream =  imUtil.getImageInputStream(fullPath) ;
			}
			
			
			/******************************
			 * This is the old way. Now there is one file/URL opening method caled from anywhere
			//RandomAccessFile raf = new RandomAccessFile(detachedFilename, "r");
			FileImageInputStream stream = null;
			// this will be replaced by a method or Class which opens a file
			// This new Class/method must be able to open File, URL or webdav
			// authentication must also be used if needed, CAM, certificates etc
			
			try {
				RandomAccessFile raf = new RandomAccessFile(fullPath, "r");
				System.out.println("PDSInputFile.open() raf = "+raf);			
				stream = new FileImageInputStream(raf);
				_input_stream = stream;
				
			} catch (FileNotFoundException fne) {
				System.out.println("PDSInputFile: FileNotFoundException "+fne);
			}
			
			if (stream == null) {
				// try again as a URL
				System.out.println("try to open as URL "+fullPath);
				try {
	            	URL url = new URL(fullPath);
	            	InputStream is = url.openStream();
	            	ImageInputStream iis = ImageIO.createImageInputStream(is);
	            	_input_stream = iis;
	            	
	            	System.out.println("new URL "+fullPath);
	                

	            } catch (Exception e) {
	                System.out.println("URL exception !");
	                _input_stream = null;
	            }
				
				
			}
			************/
			
			
			// The image is all we will read from now on
			// _input_stream = stream;
			_current_file_pos = 0;
		}
		
		if (debug) {
			System.out.println("PDSInputFile.openInternal _input_stream "+_input_stream);
		}
		if (_input_stream == null) {
			// return?? throw an exception?? Will the receiver of this detect it??
			System.out.println("PDSInputFile.openInternal _input_stream NULL "+_input_stream+"  #########################");
		}
		
		if (debug) {
			System.out.println("PDSInputFile.openInternal _hasEmbeddedVicarLabel "+_hasEmbeddedVicarLabel);
		}
		if (_hasEmbeddedVicarLabel) {
			// we have switched _input_stream (if this is a detached label) to the one which contains
			// the embedded Vicar label, now we can read the vicar label in
	    	readEmbeddedVicarLabel();
		    
	    }

		// This is a bit messy but avoids having the io.streams package
		// depend on the io.vicar package...

		int int_order = ImageInputStreamStride.HIGH_ORDER;
		if (_data_format.getIntFormatCode() == VicarDataFormat.INT_FMT_LOW)
			int_order = ImageInputStreamStride.LOW_ORDER;

		int float_order = ImageInputStreamStride.HIGH_ORDER;
		// if (_data_format.getIntFormatCode() == VicarDataFormat.REAL_FMT_RIEEE)
		if (_data_format.getRealFormatCode() == VicarDataFormat.REAL_FMT_RIEEE)
			float_order = ImageInputStreamStride.LOW_ORDER;
		// if (_data_format.getIntFormatCode() == VicarDataFormat.REAL_FMT_VAX)
		if (_data_format.getRealFormatCode() == VicarDataFormat.REAL_FMT_VAX)
			float_order = ImageInputStreamStride.VAX_ORDER;

		// If an ImageInputStream, set the byte ordering there too.  If
		// it's VAX or inconsistent, leave it BIG and the Stride class
		// will take care of it (it re-verifies the order is right).
		
		if (debug) {
			System.out.println("PDSInputFile.openInternal()");
			System.out.println("_data_format="+_data_format);
			// Get the host and data formats and set up the VicarDataFormat object.
			System.out.println("_system.getHost() = "+_system.getHost());
			System.out.println("_system.getIntFmt() = "+_system.getIntFmt());
			System.out.println("_system.getRealFmt() = "+_system.getRealFmt());
			
			System.out.println("PDSInputFile.openInternal()");
			System.out.println("ImageInputStreamStride.LOW_ORDER = "+ImageInputStreamStride.LOW_ORDER);
			System.out.println("ImageInputStreamStride.HIGH_ORDER = "+ImageInputStreamStride.HIGH_ORDER);
			System.out.println("_data_format.getRealFormatCode() ="+_data_format.getRealFormatCode());
			System.out.println("VicarDataFormat.REAL_FMT_RIEEE ="+VicarDataFormat.REAL_FMT_RIEEE);
			System.out.println("VicarDataFormat.REAL_FMT_IEEE ="+VicarDataFormat.REAL_FMT_IEEE);
			System.out.println("int_order="+int_order+", float_order="+float_order);
		}

		if (_input_stream instanceof ImageInputStream) {
			if (int_order == ImageInputStreamStride.LOW_ORDER &&
			float_order == ImageInputStreamStride.LOW_ORDER) {
				if (debug) System.out.println("PDSInputFile.openInternal()  ByteOrder.LITTLE_ENDIAN");

			((ImageInputStream)_input_stream).setByteOrder(
							ByteOrder.LITTLE_ENDIAN);
			}
			else {
				if (debug) System.out.println("PDSInputFile.openInternal()  ByteOrder.BIG_ENDIAN");
			
			((ImageInputStream)_input_stream).setByteOrder(
							ByteOrder.BIG_ENDIAN);
			}
		}
		
		

		_input_stream_wrap = new ImageInputStreamStride(_input_stream,
						int_order, float_order);

		_file_opened = true;
		// gives subclassers a hook to do something else here
		}
		
		openInternalLast();
		}	


/***********************************************************************
 * openInternalLast
 * 
 * called at the end of openInternal. Can be used by subclasses to do any 
 * extra work once openInternal has completed. Created so 
 * PDSInputFile and ISISInputFile  could have the oportunity to modify 
 * _input_stream_wrap. 
 */
protected void openInternalLast() {
	
	// provided for subclasses to put good stuff here
	// this is needed to correctly read some datasets
	if (debug) {
		System.out.println("PDSInputFile.openInternalLast() *************************************");
		System.out.println("_line_prefix_bytes "+_line_prefix_bytes);
		System.out.println("_line_suffix_bytes "+_line_suffix_bytes);
		System.out.println("nbb "+_isisSystem.getNBB());
	}
	
	
	
	readLinePrefixData(_line_prefix_bytes); // this is a Vicar and PDS only operation
	
	// _line_prefix_bytes = 0; // use _isisSystem.getNBB() instead ???
	// _isisSystem.setNBB(0);
	
	// not used currently
	// _input_stream_wrap.setLinePrefixBytes(_line_prefix_bytes);
	// _input_stream_wrap.setLineSuffixBytes(_line_suffix_bytes);
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
        
        if (debug) System.out.println("PDSInputFile.setToProperties $$$$$$$$$$$$$$$$$$$$$$");
        
        if (properties != null) {
            String n = "PDS";
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
	     if (_PDS_document != null) {
	        properties.put("PDS_document", (Object) _PDS_document);
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
    * @param Document this is specific to a Document filled from a PDS image label
    * 
    *****/
    public SystemLabel  createSystemLabel(Document doc) {
    	
    	if (debug) {
    		System.out.println("==================================================================");
    		System.out.println("*************** pdsInputFile.createSystemLabel ******************");
    	}
    	
    
        DOMutils domUtils = new DOMutils();
        // avoid grabbing the IIO enabled version
        // DOMutils domUtils = new DOMutils();
        
        // use an ISIS system label since it has some extra elements specific
        // to PDS and ISIS images
        IsisSystemLabel sys = new IsisSystemLabel();
        
        String na = "\"N/A\"" ;
        String na2 = "N/A" ;
    
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
        
        // get stuff out of the Document
        Node root = doc.getDocumentElement();
        Node result;
        NodeList nlist;
        String nodeValue;
        
        // first verify that this is the correct Document type
        
        // PDSMetadata.nativeMetadataFormatName
        
        // result = XPathAPI.selectSingleNode(root, PDSMetadata.nativeMetadataFormatName);
        // String xPath = "//"+PDSMetadata.nativeMetadataFormatName ;
        // remove depnedancy on ImageIO
        String nativeMetadataFormatName = "PDS_LABEL";
        // 
        String xPath = "//"+nativeMetadataFormatName ;
        if (debug) System.out.println("PDSInputFile.createSystemLabel() " +xPath);
       
        result = domUtils.getResultNode(root,xPath);
        // String nodeValue = getItemValue(root, xPath);
        // get the needed data from the result node
        if (debug) System.out.println("result) " +result);
        if (result == null) {
            if (debug) System.out.println("PDSInputFile.createSystemLabel() incompatable Document");
            return (SystemLabel) null;
        }
    
    /*******************************************
    * determine what this file contains
    * 3 current possibilities are handled
    * ^IMAGE ^QUBE ^SPECTRAL_QUBE
    * each of the above has an OBJECT associted with it to describe how it 
    * is stored in this file
    * NON-handled possibilities include ^HISTORY AND ^TABLE
    *********************************************/
    
    /*
    SystemLabel items which must be set to read a file
    we must set these.
    find the OBJECT, then pull the corrct items from the OBJECT
    **/
    
    // domUtils.setDebug(true);
    // System.out.println("\n ****************************************************");
    Node node = domUtils.getSingleNode(_PDS_document,"//OBJECT[@name='IMAGE']");   
    /*
    grabbing the node first and then trying to get things from that node does NOT seem to
    work. Instead we must give a more specific XPATH expression
    */
    if (node != null) { // ^IMAGE
    	
    	if (debug) System.out.println("\n createSystemLabel for ^IMAGE     ###############");
        // serialize the Node returned to see if it's what was expected
        if (debug) {
        	System.out.println("createSystemLabel for ^IMAGE");
        
        
        	Node pNode = node.getParentNode();
        	String pname = pNode.getNodeName();
        	String name = node.getNodeName();
        	System.out.println("before serialize IMAGE.xml >"+name+ "< >"+pname+"< %%%%%%%%%%%%%%%%%%%%&&&");
        	domUtils.serializeNode(node, "IMAGE.xml", "xml");
        }
    
        xPath = "//OBJECT[@name='IMAGE']/item[@key='SAMPLE_BITS']" ;
        nodeValue = domUtils.getItemValue(node, xPath);
        if (debug) System.out.println("SAMPLE_BITS <"+nodeValue+"> ");
        bits = Integer.parseInt(nodeValue);
        // use this to get format or SAMPLE_TYPE
    
        xPath = "//OBJECT[@name='IMAGE']/item[@key='SAMPLE_TYPE']" ;
        nodeValue = domUtils.getItemValue(node, xPath);
        if (debug) System.out.println("SAMPLE_TYPE <"+nodeValue+"> ");
        // convert PDS SAMPLE_TYPE to vicar
        
		if (nodeValue.equalsIgnoreCase("IEEE_REAL") && bits == 64) {
					format = "DOUB" ; 
					intFormat = "HIGH";  // "HIGH"
					realFormat = "IEEE";
					unsignedFlag = false;
			if (debug) { System.out.println("IEEE 64 DOUB"); }
				}
		// add RIEEE cases too ?? what do I look for ??
				
        if (nodeValue.equalsIgnoreCase("IEEE_REAL") && bits == 32) {
        	format = "REAL" ; 
        	intFormat = "HIGH";  // "HIGH"
        	realFormat = "IEEE";
        	unsignedFlag = false;
			if (debug) { System.out.println("IEEE 32 REAL"); }
        }
        
        if (nodeValue.equalsIgnoreCase("PC_REAL") && bits == 32) {
        	format = "REAL" ; 
        	
        	// intFormat = "HIGH";
        	// realFormat = "IEEE";
        	intFormat = "LOW";  
        	realFormat = "RIEEE";
        	host = "X86-LINUX";
        	unsignedFlag = false;
			if (debug) { System.out.println("PC_REAL 32 RIEEE"); }
        }
        
        if (nodeValue.equalsIgnoreCase("PC_REAL") && bits == 64) {
        	format = "DOUB" ; 
        	intFormat = "HIGH";  // "HIGH"
        	// realFormat = "IEEE";
        	realFormat = "RIEEE";
        	unsignedFlag = false;
			if (debug) { System.out.println("PC_REAL 64 RIEEE DOUB"); }
        }
        
        if (nodeValue.equalsIgnoreCase("MSB_INTEGER") && bits == 16) {
        	format = "HALF" ; // "USHORT""
        	intFormat = "HIGH";  // "HIGH"
        	unsignedFlag = false;
        	
        	// System.out.println("MSB_INTEGER HALF");
        	
        } else if ( bits == 16) {
        	format = "HALF" ; // "USHORT""
        	intFormat = "HIGH";  // "HIGH"
        	unsignedFlag = false;
        	
        	if (debug) System.out.println("HALF");
        }
        
        if (nodeValue.equalsIgnoreCase("MSB_UNSIGNED_INTEGER") || 
        	nodeValue.equalsIgnoreCase("PC_UNSIGNED_INTEGER") ||
            nodeValue.equalsIgnoreCase("UNSIGNED_INTEGER"))  {
            intFormat = "HIGH";  // "HIGH"
            unsignedFlag = true ;
            // intFormat = "LOW";  // "HIGH"
            if (bits == 16) { // check sample size 
                format = "HALF"; 
                // format = "USHORT" ; 
            }
        }
        else {
            unsignedFlag = false;
        }
    
    	// what about other bits and sample types ??? I seem to have ignored this
    	// another To-do
        // USHORT  was added to support PDS, vicar doesn't have USHORT - HALF is signed
        // sys.setFormat(format); // BYTE HALF FULL REAL DOUB COMP USHORT
    
        
        // xPath = "//item[@key='LINES']" ;
        xPath = "//OBJECT[@name='IMAGE']/item[@key='LINES']" ;
        if (debug) {
        	Node pNode = node.getParentNode();
        	String pname = pNode.getNodeName();
        	String name = node.getNodeName();
        	System.out.println("LINES parent name >"+name+ "< >"+pname+"< %%%%%%%%%%%%%%%%%%%%");
        }
        
        nodeValue = domUtils.getItemValue(node, xPath);
        if (debug) System.out.println("LINES "+nodeValue +" ###############");
        if (nodeValue.equalsIgnoreCase(na) || nodeValue.equalsIgnoreCase(na2)) {
        	// do nothing
        }
        else {
        	nl = Integer.parseInt(nodeValue);
        }
        // sys.setNL(nl);
    
        xPath = "//item[@key='LINE_SAMPLES']" ;
        xPath = "//OBJECT[@name='IMAGE']/item[@key='LINE_SAMPLES']" ;
        nodeValue = domUtils.getItemValue(node, xPath);
        if (debug) System.out.println("LINES_SAMPLES "+nodeValue+" ###############");
        if (nodeValue.equalsIgnoreCase(na) || nodeValue.equalsIgnoreCase(na2)) {
        	// do nothing
        }
        else {
        	ns = Integer.parseInt(nodeValue);
        }
        // sys.setNS(ns);
    
        xPath = "//OBJECT[@name='IMAGE']/item[@key='BANDS']" ;
        nodeValue = domUtils.getItemValue(root, xPath);
        if (debug) System.out.println("PDSInputFile BANDS <"+nodeValue+"> ");
        if (nodeValue != null && nodeValue != "" &&
        	nodeValue.equalsIgnoreCase(na) != true &&
        	nodeValue.equalsIgnoreCase(na2) != true) { // defaults to 1
            if (debug) System.out.println("PDSInputFile BANDS <"+nodeValue+"> "+nodeValue.length());
            nb = Integer.parseInt(nodeValue);
        }
        // sys.setNB(nb);
        
    	
        xPath = "//OBJECT[@name='IMAGE']/item[@key='LINE_PREFIX_BYTES']" ;
        
        nodeValue = domUtils.getItemValue(node, xPath);
        if (debug) System.out.println("LINE_PREFIX_BYTES <"+nodeValue+"> ");
        if (nodeValue != null && nodeValue != "") {
        	
        	line_prefix_bytes = Integer.parseInt(nodeValue);
        }
        
        xPath = "//OBJECT[@name='IMAGE']/item[@key='LINE_SUFFIX_BYTES']" ;
        
        nodeValue = domUtils.getItemValue(node, xPath);
        if (debug) System.out.println("LINE_SUFFIX_BYTES <"+nodeValue+"> ");
        if (nodeValue != null && nodeValue != "") {
        	
        	line_suffix_bytes = Integer.parseInt(nodeValue);
        }
        
        
        // BAND_STORAGE_TYPE - BAND_SEQUENTIAL LINE_INTERLEAVED SAMPLE_INTERLEAVED
        // BAND_SEQUENCE
        if (nb > 1) {
            xPath = "//OBJECT[@name='IMAGE']/item[@key='BAND_STORAGE_TYPE']" ;
            nodeValue = domUtils.getItemValue(root, xPath);
            // convert PDS SAMPLE_TYPE to vicar
            if (nodeValue != null ) {
                if (nodeValue.equalsIgnoreCase("BAND_SEQUENTIAL") )  {
                    org = "BSQ";
                }
                else if (nodeValue.equalsIgnoreCase("LINE_INTERLEAVED") )  {
                    org = "BIL";
                }
                else if (nodeValue.equalsIgnoreCase("SAMPLE_INTERLEAVED") )  {
                    org = "BIP";
                }
            }
        }
        
        
        if (debug) {
        	System.out.println("###### end createSystemLabel for ^IMAGE     ###############\n");
        }
        
        // debug = d;
        // domUtils.setDebug(d);
        
        // sys.setBandsToUse(bandsToUse);
        //    sys.setBSB(bsb);
        //    sys.setLSB(lsb);
        //    sys.setSSB(ssb);
        //    sys.setCore_base(core_base);
        //    sys.setCore_multiplier(core_multiplier);
            
        //    sys.setLPB(lpb);
        sys.setLinePrefixBytes(line_prefix_bytes);
        sys.setLineSuffixBytes(line_suffix_bytes);
    }
    else { // Be sure to search for SPECTRAL_QUBE  before QUBE so we don't get a false match
            if (debug) System.out.println("createSystemLabel LOOKING for ^SPECTRAL_QUBE ****************");
            node = domUtils.getSingleNode(_PDS_document,"//OBJECT[@name='SPECTRAL_QUBE']");    
            if (node != null) { // ^SPECTRAL_QUBE
                // serialize the Node returned to see if it's what was expected
                if (debug) System.out.println("createSystemLabel for ^SPECTRAL_QUBE");
                if (debug) domUtils.serializeNode(node, "SPECTRAL_QUBE.xml", "xml");
            
                xPath = "//OBJECT[@name='SPECTRAL_QUBE']/item[@key='AXES']" ;
                nodeValue = domUtils.getItemValue(root, xPath);
                axes = Integer.parseInt(nodeValue);
                // can axes be other than 3 ??? ignore for now
                
                xPath = "//OBJECT[@name='SPECTRAL_QUBE']/item[@key='AXES_NAME']" ;
                nodeValue = domUtils.getItemValue(root, xPath);
                // convert ISIS AXIS_NAME to vicar
                if (nodeValue != null ) {
                    if (nodeValue.equalsIgnoreCase("(SAMPLE,LINE,BAND)") )  {
                        org = "BSQ";
                    }
                    else if (nodeValue.equalsIgnoreCase("(SAMPLE,BAND,LINE)") )  {
                        org = "BIL";
                    }
                    else if (nodeValue.equalsIgnoreCase("(BAND,SAMPLE,LINE)") )  {
                        org = "BIP";
                    }
                }
                String[] axes_names = getItemStringArray ( nodeValue );
                
                
                xPath = "//OBJECT[@name='SPECTRAL_QUBE']/item[@key='CORE_ITEMS']" ;
                nodeValue = domUtils.getItemValue(root, xPath);
                int[] core_items = getItemIntArray ( nodeValue );
                
                xPath = "//OBJECT[@name='SPECTRAL_QUBE']/item[@key='CORE_ITEM_BYTES']" ;
                nodeValue = domUtils.getItemValue(root, xPath);
                if (nodeValue != null && nodeValue != "") {
                	bits = Integer.parseInt(nodeValue);
                }
                                
                xPath = "//OBJECT[@name='SPECTRAL_QUBE']/item[@key='CORE_ITEM_TYPE']" ;
                nodeValue = domUtils.getItemValue(node, xPath);
                // convert isis/pds CORE_ITEM_TYPE to vicar
                if (nodeValue.equalsIgnoreCase("MSB_UNSIGNED_INTEGER") || 
                    nodeValue.equalsIgnoreCase("UNSIGNED_INTEGER"))  {
                    intFormat = "HIGH";  // "HIGH"
                    unsignedFlag = true;
                    }
                  else {
                    intFormat = "LOW";  // "HIGH"
                    unsignedFlag = false;
                    }
                
                
                xPath = "//OBJECT[@name='SPECTRAL_QUBE']/item[@key='CORE_VALID_MINIMUM']" ;
                nodeValue = domUtils.getItemValue(root, xPath);
                core_valid_minimum = Integer.parseInt(nodeValue);
                
                xPath = "//OBJECT[@name='SPECTRAL_QUBE']/item[@key='CORE_BASE']" ;
                nodeValue = domUtils.getItemValue(root, xPath);
                core_base = Double.parseDouble(nodeValue);
                
                xPath = "//OBJECT[@name='SPECTRAL_QUBE']/item[@key='CORE_MULTIPLIER']" ;
                nodeValue = domUtils.getItemValue(root, xPath);
                core_multiplier = Double.parseDouble(nodeValue);
                
                xPath = "//OBJECT[@name='SPECTRAL_QUBE']/item[@key='SUFFIX_BYTES']" ;
                nodeValue = domUtils.getItemValue(root, xPath);
                int suffix_bytes = Integer.parseInt(nodeValue);
                
                xPath = "//OBJECT[@name='SPECTRAL_QUBE']/item[@key='SUFFIX_ITEMS']" ;
                nodeValue = domUtils.getItemValue(root, xPath);
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
        
                
                
                
                switch (bits ) {
            
                    case 1 :
                        format = "BYTE";
                    break;
                    case 2:
                    // check CORE_VALID_MINIMUM = -32752 to see if this is signed or unsigned
                        if (core_valid_minimum < 0) {
                            format = "HALF"; // HALF is signed // this is the Vicar name
                        }
                        else if (unsignedFlag == true ) {
                        	// unsigned flag is set above using 
                        	format = "USHORT" ;
                        }
                        else {
                            format = "HALF" ;
                        }
                        // which does ISIS support ???
                    break;
                    case 4:
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
            
            // sys.setLPB(lpb);
            sys.setLinePrefixBytes(line_prefix_bytes);
            sys.setLineSuffixBytes(line_suffix_bytes);
            
            if (nb == 3) isRGB = true;
            else isRGB = false;
            sys.setIsRGB(isRGB);
            }
            else {
                /* this is a placeholder since I have never received a file with a ^QUBE
                 * in it. It may be exacly the same as a ^SPECTRAL_QUBE
                 */
                if (debug) System.out.println("createSystemLabel for ^QUBE");
                node = domUtils.getSingleNode(_PDS_document,"//OBJECT[@name='QUBE']");    
                if (node != null) { // ^SPECTRAL_QUBE
                    // serialize the Node returned to see if it's what was expected
                	if (debug) {
                		domUtils.serializeNode(node, "QUBE.xml", "xml");
                	}
                }
            }
        }
    
    if (debug) {
    	System.out.println("createSystemLabel ");
    	System.out.println("createSystemLabel org="+org+"  nl="+nl+" ns="+ns+" nb="+nb);
    	System.out.println("createSystemLabel format="+format+" intFormat="+intFormat+" realFormat="+realFormat+" ");
    }
    sys.setOrg(org); // BSQ BIL BIP
    // org is needed before these so N1 N2 N3 will be set correctly
    sys.setNL(nl);
    sys.setNS(ns);
    sys.setNB(nb);
    sys.setNBB(line_prefix_bytes);
    
    // USHORT  was added to support PDS, vicar doesn't have USHORT - HALF is signed
    sys.setFormat(format); // BYTE HALF FULL REAL DOUB COMP USHORT
       
    sys.setHost(host) ;// JAVA is default

    // host IS ACTUALLY NOT USED .. this is the important one
    sys.setIntFmt(intFormat ) ;// LOW default , HIGH
    sys.setRealFmt(realFormat);

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
    * @return the Document derived from the PDS label
    */
    public Document getPDSDocument() {
            return _PDS_document;
    }
    
    /**
    *
    * @return the Document derived from the embedded vicar label
    */
    public Document getVicarDocument() {
    	if (debug) System.out.println("getVicarDocument() "+_Vicar_document);
    	
    	if (_Vicar_document == null) {
    		// create the document
    		DOMutils domUtils = new DOMutils();
    		Document doc = domUtils.getNewDocument();
            Node lbl = _embeddedVicarLabel.toXML(doc);
            doc.appendChild(lbl);
            _Vicar_document = doc;
    	}
        return _Vicar_document;
    }

    /**
    Function hasEmbeddedVicarLabel(). <p>
    <pre>
    public boolean hasEmbeddedVicarLabel() {
        if (_embeddedVicarLabel != null) {
	        return true;
	    }
	    else {
	        return false;
	    }
    }
    </pre>
    **/
    public boolean getHasEmbeddedVicarLabel() {
        return _hasEmbeddedVicarLabel;
    }
 
    /**
    * 
    * This is specific to a Document filled from a PDS image label
    * calculate the size of the image's label in bytes. This is how much must 
    * be skipped to start reading image data.
    * This method use the XPathAPI to search a Document for a Node.
    * if we get back a node then we extract a value.
    **/ 
    // public int  getLabelsize(Document doc) {
    
    public int getImageStart(Document doc) {
        
    	// debug = true;
        // jpl.mipl.util.DOMutils domUtils = new jpl.mipl.util.DOMutils();
        DOMutils domUtils = new DOMutils();
        SystemLabel sys = new SystemLabel();
        // Perl5Util perl = new Perl5Util();
    
        int imageStart = 0;
        int record_bytes = 0;
        int image_starting_record = 0;
        
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
        
        // PDSMetadata.nativeMetadataFormatName
        // xPath = "//"+PDSMetadata.nativeMetadataFormatName ;
        String nativeMetadataFormatName = "PDS_LABEL"; // put this in the PDSCodec ???
        xPath = "//"+nativeMetadataFormatName ;
        // nodeValue = getItemValue(root, xPath);
        result = domUtils.getResultNode(root, xPath );
        // get the needed data from the result node
        if (result == null) {
            System.out.println("PDSInputFile.getLabelSize() incompatable Document");
            return 0;
        }
    
        
        xPath = "//item[@key='RECORD_BYTES']" ;
        nodeValue = domUtils.getItemValue(root, xPath);
        
        try {
        record_bytes = Integer.parseInt(nodeValue);
        } catch (NumberFormatException  nfe) {        
           System.out.println("NumberFormatException "+nfe);
           System.out.println("no value found for "+xPath);
           record_bytes = 0;
        }
        if (debug) {
        	System.out.println("PDSInputFile.getLabelSize() RECORD_BYTES="+nodeValue);
        	domUtils.serializeDocument( _PDS_document, "PDS_label.xml", "xml");
    	}
    	
        
        // ---------------
        xPath = "//item[@key='^IMAGE']" ;
        Node node = domUtils.getSingleNode(_PDS_document,xPath);
        _hasEmbeddedVicarLabel = false;
        if (debug) {
        	System.out.println("  1) find single node for "+xPath+" "+node);
        	domUtils.serializeNode( node, "IMAGE_node.xml", "xml");
        }
        
        if (node != null) {
            if (debug) System.out.println("   1) getImageStart() getting value ");
            imageStart = -1;
            value = domUtils.getNodeValue(node);
            
            if (debug) { 
             System.out.println(" ^IMAGE ************  value >"+value+"< **************");
            }
            // value may be in the attributes also -- above method finds the value in either spot
            if (value != null && value != "") {
            	if (debug) 
                    System.out.println(" ^IMAGE #################  value >"+value+"<");
            	// the value may be complicated
            	// look for something of the form ("filename.pds",27)
            	// the 27 is the start record of the image data
            	int commaPos = value.indexOf(",");
            	if (commaPos != -1) {
            		
            		String[] sv = value.split("/,/");
            		// value =  (String) v.elementAt(1);
            		value = sv[1];
            		
            		value = value.replaceAll("/\\s*/","");
            		value = value.replaceAll("/\\)/","");
            		// this value might also be like the one below and contain <BYTES> in it
            		// image_starting_record = Integer.parseInt(value);
            	} 
            }
            else {
            	if (debug) System.out.println("^IMAGE *********************** look for subitems");
            	// value may be in subitems
            	
            	xPath = "//item[@key='^IMAGE']/subitem[@key='^IMAGE']";
            	  
            	 // xPath = "//subitem[@key='^IMAGE']" ;
            	 String subitemValue = null;
            	 Node n;
            	 int ii=0;
            	 
            	 // test nodeList 
            	 Vector imageSubitemVec = new Vector();
            	 NodeList inList = domUtils.getNodeList(root, xPath);
            	 ii=0;
            	 for (int x=0 ;x < inList.getLength() ; x++) {
         			n = inList.item(x);
         			subitemValue = domUtils.getNodeValue(n);
        	  		if (debug) System.out.println("^IMAGE ["+ii+"]  subitemValue=>"+subitemValue+"<");
        	  		imageSubitemVec.add(subitemValue);
        	  		ii++;
            	 }
            	 
                 int vLen =   imageSubitemVec.size();
                 value="";
                 if (vLen == 0) {
                 	if (debug) System.out.println("^IMAGE   has no subitems !");
                 	value = "";
                 } else if (vLen == 1) {
                	 // The contents of value may be either the offset or the filename.
                	 // below it will determine how to use the value
                	 value = (String)imageSubitemVec.get(0);
                	 String vTrim = value.trim();
                	 int doti = vTrim.indexOf(".");
                 	if (debug) {
                 		System.out.println("^IMAGE   has 1 subitems !");
                 		System.out.println("value "+value+"  "+vTrim+"  doti = "+doti+" ****************");                		
                 	}
                 	
                 } else if (vLen == 2) {
                 	if (debug) System.out.println("^IMAGE   has 2 subitems !");
                 	// assume filename first, offset 2nd
                 	detachedFilename = (String)imageSubitemVec.get(0);
                 	detachedLabel = true;
                 	// offset may contain <BYTES>
                 	value = (String) imageSubitemVec.get(1);
                 	if (debug) System.out.println("^IMAGE  detachedFilename="+detachedFilename+"  value="+value);
                 } else {
                 	if (debug) System.out.println("^IMAGE   has "+vLen+" subitems !");
                 }
            	             	 
            }
            
            // we have a single value. It could be just the filename or it could be the offset.
            if (debug) System.out.println("^IMAGE   value="+value+"< $$$$$$$$$$$$$$$$$$$$$$$$$$$");
            
              if (value.indexOf ("<BYTES>") != -1) {
              		if (debug) System.out.println("<BYTES>   v="+value+"~");
            		String v = value.replaceAll("<BYTES>",""); // remove <BYTES>
            		v = v.trim();
            		if (debug)System.out.println("<BYTES>   v="+v+"~");
            		imageStart = Integer.parseInt(v);
            		if (imageStart > 0) {
            			imageStart--; // header value is really the first byte of data, not the amount to skip to begin reading
            		}
            		
            		if (debug) System.out.println("<BYTES>   imageStart="+imageStart );
            	}
            	else if (value.indexOf ("<RECORDS>") != -1) {
            		String v = value.replaceAll("<RECORDS>",""); // remove <RECORD>
            		if (debug) System.out.println("<RECORDS>   v="+v+"~");
            		v = v.trim();          		
            		image_starting_record = Integer.parseInt(v);
            	}
            	else {
            		// If we have a single value it is the filename, offset value is 0 (default
            		// assume the value here is records <RECORDS>
            		int doti = value.indexOf(".");
            		// since the offset is an integer it should not contain "." A filename would
            		if (debug) System.out.println("value "+value+"  "+value.trim()+" doti = "+doti+" *****************");
            		
            		// catch exception trying to parse as an integer.
            		// if parse fails, assume it a filename with no (0) offset 
            		try {
            			image_starting_record = Integer.parseInt(value.trim());
            		} catch (NumberFormatException e) {
            			if (debug) {
            				System.out.println("NumberFormatException "+e);
            			}
            			image_starting_record = 0;
            			detachedFilename = value.trim();
                     	detachedLabel = true;
            		}
            	} 
            	       
          //  }
        
                  
            if (imageStart == -1) {
           		 imageStart = (image_starting_record - 1) * record_bytes;      		 
            } 
            else {
            	// calculate image_starting_record just as a check
            	image_starting_record = ( imageStart - 1) /record_bytes;
            }
            
            if (imageStart < 0) {
            	imageStart = 0;           
            }
            
            if (debug) {
                System.out.println(" image_starting_record "+image_starting_record); 
                System.out.println(" record_bytes "+record_bytes); 
                System.out.println(" imageStart "+imageStart);         
                System.out.println(" walk thru attributes hashtable"); 
                System.out.println(" calling domUtils.getNodeAttributesHash("+node+")"); 
            }
         
           if (node == null) {
        	   if (debug) System.out.println(" *** node is null,  attrs is null *******");
        	   image_starting_record = 0;
        	   imageStart = 0;
           } else {
           
            
         // find out if the imageHeader value is in BYTES or RECORDS
            Hashtable attrs = domUtils.getNodeAttributesHash(node);
            
            if (attrs != null) 
                {
                if (debug) System.out.println(" *** attrs isn't null *******");
                String v = (String) attrs.get("value"); // try capitalized too ???
                if (v != null) { // value may be a real value, not in an attribute
                    value = v;
                    image_starting_record = Integer.parseInt(value);
                }
                units = (String) attrs.get("units"); // try capitalized too ???
                if (debug) System.out.println(" *** value="+value+"   units="+units); 
            
                String file = (String) attrs.get("file"); // try capitalized too ???
                if (debug) System.out.println("^IMAGE file="+file);
            
                if (units != null) { // found "units" in attributes
                    if (units.equalsIgnoreCase("BYTES") ) {
                        // byte value is first byte of data
                        imageStart = image_starting_record - 1 ;
                    }
                    else if (units.equalsIgnoreCase("RECORDS") ) {
                        imageStart = (image_starting_record -1) * record_bytes;
                    }
                }
             }
           } 
        }
        else { // node == null   now try QUBE and SPECTRAL_QUBE
            node = domUtils.getSingleNode(_PDS_document,"//item[@key='^SPECTRAL_QUBE']");
            _hasEmbeddedVicarLabel = false;
            if (debug) System.out.println("  find single node for ^SPECTRAL_QUBE "+node);
            if (node != null) {
                if (debug) System.out.println("   getImageStart() getting value ^SPECTRAL_QUBE");
                value = domUtils.getNodeValue(node);
                if (debug) System.out.println("   value "+value);
                // value may be in the attributes also -- above method finds the value in either spot
                if (value != null) {
                    image_starting_record = Integer.parseInt(value);
                }
                imageStart = (image_starting_record - 1) * record_bytes ;
            }
            else {
                node = domUtils.getSingleNode(_PDS_document,"//item[@key='^QUBE']");
                _hasEmbeddedVicarLabel = false;
                if (debug) System.out.println("  find single node for ^QUBE "+node);
                if (node != null) {
                    if (debug) System.out.println("   getImageStart() getting value ");
                    value = domUtils.getNodeValue(node);
                    if (debug) System.out.println("   value "+value);
                    // value may be in the attributes also -- above method finds the value in either spot
                    if (value != null) {
                        image_starting_record = Integer.parseInt(value);
                    }
                imageStart = (image_starting_record - 1) * record_bytes;
                }
            }
        }
       

    if (debug) {
    	System.out.println("*****   PDSInputFile.getImageStart() imageStart="+imageStart);
    	System.out.println("image_starting_record="+image_starting_record);
    }
        
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
        if (debug) {
        	/*
        System.out.print("PDSInputFile.calcFilePos: "+_system.getOrg());
		System.out.print(" NBB="+_system.getNBB()+" _line_prefix_bytes=" +_line_prefix_bytes);
        System.out.print(" start="+start+" N1="+_system.getN1() );
        System.out.print(" n2="+n2+" N2="+_system.getN2() );
        System.out.print(" n3="+n3+" N3="+_system.getN3() );
        System.out.println(" pixelSize="+_system.getPixelSize() );
        */
        }
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
	if (n3 < 0 || n3 >= _system.getN3())
	    throw new IOException(
		"Attempt to read past edge of image for dimension 3: N3=" +
		_system.getN3() + ", read position=" + n3);

	long filePos = _lblsize_front +
		(_system.getNLB() + (long)n3 * _system.getN2() + (long)n2)
							* _system.getRecsize() +
		(long)start * _system.getPixelSize() +
		_system.getNBB();
	
		// System.out.print("PDSInputFile.calcFilePos: "+filePos);
		
		// add suffixs in to set the file postion correctly
	int org_code = _system.getOrgCode();
	int suffix = 0; 
	int prefix = 0;
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
	        
	        
	        
	        // if (start == 0) {
	        suffix = _line_prefix_bytes ;
	        // }
	        // add prefix for current line ??
	        // System.out.println("calFilePos.BSQ _lblsize_front "+_lblsize_front+" start="+start+" n2="+n2+" n3="+n3+" suffix="+suffix+" filePos="+filePos);
	        
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
	        
	        suffix = 0;	        	        
	        
	        suffix +=_line_prefix_bytes;
	        
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
	        suffix = 0;
	        
	        suffix +=_line_prefix_bytes;
	        /**
	        if (n3 % 100 == 0 && n2 %100 == 0) {
	          System.out.print("BIL start="+start+" lines="+lines+" n2="+n2+" n3="+n3+" _ssb="+_ssb );
	          System.out.println(" _bsb="+_bsb+" bands="+bands+" suffix="+suffix );
	        } **/
	    break;
    }	
		
	// nbb is now set which is the same as _line_prefix_bytes	
	// filePos += (long) suffix;	
	
	/***
	if ( n2 == 0 || n2 == 299 || (n3 % 100 == 0 && n2 %100 == 0) || (n3 % 100 == 1 && n2 %100 == 1)) {
	    System.out.print("  n2="+n2+" n3="+n3+"       filePos "+filePos );
	    System.out.println(" getRecsize() "+ _system.getRecsize()+ " getPixelSize() "+_system.getPixelSize() );
	} ***/
	// System.out.print(" NBB="+_system.getNBB()+" _line_prefix_bytes=" +_line_prefix_bytes);
	// System.out.println("   filePos: "+filePos);
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
 * Indicates whether or not the <code>PDSLabel</code> has been completely
 * read.  This can be used with sequential or random-hard streams to determine
 * whether or not <code>getVicarLabel()</code> will do bad things.  The label
 * will always be completely read for random-easy streams, or for any stream
 * if there are no EOL labels.
 * @see #getVicarLabel()
 */
    public synchronized boolean isLabelComplete()
    {
	// return _label.isReadComplete();
	if (_PDS_document != null)
	    return true;
	else return false;
    }

   


////////////////////////////////////////////////////////////////////////

}

