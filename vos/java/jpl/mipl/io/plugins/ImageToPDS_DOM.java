/*
*
* @(#)ImageToPDS_DOM.java	1.0 00/12/15
 *
 * Steve Levoe
 * Jet Propulsion Laboratory
 * Multimission Image Processing Laboratory
 * 04-19-2001 ImageIO EA2 version
*
***************************************/
// package jpl.mipl.io.plugins.vicar;
// this file and DOMutils want to end up in the above package

package jpl.mipl.io.plugins;

import org.w3c.dom.*;

import java.awt.image.*;

import javax.xml.parsers.*;

import java.io.IOException;
import java.util.*;
import java.text.SimpleDateFormat;

import javax.imageio.metadata.*;

// import VicarIO stuff
// VicarInputFile  SystemLabel
import jpl.mipl.io.streams.*;
import jpl.mipl.io.vicar.*;
import jpl.mipl.io.util.*;

/**
 * This class builds a DOM Document from a rendered image
 * <BR>
 * The System label type information is extracted from a RenderedImage.
 * This is the information any formats label needs to specify how
 * the data in the image is laid out in the file.
 * Each format will override this class to produce a DOM with the keywords
 * specific to that format. This class will emit a generic DOM which is the
 * main class which should be overridden.
 * <br>
 * Add a way to put filename into this DOM ??
 *
 * user calls getDocument() to get the Document object build here
 */
public class ImageToPDS_DOM extends ImageToDOM {
    
	
    String myFormatName = "pds"; 
    String myLabelName = "PDS_LABEL";
    String myNativeMetadataFormatName = "jpl.mipl.io.plugins.vicar.pdsimage_1.0";
    
    String  pdsLabelType = "PDS3"; // can also be "ODL3" annd "PDS4"
    
    // pds specific variables
    String band_storage_type = "BAND_SEQUENTIAL" ; // "SAMPLE_INTERLEAVED" BAND_SEQUENTIAL   LINE_INTERLEAVED
    // String band_storage_type = "SAMPLE_INTERLEAVED" ; 
    // "BIP" = "SAMPLE_INTERLEAVED"
    // "BSQ" = "BAND_SEQUENTIAL"   
    // "BIL" = "LINE_INTERLEAVED"
    // check the sampleModel to decide this (ignore for now)
    // get the class of the sample model to determine organization  
    // if (sm instanceof ComponentSampleModel) org = "BSQ";
    // only for color images
    String band_sequence = "(RED, GREEN, BLUE)";
    String sample_type = "UNSIGNED_INTEGER";
    
    String pds3SampleType = "";
    int sample_bits = 0;
    
    // file_records isn't known until the file is written
    // that value is calculated when the label is actually written out
    int file_records = 0;
    
    // we cannot calculate this from the image data. It must be passed in via imageWriteParams
    int image_start_byte = 0;
    
    String readerFormat = "";
    
    // int bands = 3; // ImageToDOM already has bands
    
    int nbb = 0; // this is the number of binary prefix bytes per line
       
    PDSimageStatistics pdsImageStatistics = null;
    
    // String keyString = "name"; // or "key"""
    String keyString = "key" ;
    // String versions, calculate must set them ??? - it will use doubles internally ??
    
    
    /**
    public ImageToPDS_DOM (BufferedImage bi_) {
        // RenderedImage is an implementation of BufferedImage
        
        
        // super(bi_, myFormatName, myNativeMetadataFormatName);
        super(bi_);
        formatName = myFormatName;
        nativeMetadataFormatName = myNativeMetadataFormatName;
        labelName = myLabelName;
        
        if (debug)     
          System.out.println("ImageToDOM(BufferedImage "+formatName+" constructor");
        // getValues();
        // buildDom();
    }
    **/
	public ImageToPDS_DOM (RenderedImage ri_) {
		
		super(ri_);
        
		nbb = 0;
		formatName = myFormatName;
		nativeMetadataFormatName = myNativeMetadataFormatName;
		labelName = myLabelName;
        
		if (debug)     
			System.out.println("ImageToDOM(RenderedImage "+formatName+" constructor");
	}
	
    public ImageToPDS_DOM (RenderedImage ri_, int nbb_) {
        // RenderedImage is an implementation of BufferedImage
        
        
        // super(bi_, myFormatName, myNativeMetadataFormatName);
        super(ri_);
        
        nbb = nbb_;
        formatName = myFormatName;
        nativeMetadataFormatName = myNativeMetadataFormatName;
        labelName = myLabelName;
        
        if (debug)     
          System.out.println("ImageToDOM(RenderedImage "+formatName+" constructor");
        // getValues();
        // buildDom();
    }
    
    // ----------------------------------------------------------
    
    /** 
    * This is an override to calculate the number of bytes in a single
    * record based on information specific to the PDS format.<br>
    * This method is called from getValues()
    **/
    public int calcRecord_bytes() {
    
    
    // file_records depends on this value, however it isn't
    // calculated until the file is actually written out
    if (band_storage_type.equals("BAND_SEQUENTIAL") )  {
        record_bytes = width * bytes_per_sample + nbb;
        // file_records = (lines * bands) + label_records;
    }
    else if (band_storage_type.equals("LINE_INTERLEAVED") )  {
        record_bytes = width * bytes_per_sample + nbb;
        // file_records = (lines * bands) + label_records;
    }
    else if (band_storage_type.equals("SAMPLE_INTERLEAVED") )  {
        record_bytes = width * bytes_per_sample * bands; // + nbb ????
        // file_records = lines  + label_records;
    }
    
    if (debug) {
    	System.out.println("ImageToPDS_DOM.calcRecord_bytes "
    	     +band_storage_type+ "  "+record_bytes+"  "+bands+"  "+width); 
		System.out.println(" nbb="+nbb);
    }
    
    return record_bytes;
    
    // "SAMPLE_INTERLEAVED" BAND_SEQUENTIAL   LINE_INTERLEAVED
    // record_bytes = width * bytes_per_sample; 
    // FILE_RECORDS = (lines * bands) + label_records
    // if SAMPLE_INTERLEAVED
    // record_bytes = width * bytes_per_sample * bands;
    // FILE_RECORDS = lines  + label_records
    }
    
    
    public void setPDSimageStatistics(PDSimageStatistics imageStatistics) {
    	pdsImageStatistics = imageStatistics;
    }
    
    public PDSimageStatistics setPDSimageStatistics() {
    	return pdsImageStatistics ;
    }
    
    
    public void setPdsLabelType(String _pdsLabelType) {
		pdsLabelType = _pdsLabelType ;
	}
    
    public void setImageStartByte(int i) {
		image_start_byte = i ;
	}
    
    public void setReaderFormat(String s) {
		readerFormat = s ;
	}
	
	public String getPdsLabelType() {
		return pdsLabelType ;
	}
    
	/**
	 * getBands
	 * can be used by PDS4ImageWriter to determine out put label template
	 * @return int bands 
	 */
	public int getBands() {
		return bands ;
	}
    /**
    * This is the class each format MUST override to construct a Document
    * with Elements specific to that format
    **/
    public void buildDom () 
    {
    if (debug) {
    	System.out.println("--------------- buildDom -------- ");
    	System.out.println("=========== pdsLabelType = "+pdsLabelType + "  =======================================");
    }
        
    // String formatStr = "NONE";
    
    // each format will have a test like this to get a format descriptor for their format
    // decide when  "MSB_UNSIGNED_INTEGER" "UNSIGNED_INTEGER" is appropriate
    // decide when  "MSB_INTEGER" is appropriate 
    // is MSB always correct since we are writing from java ??
    if (dataType == DataBuffer.TYPE_BYTE) formatStr = "UNSIGNED_INTEGER";
    if (dataType == DataBuffer.TYPE_SHORT) formatStr = "MSB_INTEGER";
    if (dataType == DataBuffer.TYPE_USHORT) formatStr = "MSB_UNSIGNED_INTEGER"; // ??? IS THIS CORRECT "UNSIGNED_INTEGER" ???"
    if (dataType == DataBuffer.TYPE_INT) formatStr = "MSB_INTEGER";
    if (dataType == DataBuffer.TYPE_FLOAT) formatStr = "IEEE_REAL"; 
    if (dataType == DataBuffer.TYPE_DOUBLE) formatStr = "IEEE_REAL";
    // SAMPLE_BITS will be 32 or 64 to distinguish double or float
    
    sample_type = formatStr;
    // CHECK TO SEE IF SAMPLE_BITS IS CONSISTENT WITH THE DATA TYPE
    
    // String band_storage_type = "SAMPLE_INTERLEAVED"; // BAND_SEQUENTIAL   LINE_INTERLEAVED
    band_storage_type = "BAND_SEQUENTIAL" ; // "SAMPLE_INTERLEAVED" BAND_SEQUENTIAL   LINE_INTERLEAVED
    // "BIP" = "SAMPLE_INTERLEAVED"
    // "BSQ" = "BAND_SEQUENTIAL"   
    // "BIL" = "LINE_INTERLEAVED"
    // check the sampleModel to decide this (ignore for now)
    // get the class of the sample model to determine organization  
    // if (sm instanceof ComponentSampleModel) org = "BSQ";
    // only for color images
    band_sequence = "(RED, GREEN, BLUE)";
    
    if (debug) System.out.println("--------------- buildDom -------- 2");
    /**
    String formatStr
    int dataType
    int width
    int height
    int bands ;
    int[] sampleSize ; // in bits 
    int b0size ; // same as sampleSize[0]
    String sampleModelClassName = null;
    String colorModelClassName
    
    * <SYSTEM_LABEL NAME="pds">
    *   <dataType>dataType</dataType>
    *   <formatString> </formatString>
    *   <width> </width>
    *   <height>
    *   <bands>
    *   <sampleSize band="0">
    *   *** one for each band ???
    *   <sampleModelName>
    *   <colorModelName>
    * </SYSTEM_LABEL>
    **/
    
    /******* 
     * Now do the real construction of the PDS Document.
     * All of the values used to create the PDS specific Document are 
     * derived in the getValues()method of the superclass ImageToDOM
     * when the class is constructed.
     **/
    try {
          // DocumentBuilder builder = factory.newDocumentBuilder();
          // document = builder.newDocument();  // Create from whole cloth
          // look at DOMUtils. creat the Document in the same way
          // them we know it will work with the serializer, XPath, XSL tools
          // probably we should ALWAYS get5 new Documents from DOMUtils
          
          DOMutils domUtils = new DOMutils();
          _document = domUtils.getNewDocument();
          
          
          // this document already has _documentName in it as its root Element
          
          /***
          // for now till DOMutils is in the right place
          _document = new org.apache.xerces.dom.DocumentImpl();
          
          Class c = _document.getClass();
          _documentName = c.getName();
          
          Element documentNameNode = (Element) _document.createElement(_documentName); 
          _document.appendChild (documentNameNode);
          *************/
          // ----------------------------
          Element system = null;
          Element root = null;
          if ( pdsLabelType.equalsIgnoreCase("PDS4")) {
        	  // don't need or want the extra Element
        	  if (debug) System.out.println("--------------- buildDom ---- PDS4 --- 3");
        	  root = (Element) _document.createElement("PDS_LABEL"); 
        	  _document.appendChild (root);
        	  system = root;
          } else {
        	  root = (Element) _document.createElement(nativeMetadataFormatName); 
        	// documentNameNode.appendChild(root);
              _document.appendChild (root);
              if (debug) System.out.println("--------------- buildDom -------- 3");
              
              system = _document.createElement("PDS_LABEL");
              // system.setAttribute("format", formatName);
              // system.setAttribute("type", "SYSTEM");
              root.appendChild (system);
          }
          
          
          
          // put everything inside system
          // this node can be extracted later and merged with a Document with the SAME
          // nativeMetadataFormatName
          Element item;
          String value;
          Text text; // this is Node's "value" Element
          // <PDS_VERSION_ID>PDS3</PDS_VERSION_ID> or <PDS3>PDS_VERSION_ID</PDS3>
          if (debug) System.out.println("--------------- buildDom ---pdsLabelType="+pdsLabelType+" ---- 3.5 ");
          if (pdsLabelType.contains("PDS")) { 
        	  if (debug) System.out.println("--------------- buildDom --  pdsLabelType.contains(\"PDS\") -- 3");
        	  item = (Element) _document.createElement("PDS_VERSION_ID"); 
        	  text = (Text) _document.createTextNode(pdsLabelType); // PDS3 or PDS4
        	  item.appendChild(text);
        	  system.appendChild(item);
          } else if (pdsLabelType.contains("ODL")) {
        	  if (debug) System.out.println("--------------- buildDom --  pdsLabelType.contains(\"ODL\") -- 3");
        	  item = (Element) _document.createElement("ODL_VERSION_ID"); 
        	  text = (Text) _document.createTextNode(pdsLabelType);
        	  item.appendChild(text);
        	  system.appendChild(item);
          } else  { // default . The OLD way
        	  if (debug) System.out.println("--------------- buildDom --  else -- 3");
        	  item = (Element) _document.createElement("PDS_VERSION_ID"); 
        	  text = (Text) _document.createTextNode("PDS3");
        	  // text = (Text) _document.createTextNode( pdsLabelType);
        	  item.appendChild(text);
        	  system.appendChild(item);
          } 
          
          
          // filename
          if (debug) System.out.println("ImageToPDS_DOM.buildDom filename = "+filename);
          if (filename != null) {
            item = (Element) _document.createElement("INPUT_FILENAME"); 
	        text = (Text) _document.createTextNode(filename);
	        item.appendChild(text);
            system.appendChild(item);
          }
          
          
          item = (Element) _document.createElement("item"); 
          item.setAttribute(keyString, "RECORD_TYPE"); 
          item.setAttribute("quoted", "false"); 
	      text = (Text) _document.createTextNode("FIXED_LENGTH");
	      item.appendChild(text);
          system.appendChild(item);
          
          item = (Element) _document.createElement("item"); 
          item.setAttribute(keyString, "RECORD_BYTES"); 
          item.setAttribute("quoted", "false"); 
	      text = (Text) _document.createTextNode(""+record_bytes);
	      item.appendChild(text);
          system.appendChild(item);
       
          // this is a really a place holder, file_records will be calculated when the 
          // file is written out
          item = (Element) _document.createElement("item"); 
          item.setAttribute(keyString, "FILE_RECORDS"); 
          item.setAttribute("quoted", "false");
	      text = (Text) _document.createTextNode("("+height+"*BANDS)+LABEL_RECORDS");
	      item.appendChild(text);
          system.appendChild(item);
          
          
          
          if ( pdsLabelType.equalsIgnoreCase("PDS4")) {
        	  // add some PDS4 specific items
        	  // "LABEL_RECORDS", 
        	  // axes -> from BANDS
        	  // "image_start_byte" => offset
        	  // pds4_data_type -> SAMPLE+TYPE + SAMPLE_BITS
        	  // axis_index_order -> from BAND_STORAGE_TYPE
        	  // "detachedFilename"
        	  // add todays date??
        	  // core_base, core_multiplier
        	  // nl = IMAGE.LINES, ns = IMAGE.LINE_SAMPLES
        	  String objectStr = "OBJECT"; // object"
              Element object = (Element) _document.createElement(objectStr); 
              // "^IMAGE"  ^ is an illegeal charater for an element name
              // may need to go to something else if IMAGE is used elsewhere
              // perhaps IMAGE_OBJECT
    	      // object.setAttribute("name", "IMAGE_DATA"); 
    	      object.setAttribute("name", "PDS4_IMAGE");
              system.appendChild(object);
              
             
              /*********
               * $label.todays_date = sys.getDateNowZ();
$label.detachedFilename = sys.getDetachedFilename();
$label.filesize_bytes = sys.getFilesize_bytes() ;    #### add 
// where can we get this from other inputs??
$label.file_records = sys.getFile_records();    ### add
if this value is 0 don't add to the label?? ignore
or use sys.getRecsize(); // not the same thing
$label.checksum32ch = sys.getChecksum();    #### add
$label.offset = sys.getOffset_bytes();
$label.axes = sys.getAxes();   ## calculates from NB
$label.pds4data_type = sys.getData_type();
$label.nl = sys.getNL();
$label.ns = sys.getNS():
$label.nb = sys.getNB();
$label.core_multiplier = sys.getCore_multiplier();
$label.core_base= sys.getCore_base();

               */
              // all of these MUST be <item Elements
              /**
               * 
               * item = (Element) _document.createElement("item"); 
          item.setAttribute(keyString, "RECORD_BYTES"); 
          item.setAttribute("quoted", "false"); 
	      text = (Text) _document.createTextNode(""+record_bytes);
	      item.appendChild(text);
          system.appendChild(item);
               */
              item = (Element) _document.createElement("item"); 
              item.setAttribute(keyString, "nl"); 
              // item = (Element) _document.createElement("nl");               
    	      text = (Text) _document.createTextNode(""+height);
    	      item.appendChild(text);
              object.appendChild(item);
              
              item = (Element) _document.createElement("item"); 
              item.setAttribute(keyString, "ns");               
    	      text = (Text) _document.createTextNode(""+width);
    	      item.appendChild(text);
              object.appendChild(item);
              
              item = (Element) _document.createElement("item"); 
              item.setAttribute(keyString, "nb");               
    	      text = (Text) _document.createTextNode(""+bands);
    	      item.appendChild(text);
              object.appendChild(item);
              
              item = (Element) _document.createElement("item"); 
              item.setAttribute(keyString, "axes"); 
              if (bands > 1) {
            	  text = (Text) _document.createTextNode("3");
              } else {
            	  text = (Text) _document.createTextNode("2");
              }
    	      item.appendChild(text);
              object.appendChild(item);
              
              item = (Element) _document.createElement("item"); 
              item.setAttribute(keyString, "core_base");             
    	      text = (Text) _document.createTextNode("0.0");
    	      item.appendChild(text);
              object.appendChild(item);
              
              item = (Element) _document.createElement("item"); 
              item.setAttribute(keyString, "core_multiplier");              
    	      text = (Text) _document.createTextNode("1.0");
    	      item.appendChild(text);
              object.appendChild(item);
              
              item = (Element) _document.createElement("item"); 
              item.setAttribute(keyString, "checksum32ch"); 
              // evemtually get a real one from the statistics obhect
    	      text = (Text) _document.createTextNode("0123456789012345678901");
    	      item.appendChild(text);
              object.appendChild(item);
              
              item = (Element) _document.createElement("item"); 
              item.setAttribute(keyString, "offset"); 
    	      text = (Text) _document.createTextNode(""+image_start_byte);
    	      item.appendChild(text);
              object.appendChild(item);
              
              item = (Element) _document.createElement("item"); 
              item.setAttribute(keyString, "reader_format"); 
    	      text = (Text) _document.createTextNode(readerFormat);
    	      item.appendChild(text);
              object.appendChild(item);
              
              // int file_records = this.file_records * this.file_records;
              item = (Element) _document.createElement("item"); 
              item.setAttribute(keyString, "file_records");  
    	      text = (Text) _document.createTextNode(""+this.file_records);
    	      item.appendChild(text);
              object.appendChild(item);
              
              String d = getDateNowZ();
              item = (Element) _document.createElement("item"); 
              item.setAttribute(keyString, "todays_date");   
    	      text = (Text) _document.createTextNode(d);
    	      item.appendChild(text);
              object.appendChild(item);
              
             //  i2PDSdom.setPDS3dataType(pds3DataType);
              
              String pds4data_type = getPds4data_type();
              item = (Element) _document.createElement("item"); 
              item.setAttribute(keyString, "pds4_data_type"); 
                 
    	      text = (Text) _document.createTextNode(pds4data_type);
    	      item.appendChild(text);
              object.appendChild(item);
              
              if (filename != null) {
            	  item = (Element) _document.createElement("item"); 
                  item.setAttribute(keyString, "detached_filename");
      	        text = (Text) _document.createTextNode(filename);
      	        item.appendChild(text);
                 object.appendChild(item);
                }
              
              if (pdsImageStatistics != null) {         		
            		pdsImageStatistics.addItems(_document, object, keyString);
              }
        	  
          }
          
          // this is also really a place holder, label_records will be calculated when the 
          // file is written out
          item = (Element) _document.createElement("IMAGE_START_RECORD"); 
          
          // item.setAttribute(keyString, "IMAGE"); 
          // item.setAttribute("quoted", "false"); 
	      text = (Text) _document.createTextNode("LABEL_RECORDS+1");
	      item.appendChild(text);
          system.appendChild(item);
          
          // leave this comment out
          // item = (Element) _document.createElement("comment"); 
	      // text = (Text) _document.createTextNode("/* this is the IMAGE object description */");
	      // item.appendChild(text);
          // system.appendChild(item);
          
          String objectStr = "OBJECT"; // object"
          Element object = (Element) _document.createElement(objectStr); 
          // "^IMAGE"  ^ is an illegeal charater for an element name
          // may need to go to something else if IMAGE is used elsewhere
          // perhaps IMAGE_OBJECT
	      // object.setAttribute("name", "IMAGE_DATA"); 
	      object.setAttribute("name", "IMAGE");
          system.appendChild(object);
          
          // put all these items in the "^IMAGE" object
          item = (Element) _document.createElement("item"); 
          item.setAttribute(keyString, "INTERCHANGE_FORMAT"); 
          item.setAttribute("quoted", "false"); 
	      text = (Text) _document.createTextNode("BINARY");
	      item.appendChild(text);
          object.appendChild(item);
          
          item = (Element) _document.createElement("item"); 
          item.setAttribute(keyString, "LINES"); 
          item.setAttribute("quoted", "false"); 
	      text = (Text) _document.createTextNode(""+height);
	      item.appendChild(text);
          object.appendChild(item);
          
          if (nbb != 0) {
          
		    item = (Element) _document.createElement("item"); 
		    item.setAttribute(keyString, "LINE_PREFIX_BYTES"); 
			item.setAttribute("quoted", "false"); 
			text = (Text) _document.createTextNode(""+nbb);
			item.appendChild(text);
			object.appendChild(item);
          }
          
          item = (Element) _document.createElement("item"); 
          item.setAttribute(keyString, "LINE_SAMPLES"); 
          item.setAttribute("quoted", "false"); 
	      text = (Text) _document.createTextNode(""+width);
	      item.appendChild(text);
          object.appendChild(item);
          
          item = (Element) _document.createElement("item"); 
          item.setAttribute(keyString, "SAMPLE_TYPE"); 
          item.setAttribute("quoted", "false"); 
	      text = (Text) _document.createTextNode(sample_type);
	      item.appendChild(text);
          object.appendChild(item);
          
          item = (Element) _document.createElement("item"); 
          item.setAttribute(keyString, "SAMPLE_BITS"); 
          item.setAttribute("quoted", "false"); 
	      text = (Text) _document.createTextNode(""+sampleSize[0]);
	      item.appendChild(text);
          object.appendChild(item);
          
          item = (Element) _document.createElement("item"); 
          item.setAttribute(keyString, "BANDS"); 
          item.setAttribute("quoted", "false"); 
	      text = (Text) _document.createTextNode(""+bands);
	      item.appendChild(text);
          object.appendChild(item);
          
          
          /** MER sis doesn't include this item **           
          if (bands == 3) {
            item = (Element) _document.createElement("item"); 
            item.setAttribute(keyString, "STORAGE_SEQUENCE"); 
            item.setAttribute("quoted", "true"); 
	        text = (Text) _document.createTextNode(band_sequence);
	        item.appendChild(text);
            object.appendChild(item);
          }
          ************************************************/
          
          item = (Element) _document.createElement("item"); 
          item.setAttribute(keyString, "BAND_STORAGE_TYPE"); 
          item.setAttribute("quoted", "false"); 
	      text = (Text) _document.createTextNode(band_storage_type);
	      item.appendChild(text);
          object.appendChild(item);
          
          
          if (pdsImageStatistics != null && !pdsLabelType.equalsIgnoreCase("PDS4")) {         		
          		pdsImageStatistics.addItems(_document, object, keyString);
          }
          
          
          
          
          
          
        } catch (Exception e) {
            // Parser with specified options can't be built
            System.out.println("ImageToPDS_DOM.buildDOM() Exception "+ e );
            e.printStackTrace();

        }
        
        if (debug) {
        	System.out.println("--------------- buildDom -------- 5");
        	System.out.println("RenderedImageToDOM.buildDOM() ");
        }
        /***
        catch (ParserConfigurationException pce) {
            System.out.println("buildDocument ParserConfigurationException "+ pce );
        }
        catch (IOException ioe) {
            System.out.println("buildDocument IOException "+ ioe );
        }
        catch (SAXException saxe) {
            System.out.println("buildDocument SAXException "+ saxe );
        }
        ****/
    
    } // buildDom
    
    
    /**
     * setPds3SampleType
     * sets the Sample R=Type from the input metadata
     * This will be used instead of the SampleType of the RenderedImage
     * If a Detached label is being created the sample type (which includes endiness)
     * must be correct for the input data file the label is pointing to
     * @param s
     */
    public void setPds3SampleType(String s) {
    	
    	pds3SampleType = s;
    }
    
    public void setSample_bits(int b) {
    	sample_bits = b;
    }
    
    public String getPds4data_type() {
    	String pds4data_type = "";
    	
    	int bytes = this.bytes_per_sample; // from RendereImage
    	// this data is in JAVA / LSB / LOW
    	// PDS3 Sample Type
    	// <item key="SAMPLE_TYPE" quoted="false">MSB_INTEGER</item>
		// <item key="SAMPLE_BITS" quoted="false">16</item>
    	if (debug) {
    		System.out.println("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++");
    		System.out.println("ImageToPDS_DOM.getPds4data_type() pds3SampleType = "+pds3SampleType+"  sample_bits "+sample_bits);
    	}
    	if (pds3SampleType != "" && sample_bits > 0) {
    		// if these are set override the RenderedImage data type
    		// if these values were not set we don't want to override
    		System.out.println("ImageToPDS_DOMgetPds4data_type() from pds3SampleType & sample_bits");
    		 if (pds3SampleType.equalsIgnoreCase("UNSIGNED_INTEGER") && sample_bits == 8) {
    			 pds4data_type = "UnsignedByte" ; 
    		 } else if (pds3SampleType.equalsIgnoreCase("MSB_INTEGER") && sample_bits == 16) {
    			 pds4data_type = "SignedMSB2" ; 
    		 } else if (pds3SampleType.equalsIgnoreCase("LSB_INTEGER") && sample_bits == 16) {
    			 pds4data_type = "SignedLSB2" ; 
    		 } else if (pds3SampleType.equalsIgnoreCase("MSB_INTEGER") && sample_bits == 32) {
    			 pds4data_type = "SignedMSB4" ; 
    		 } else if (pds3SampleType.equalsIgnoreCase("LSB_INTEGER") && sample_bits == 32) {
    			 pds4data_type = "SignedLSB4" ; 
    		 } else if (pds3SampleType.equalsIgnoreCase("MSB_UNSIGNED_INTEGER") && sample_bits == 16) {
    			 pds4data_type = "UnsignedMSB2" ; 
    		 } else if (pds3SampleType.equalsIgnoreCase("LSB_UNSIGNED_INTEGER") && sample_bits == 16) {
    			 pds4data_type = "UnsignedLSB2" ; 
    		 } else if (pds3SampleType.equalsIgnoreCase("MSB_UNSIGNED_INTEGER") && sample_bits == 32) {
    			 pds4data_type = "UnsignedMSB4" ; 
    		 } else if (pds3SampleType.equalsIgnoreCase("LSB_UNSIGNED_INTEGER") && sample_bits == 32) {
    			 pds4data_type = "UnsignedLSB4" ; 
    		 }
    		 
    		 /** not sure if I need something else to determine LSB MSB
    		 * RIEEE MSB ??, IEEE LSB ??
    		 * HIGH - x86-linux vax intel
             * LOW - sun java PPC
             * 
             * RIEEE - x86-linux vax intel (MSB)
             * IEEE - sun java PPC (LSB)
             * ***/
    		 if (pds3SampleType.equalsIgnoreCase("IEEE_REAL") && sample_bits == 32) {
    			 pds4data_type = "IEEE754LSBSingle" ; // 'SignedByte'
    		 } else if (pds3SampleType.equalsIgnoreCase("IEEE_REAL") && sample_bits == 64) {
    			 pds4data_type = "IEEE754LSBDouble0" ; // 'SignedByte'
    		 } else if (pds3SampleType.equalsIgnoreCase("RIEEE_REAL") && sample_bits == 32) {
    			 pds4data_type = "IEEE754MSBSingle" ; // 'SignedByte'
    		 } else if (pds3SampleType.equalsIgnoreCase("RIEEE_REAL") && sample_bits == 64) {
    			 pds4data_type = "IEEE754MSBDouble0" ; // 'SignedByte'
    		 }
    		 
    		 
    		
    	} else {
    	
    		if (debug) {
    			System.out.println("ImageToPDS_DOM.getPds4data_type() from dataType "+dataType);
    		}
    		// these are from the RenderedImage which is JAVA and therefore always LSB
    	 if (dataType == DataBuffer.TYPE_BYTE) pds4data_type = "UnsignedByte" ; // 'SignedByte'
         if (dataType == DataBuffer.TYPE_SHORT) pds4data_type = "SignedLSB2" ; // 'SignedMSB2'
         if (dataType == DataBuffer.TYPE_USHORT) pds4data_type = "UnsignedLSB2"; // 'UnsignedMSB2'
         if (dataType == DataBuffer.TYPE_INT) pds4data_type = "SignedLSB4" ; // 'UnsignedMSB4'  // no unsigned int??
         
         if (dataType == DataBuffer.TYPE_FLOAT) pds4data_type = "IEEE754LSBSingle";
         if (dataType == DataBuffer.TYPE_DOUBLE) pds4data_type = "IEEE754LSBDouble";
    	}
    	
    	if (debug) {
    		System.out.println("ImageToPDS_DOM.getPds4data_type() pds4dataType = "+pds4data_type+"  ");
    	}
    	return pds4data_type;
    }

    public String getDateNowZ() {
        	// <start_date_time>1997-07-07T23:48:33.442Z</start_date_time>
        	// <creation_date_time>2009-05-04T13:46:30.1158Z</creation_date_time>
        	
        	Date dNow = new Date( );
            SimpleDateFormat ft = new SimpleDateFormat ("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'");
           
            return ft.format(dNow);
        }
  
  // from VicarLabel.java    
    public String toString()
    {
	    return "ImageToPDS_DOM.toString()";
    }
    


}