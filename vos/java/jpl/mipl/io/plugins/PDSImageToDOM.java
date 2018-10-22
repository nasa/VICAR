/*
*
* @(#)PDSImageToDOM.java	1.0 00/12/15
 *
 * Steve Levoe
 * Jet Propulsion Laboratory
 * Multimission Image Processing Laboratory
 * 04-19-2001 ImageIO EA2 version
 * 8-21-2002 jdk1.4 version imageIO is now part of core
*
***************************************/


package jpl.mipl.io.plugins;

import org.w3c.dom.Document;
import org.w3c.dom.DOMException;
import org.w3c.dom.Element;
import org.w3c.dom.*;

import java.awt.image.*;

import javax.xml.parsers.*;

import java.io.IOException;
import java.util.*;

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
 * Each format will overide this class to produce a DOM with the keywords
 * specific to that format. This class will emit a generic DOM which is the
 * main class which should be overridden.
 * <br>
 * Add a way to put filename into this DOM ??
 */
public class PDSImageToDOM extends ImageToDOM {
    
    String myFormatName = "generic"; 
    String myNativeMetadataFormatName = "jpl.mipl.io.plugins.vicar.pdsimage_1.0";
    boolean debug = false;
    
    /******************************************
    private static final boolean debug = false;
    
    RenderedImage ri = null;
    BufferedImage bi = null;
    // data values from the RenderedImage used to describe the image contents
    SampleModel sm ;
    ColorModel cm ;
    // the names of variables will mirror the SampleModel and ColorModel variables
    int dataType ;
    String formatStr = "NONE";
    
    int width ;
    int height ;
    int bands ;
    int[] sampleSize ; // in bits 
    int b0size ; // same as sampleSize[0]
    // b0size = sm.getSampleSize(0);
    
    int elements ; // I don't think we need this
    
    String sampleModelClassName = null;
    String colorModelClassName = null;
    
    
    
    
    
    private Document _document = null;
    
    String _documentName = null;
    
    String formatName = "generic"; 
    /
    * this should have something to distuinguish it so it can be merged 
    * so it can be merged with one created by a reader or transcoder for that format
    * "pds", "vicar", later fits, isis will be overide classes based on this one
    * <SYSTEM_LABEL NAME="pds">
    * ALL THE INFORMATION HERE
    * </SYSTEM_LABEL>
    **
    
    // overiding class should set this value
    
    // String nativeMetadataFormatName = "jpl.mipl.io.plugins.vicar.pdsimage_1.0";
    // String nativeMetadataFormatName = "VICAR_LABEL";
    String nativeMetadataFormatName = "jpl.mipl.io.plugins.vicar.renderedimage_1.0";
    
    *********************************************************************/
    
    
    
    
    
    public PDSImageToDOM (BufferedImage bi_) {
        // RenderedImage is an implementation of BufferedImage
        
        
        // super(bi_, myFormatName, myNativeMetadataFormatName);
        super(bi_);
        formatName = myFormatName;
        nativeMetadataFormatName = myNativeMetadataFormatName;
        
        if (debug)     
          System.out.println("ImageToDOM(BufferedImage "+formatName+" constructor");
        // getValues();
        // buildDom();
    }
    
    
    // --------------------------------------------------------
    
    /**
    * This is the class each format MUST overide to construct a Document
    * with Elements specific to that format
    **/
    public void buildDom () 
    {
    if (debug) {
    	System.out.println("--------------- buildDom -------- ");
    	} 
        
    // String formatStr = "NONE";
    
    // each format will have a tet like this to get a format descriptor for their format
    // decide when  "MSB_UNSIGNED_INTEGER" "UNSIGNED_INTEGER" is appropriate
    // decide when  "MSB_INTEGER" is appropriate
    if (dataType == DataBuffer.TYPE_BYTE) formatStr = "UNSIGNED_INTEGER";
    if (dataType == DataBuffer.TYPE_SHORT) formatStr = "MSB_INTEGER";
    if (dataType == DataBuffer.TYPE_USHORT) formatStr = "UNSIGNED_INTEGER"; // ??? IS THIS CORRECT
    if (dataType == DataBuffer.TYPE_INT) formatStr = "MSB_INTEGER";
    if (dataType == DataBuffer.TYPE_FLOAT) formatStr = "IEEE_REAL"; 
    if (dataType == DataBuffer.TYPE_DOUBLE) formatStr = "IEEE_REAL"; 
    // SAMPLE_BITS will 32 or 64
    
    String sample_type = formatStr;
    // CHECK TO SEE IF SAMPLE_BITS IS CONSISTENT WITH THE DATA TYPE
    
    String band_storage_type = "SAMPLE_INTERLEAVED"; // BAND_SEQUENTIAL   LINE_INTERLEAVED
    // check the sampleModel to decide this (ignore for now)
    // get the class of the sample model to determine organization  
    // if (sm instanceof ComponentSampleModel) org = "BSQ";
    // only for color images
    String band_sequence = "(RED, GREEN, BLUE)";
    
    
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
    
    /******* now do the real construction *********/
    try {
          // DocumentBuilder builder = factory.newDocumentBuilder();
          // document = builder.newDocument();  // Create from whole cloth
          // look at DOMUtils. creat the Document in the same way
          // them we know it will work with the serializer, XPath, XSL tools
          // probably we should ALWAYS get5 new Documents from DOMUtils
          
          DOMutils domUtils = new DOMutils();
          _document = domUtils.getNewDocument();
          // this document already has _documentName in it as its root Element
          
          
          
          Element root = (Element) _document.createElement(nativeMetadataFormatName); 
          
          // documentNameNode.appendChild(root);
          _document.appendChild (root);
          
          /**
          Element formatNode = _document.createElement(formatName);          
          root.appendChild (formatNode);
          **/
          // OR
          // <SYSTEM_LABEL name="generic">
          Element system = _document.createElement("SYSTEM_LABEL");
          system.setAttribute("name", formatName);
          root.appendChild (system);
          
          // fior now I'll pretend all sample sizes are multiples of 8
          int bytes_per_sample = (sampleSize[0] / 8);
          int record_bytes = width * bytes_per_sample ;
          // put everything inside system
          // this node can be extracted later and merged with a Document with the SAME
          // nativeMetadataFormatName
          Element item;
          String value;
          Text text; // this is Node's "value" Element
          item = (Element) _document.createElement("PDS_VERSION_ID"); 
	      text = (Text) _document.createTextNode("PDS3");
	      item.appendChild(text);
          system.appendChild(item);
          
          item = (Element) _document.createElement("item"); 
          item.setAttribute("key", "RECORD_TYPE"); 
          item.setAttribute("quoted", "false"); 
	      text = (Text) _document.createTextNode("FIXED_LENGTH");
	      item.appendChild(text);
          system.appendChild(item);
          
          item = (Element) _document.createElement("item"); 
          item.setAttribute("key", "RECORD_BYTES"); 
          item.setAttribute("quoted", "false"); 
	      text = (Text) _document.createTextNode(""+record_bytes);
	      item.appendChild(text);
          system.appendChild(item);
       
          
          item = (Element) _document.createElement("item"); 
          item.setAttribute("key", "FILE_RECORDS"); 
          item.setAttribute("quoted", "false"); 
	      text = (Text) _document.createTextNode(height+"+LABEL_RECORDS");
	      item.appendChild(text);
          system.appendChild(item);
          
          Element object = (Element) _document.createElement("object"); 
	      item.setAttribute("name", "^IMAGE"); 
          system.appendChild(object);
          
          // put all these items in the "^IMAGE" object
          item = (Element) _document.createElement("item"); 
          item.setAttribute("key", "INTERCHANGE_FORMAT"); 
          item.setAttribute("quoted", "false"); 
	      text = (Text) _document.createTextNode("BINARY");
	      item.appendChild(text);
          object.appendChild(item);
          
          item = (Element) _document.createElement("item"); 
          item.setAttribute("key", "BANDS"); 
          item.setAttribute("quoted", "false"); 
	      text = (Text) _document.createTextNode(""+bands);
	      item.appendChild(text);
          object.appendChild(item);
          
          item = (Element) _document.createElement("item"); 
          item.setAttribute("key", "BAND_STORAGE_TYPE"); 
          item.setAttribute("quoted", "false"); 
	      text = (Text) _document.createTextNode(band_storage_type);
	      item.appendChild(text);
          object.appendChild(item);
          
          /***** remove MER sis doesn't include this keyword **
          if (bands == 3) {
            item = (Element) _document.createElement("item"); 
            item.setAttribute("key", "STORAGE_SEQUENCE"); 
            item.setAttribute("quoted", "false"); 
	        text = (Text) _document.createTextNode(band_sequence);
	        item.appendChild(text);
            object.appendChild(item);
          }
          ****/
          
          item = (Element) _document.createElement("item"); 
          item.setAttribute("key", "LINES"); 
          item.setAttribute("quoted", "false"); 
	      text = (Text) _document.createTextNode(""+height);
	      item.appendChild(text);
          object.appendChild(item);
          
          item = (Element) _document.createElement("item"); 
          item.setAttribute("key", "LINE_SAMPLES"); 
          item.setAttribute("quoted", "false"); 
	      text = (Text) _document.createTextNode(""+width);
	      item.appendChild(text);
          object.appendChild(item);
          
          item = (Element) _document.createElement("item"); 
          item.setAttribute("key", "SAMPLE_BITS"); 
          item.setAttribute("quoted", "false"); 
	      text = (Text) _document.createTextNode(""+sampleSize[0]);
	      item.appendChild(text);
          object.appendChild(item);
          
          item = (Element) _document.createElement("item"); 
          item.setAttribute("key", "SAMPLE_TYPE"); 
          item.setAttribute("quoted", "false"); 
	      text = (Text) _document.createTextNode(sample_type);
	      item.appendChild(text);
          object.appendChild(item);
          
        } catch (Exception e) {
            // Parser with specified options can't be built
            System.out.println("RenderedImageToDOM.buildDOM() Exception "+ e );
            e.printStackTrace();

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
  
  // from VicarLabel.java    
    public String toString()
    {
	    return "PDSImageToDOM.toString()";
    }
    


}