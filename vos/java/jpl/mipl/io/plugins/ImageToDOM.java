/*
*
* @(#)ImageToDOM.java	1.0 00/12/15
 *
 * Steve Levoe
 * Jet Propulsion Laboratory
 * Multimission Image Processing Laboratory
 * 04-19-2001 ImageIO EA2 version
*
***************************************/
// package jpl.mipl.io.plugins.vicar;


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
public class ImageToDOM {
    
    boolean debug = false;
    
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
    int bytes_per_sample ;
    int record_bytes ;
    
    int elements ; // I don't think we need this
    
    String sampleModelClassName = null;
    String colorModelClassName = null;
    
    String filename = null;
    
    
    
    Document _document = null;
    
    String _documentName = null;
    
    String labelName = "IMAGE_LABEL";
    String formatName = "generic"; 
    /**
    * this should have something to distuinguish it so it can be merged 
    * so it can be merged with one created by a reader or transcoder for that format
    * "pds", "vicar", later fits, isis will be overide classes based on this one
    * <SYSTEM_LABEL NAME="pds">
    * ALL THE INFORMATION HERE
    * </SYSTEM_LABEL>
    **/
    
    // overiding class should set this value
    
    // String nativeMetadataFormatName = "jpl.mipl.io.plugins.vicar.pdsimage_1.0";
    // String nativeMetadataFormatName = "VICAR_LABEL";
    String nativeMetadataFormatName = "jpl.mipl.io.plugins.vicar.renderedimage_1.0";
    
    
    /**
    
    **/
    
    // Constructor
    /***
    public ImageToDOM (BufferedImage bi_) {
        
        bi = bi_;
        if (debug)     
          System.out.println("ImageToDOM(BufferedImage "+formatName+" constructor");
        // getValues();
        // buildDom();
    }
    ***/
    
    public ImageToDOM (BufferedImage bi_, String formatName_, String nativeMetadataFormatName_) {
        
        bi = bi_;
        formatName = formatName_;
        nativeMetadataFormatName = nativeMetadataFormatName_;
        
        if (debug)     
          System.out.println("ImageToDOM(BufferedImage "+formatName+" constructor");
        // getValues();
        // buildDom();
    }
    
    public ImageToDOM (RenderedImage ri_) {
        
        if (ri_ instanceof BufferedImage) {
            if (debug) System.out.println("ImageToDOM(RenderedImage) instanceof BufferedImage constructor");
            bi = (BufferedImage) ri_;
        }
        else {
            if (debug) System.out.println("ImageToDOM(RendereredImage  constructor");
            ri = ri_;
        }
        
        if (debug)     
          System.out.println("ImageToDOM(RenderedImage)  constructor");
        // getValues();
        // buildDom();
    }
    
    
    // --------------------------------------------------------
    
    public void setFilename(String f) {
        filename = f;
    }
    
    public String getFilename() {
        return filename ;
    }
    
    public void clearDocument() {
        // should check for null document??
        
        _document = null;
    }
    
    // allow a setter???
    public Document getDocument() {
        // should check for null document??
        
        if (_document == null) {
            getValues();
            buildDom();
        }
        return _document;
    }
    
    String getFormatName() {
        return formatName;
    }
    
    public void setDebug( boolean d) {
    	debug = d;
    }
    
    
    public void setNativeMetadataFormatName(String name) {
        nativeMetadataFormatName = name;
    }
    
    public String getNativeMetadataFormatName() {
        return nativeMetadataFormatName;
    }
    
    /**
    * This class will get all the data values from the RenderedImage and place
    * them in global variables. buildDOM() should be called next to construct a 
    * Document using the values. Always use BufferedImage since RenderedImage 
    * implements BufferedImage. We can get the values we need from BufferedImage. 
    * This class should not need to be overidden. buildDOM is the class to overide
    * for a specific format
    ***/
    public void getValues () 
    {
    if (debug) System.out.println("--------------- getValues -------- ");
    // set a flag to show we already did this ???  
    
    if (bi != null) {
        sm = bi.getSampleModel();
        cm = bi.getColorModel();
        width = bi.getWidth();
        height = bi.getHeight();
    }
    else if (ri != null) {
        sm = ri.getSampleModel();
        cm = ri.getColorModel();
        width = ri.getWidth();
        height = ri.getHeight();
    }
    else {
        if (debug) System.out.println("--------------- getValues -- ERROR -- bi and ri null-------- ");
    }
    
    dataType = sm.getDataType();
        
    sampleModelClassName = sm.getClass().getName();
    if (cm != null) {
    	colorModelClassName = cm.getClass().getName();
    }
    else {
    	colorModelClassName = "noColorModel";
    }
    
    
    
    // sample model is for a tile, not the whole image
    // int height = sm.getHeight();
    // int width = sm.getWidth();
    
    bands = sm.getNumBands();
    sampleSize = sm.getSampleSize(); // sampleSize[0] equals b0size
    b0size = sm.getSampleSize(0);
    int elements = sm.getNumDataElements();
    if (debug) {
    	System.out.println("height="+height+"  width="+width+"  bands="+bands );
    	System.out.println("dataElements="+elements+"  b0size="+b0size   );
    	for (int i=0 ; i< sampleSize.length ; i++) {
        	System.out.println(" sampleSize["+i+"]="+sampleSize[i]);
    	}
    }
    
    // this part is tricky, the values depend on the output sample interleave type
    // for now I'll pretend all sample sizes are multiples of 8
    bytes_per_sample = (sampleSize[0] / 8);
    
    // each format will need to overide this to use information specifc to the format
    // for calculating the size of a single record
    record_bytes = calcRecord_bytes(); 
    // record_bytes = width * bytes_per_sample * bands;
    
    if (debug) {
    	System.out.println("  ######################################################## ");
    	System.out.println("  record_bytes="+record_bytes+"  bytes_per_samples="+bytes_per_sample );
    	System.out.println("  width="+width+"  bands="+bands );
      }
    } // getValues
    
    /** 
    * subclasses will overdide this to calculate the number of bytes in a single
    * record based on information specific to the format.<br>
    * This method is called from getValues()
    **/
    public int calcRecord_bytes() {
        record_bytes = width * bytes_per_sample;
        return record_bytes;
    }
    
    /**
    * This is the class each format MUST overide to construct a Document
    * with Elements specific to that format
    **/
    public void buildDom () 
    {
    if (debug) System.out.println("--------------- buildDom -------- ");
        
    // String formatStr = "NONE";
    
    // each format will have a tet like this to get a format descriptor for their format
    if (dataType == DataBuffer.TYPE_BYTE) formatStr = "TYPE_BYTE";
    if (dataType == DataBuffer.TYPE_SHORT) formatStr = "TYPE_SHORT";
    if (dataType == DataBuffer.TYPE_USHORT) formatStr = "TYPE_USHORT"; // ??? IS THIS CORRECT
    if (dataType == DataBuffer.TYPE_INT) formatStr = "TYPE_INT";
    if (dataType == DataBuffer.TYPE_FLOAT) formatStr = "TYPE_FLOAT";
    if (dataType == DataBuffer.TYPE_DOUBLE) formatStr = "TYPE_DOUBLE";
    
    // THIS NEED ALOT MORE ATTENTION
    String org = "BSQ"; // BIL BIP
    // get the class of the sample model to determine organization  
    // if (sm instanceof ComponentSampleModel) org = "BSQ";
    
    
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
    
    // System.out.println("--------------- buildDom -------- 3");
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
          
          
          // ----------------------------
          
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
          
          
          
          // put everything inside system
          // this node can be extracted later and merged with a Document with the SAME
          // nativeMetadataFormatName
          Element item;
          String value;
          Text text; // this is Node's "value" Element
          item = (Element) _document.createElement("dataType"); 
	      // item.setAttribute("key", key); 
	      value = ""+dataType;
	      text = (Text) _document.createTextNode(value);
	      item.appendChild(text);
          system.appendChild(item);
          
          item = (Element) _document.createElement("formatString"); 
	      text = (Text) _document.createTextNode(formatStr);
	      item.appendChild(text);
          system.appendChild(item);
          
          item = (Element) _document.createElement("width"); 
	      text = (Text) _document.createTextNode(""+width);
	      item.appendChild(text);
          system.appendChild(item);
       
          item = (Element) _document.createElement("height"); 
	      text = (Text) _document.createTextNode(""+height);
	      item.appendChild(text);
          system.appendChild(item);
          
          item = (Element) _document.createElement("bands"); 
	      text = (Text) _document.createTextNode(""+bands);
	      item.appendChild(text);
          system.appendChild(item);
          
          // bytes_per_sample  record_bytes
          item = (Element) _document.createElement("bytes_per_sample"); 
	      text = (Text) _document.createTextNode(""+bytes_per_sample);
	      item.appendChild(text);
          system.appendChild(item);
          
          item = (Element) _document.createElement("record_bytes"); 
	      text = (Text) _document.createTextNode(""+record_bytes);
	      item.appendChild(text);
          system.appendChild(item);
          
          for (int i=0 ; i < sampleSize.length ; i++) {
            item = (Element) _document.createElement("sampleSize"); 
	        item.setAttribute("band", ""+i);
	        text = (Text) _document.createTextNode(""+sampleSize[i]);
	        item.appendChild(text);
            system.appendChild(item);
          }
          
          item = (Element) _document.createElement("sampleModelClassName"); 
	      text = (Text) _document.createTextNode(sampleModelClassName);
	      item.appendChild(text);
          system.appendChild(item);
          
          item = (Element) _document.createElement("colorModelClassName"); 
	      text = (Text) _document.createTextNode(colorModelClassName);
	      item.appendChild(text);
          system.appendChild(item);
          
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
	    return "RenderedImageToDOM.toString()";
    }
    


}