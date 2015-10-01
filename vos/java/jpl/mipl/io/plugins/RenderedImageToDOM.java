/*
*
* @(#)RenderedImageToDOM.java	1.0 00/12/15
 *
 * Steve Levoe
 * Jet Propulsion Laboratory
 * Multimission Image Processing Laboratory
 * 04-19-2001 ImageIO EA2 version
*
***************************************/
package jpl.mipl.io.plugins;
// this file and DOMutils want to end up in the above package

import org.w3c.dom.Document;
import org.w3c.dom.DOMException;
import org.w3c.dom.Element;
import org.w3c.dom.*;

import javax.xml.parsers.*;

import java.io.IOException;
import java.util.*;

import javax.imageio.metadata.*;

import javax.media.jai.*;
import java.awt.image.*;

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
public class RenderedImageToDOM {
    
    private  boolean debug = false;
    
    RenderedImage ri;
    BufferedImage bi;
    // data values from the RenderedImage used to describe the image contents
    SampleModel sm ;
    ColorModel cm ;
    // the names of variables will mirror the SampleModel and ColorModel variables
    int dataType ;
    
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
    // add another constructor to set the 
    // VicarMetadata
    public RenderedImageToDOM (RenderedImage ri_) {
        
        ri = ri_;
        if (debug)     
          System.out.println("RenderedImageToDOM(RenderedImage "+formatName+" constructor");
        getValues();
        buildDom();
    }
    
    public RenderedImageToDOM (BufferedImage bi_) {
        
        bi = bi_;
        if (debug)     
          System.out.println("RenderedImageToDOM(BufferedImage "+formatName+" constructor");
        getValues();
        buildDom();
    }
    
    
    // --------------------------------------------------------
    
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
    
    public void setDebug(boolean d) {
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
    * Document using the values. 
    * This class should not need to be overidden. buildDOM is the class to overide
    * for a specific format
    ***/
    public void getValues () 
    {
    if (debug) System.out.println("--------------- getValues -------- ");
    // set a flag to show we already did this ???    
    if (ri != null) {
    	sm = ri.getSampleModel();
    	cm = ri.getColorModel();
    	width = ri.getWidth();
   		 height = ri.getHeight();
    }
    else if (bi != null) {
    	sm = bi.getSampleModel();
    	cm = bi.getColorModel();
    	width = bi.getWidth();
   		 height = bi.getHeight();
    }
    else {
    	// set defaults values?
    	// throw an exception???
    	return;
    }
    
    dataType = sm.getDataType();
        
    sampleModelClassName = sm.getClass().getName();
    colorModelClassName = cm.getClass().getName();
    
    
    // sample model is for a tile, not the whole image
    // int height = sm.getHeight();
    // int width = sm.getWidth();
    
    bands = sm.getNumBands();
    sampleSize = sm.getSampleSize();
    b0size = sm.getSampleSize(0);
    int elements = sm.getNumDataElements();
    if (debug) {
    	System.out.println("height="+height+"  width="+width+"  bands="+bands );
    	System.out.println("dataElements="+elements+"  b0size="+b0size   );
    	for (int i=0 ; i< sampleSize.length ; i++) {
        	System.out.println(" sampleSize["+i+"]="+sampleSize[i]);
    	}
    }
    
    } // getValues
    
    
    /**
    * This is the class each format MUST overide to construct a Document
    * with Elements specific to that format
    **/
    public void buildDom () 
    {
    if (debug) System.out.println("--------------- buildDom -------- ");
        
    String formatStr = "NONE";
    
    // each format will have a tet like this to get a format descriptor for their format
    if (dataType == DataBuffer.TYPE_BYTE) formatStr = "TYPE_BYTE";
    if (dataType == DataBuffer.TYPE_SHORT) formatStr = "TYPE_SHORT";
    if (dataType == DataBuffer.TYPE_USHORT) formatStr = "TYPE_USHORT"; // ??? IS THIS CORRECT
    if (dataType == DataBuffer.TYPE_INT) formatStr = "TYPE_INT";
    if (dataType == DataBuffer.TYPE_FLOAT) formatStr = "TYPE_FLOAT";
    if (dataType == DataBuffer.TYPE_DOUBLE) formatStr = "TYPE_DOUBLE";
    
    
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
          
          /***
          // for now till DOMutils is in the right place
          _document = new org.apache.xerces.dom.DocumentImpl();
          
          Class c = _document.getClass();
          _documentName = c.getName();
          
          Element documentNameNode = (Element) _document.createElement(_documentName); 
          _document.appendChild (documentNameNode);
          *************/
          // ----------------------------
          
          Element root = (Element) _document.createElement(nativeMetadataFormatName); 
          
          // documentNameNo.appendChild(root);
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
	      value = ""+1; // dataType.toString();
	      text = (Text) _document.createTextNode(value);
	      item.appendChild(text);
          system.appendChild(item);
          
          item = (Element) _document.createElement("formatString"); 
	      text = (Text) _document.createTextNode(formatStr);
	      item.appendChild(text);
          system.appendChild(item);
          
          item = (Element) _document.createElement("width"); 
	      text = (Text) _document.createTextNode(Integer.toString(width) );
	      item.appendChild(text);
          system.appendChild(item);
       
          item = (Element) _document.createElement("height"); 
	      text = (Text) _document.createTextNode(Integer.toString(height) ); // Integer.toString(height);
	      item.appendChild(text);
          system.appendChild(item);
          
          item = (Element) _document.createElement("bands"); 
	      text = (Text) _document.createTextNode(Integer.toString(bands));
	      item.appendChild(text);
          system.appendChild(item);
          
          for (int i=0 ; i < sampleSize.length ; i++) {
            item = (Element) _document.createElement("sampleSize"); 
	        item.setAttribute("band", Integer.toString(i));
	        text = (Text) _document.createTextNode( Integer.toString(sampleSize[i]) );
	        item.appendChild(text);
            system.appendChild(item);
          }
          
          item = (Element) _document.createElement("sampleModelName"); 
	      text = (Text) _document.createTextNode(sampleModelClassName);
	      item.appendChild(text);
          system.appendChild(item);
          
          item = (Element) _document.createElement("colorModelName"); 
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