/*
 * @(#)FITSMetadata.java	1.0 02/01/17
 *
 
 */

package jpl.mipl.io.plugins;

import java.awt.image.ColorModel;
import java.awt.image.IndexColorModel;
import java.awt.image.SampleModel;
import java.util.ArrayList;
import java.util.Iterator;
import javax.imageio.ImageTypeSpecifier;
import javax.imageio.metadata.IIOInvalidTreeException;
import javax.imageio.metadata.IIOMetadata;
import javax.imageio.metadata.IIOMetadataController;
import javax.imageio.metadata.IIOMetadataFormat;
import javax.imageio.metadata.IIOMetadataNode;
import org.w3c.dom.*;

// import VicarIO stuff
// VicarInputFile  SystemLabel
import jpl.mipl.io.streams.*;
import jpl.mipl.io.vicar.*;
import jpl.mipl.io.util.*;

/**
 * @version 0.5 
 * EA2
 * <br>
 * The metdata object is needed by an IIOImage to be a container for the metadata
 * for an image. The reader will create the Document and put it in here.
 * If the "common" IIOMetadataNode verdion of the metedata is requested it will then be built
 * using the native Document as the input. 
 */
public class FITSMetadata extends IIOMetadata implements Cloneable {

    // package scope
    public static final String 
        dtd = "fits_label.dtd";
        
    public static final String 
        nativeStreamMetadataFormatName = ""; // "jpl.mipl.io.plugins.vicar.pdsimage";
        
    public static final String 
        nativeStreamMetadataFormatClassName = "" ; // "jpl.mipl.io.plugins.vicar.pdsimage";    
        
    public static final String nativeImageMetadataFormatName = "FITS_LABEL";    
      //   nativeImageMetadataFormatName = "jpl.mipl.io.plugins.pdsimage";    
        // this isn't defined yet - this is a placeholder
        
        public static final String 
        nativeImageMetadataFormatClassName = "jpl.mipl.io.plugins.FITSMetadataFormat";
        
        // this uses a DOM (currently xerces)
        // do we need one like this?? or is the native the DOM one
     // or is native really the common one ???
     // org.apache.xerces.dom.DocumentImpl
     
        
    //    nativeMetadataFormatName = "VICAR_LABEL"; // FOR TESTING     TO BE CONSISTENT 
        // WITH THE TEST XSL SCRIPT AND PERL vic2xml script
        // nativeMetadataFormatName = "jpl.mipl.io.plugins.vicar.vicar_label";
        
     // this one uses IIOMetadata to hold all the data
     public static final String commonMetadataFormatName = "com.sun.imageio_1.0";
     
     // do we need one like this?? or is the native the DOM one
     // or is native really the common one ???
     // org.apache.xerces.dom.DocumentImpl
     // public static final String commonMetadataFormatName = "com.sun.imageio_1.0";

    // public static final String commonMetadataFormatName="xml";// or default or DOM ??
    public static final String[] metadataFormatNames = {
        nativeStreamMetadataFormatName, nativeImageMetadataFormatName, commonMetadataFormatName
    };
    
    // we will hold our "native" metadata in a simple DOM Document object
    // since Document descends from Node we can return it as a Node with no problem
    org.w3c.dom.Document nativeDoc = null;
    
    // this will be created ON DEMAND from the nativeDoc
    IIOMetadataNode commonNode = null;
    
    boolean debug = false;
    
    
    /** from IIOMetadata we inherit these fields:
    
    IIOMetadataController controller;
    IIOMetadataController defaultController;
    // setDefaultController() ???
    **/

    
    

 public FITSMetadata() {
        super(false,
        nativeImageMetadataFormatName, 
        nativeImageMetadataFormatClassName,
        // metadataFormatNames,
        null, null);
    }
    
 public FITSMetadata(IIOMetadata metadata) {
        
        super(false,
        nativeImageMetadataFormatName,
         nativeImageMetadataFormatClassName,
         null, null);
         
         /****
         // use values from the passed metadata??
         super(metadata.isStandardMetadataFormatSupported(),
         metadata.getNativeMetadataFormatName(),
         metadata.getMetadataFormat(metadata.getNativeMetadataFormatName()).getClass().getName();
         metadata.getExtraMetadataFormatNames(),
         null);
         ***/
        
        // TODO -- implement check node name ?? before allowing ??
        setFromTree(commonMetadataFormatName, (Node) metadata);
        
    }
    
 	public FITSMetadata(Document doc) {
        
        super(false,
        nativeImageMetadataFormatName, 
        nativeImageMetadataFormatClassName,
        // metadataFormatNames,
        null, null);
        
        // TODO -- implement check node name ?? before allowing ??
        setFromTree(nativeMetadataFormatName, doc);
        
    }
    
   
    
	public void initialize(ImageTypeSpecifier imageType) {
        ColorModel colorModel = imageType.getColorModel();
        SampleModel sampleModel = imageType.getSampleModel();
       
	}

	public boolean isReadOnly() {
        return false;
    }
    
    public void setDebug(boolean d) {
    	debug = d;
    }
    
 // Deep clone
    public Object clone() {
        FITSMetadata metadata = null;
        try {
            metadata = (FITSMetadata)super.clone();
        } catch (CloneNotSupportedException e) {
            return null;
        }

        return metadata;
    }

// this one needs to work properly
// Build IIOMetadataFormat classes for my formats
    public IIOMetadataFormat getMetadataFormat(String formatName) {
        if (formatName.equals(nativeMetadataFormatName)) {
            return null;
            // return (IIOMetadataFormat) nativeFormat;
        } else if (formatName.equals(commonMetadataFormatName)) {
            
            return null;
        } else {
            throw new IllegalArgumentException("Not a recognized format!");
        }
    }

    public Node getAsTree(String formatName) {
        // commonMetadataFormatName
        if (formatName.equals(nativeMetadataFormatName)) {
            return getNativeTree();
        } else if (formatName.equals(commonMetadataFormatName)) {
            return getCommonTree();
        } else {
            throw new IllegalArgumentException("Not a recognized format!");
        }  
    }
   
   /**
   * getNativeTree
   *
   * create or return the tree
   * the native tree is a Document
   */
    private Node getNativeTree() {
        
        // if nativeDoc is null go create it now
        // if (nativeDoc == null ) {
        
        
        return nativeDoc; // Document is a subclass of Node
    }
  
  
  /**
   * getCommonTree
   *
   * create or return the tree
   * the common tree is a IIOMetadataNode
   */
    private Node getCommonTree() {
        
        // if nativeDoc is null go create it now
        if (commonNode == null ) {
            if (nativeDoc != null) {
                
                DOMtoIIOMetadata dom2IIOM = new DOMtoIIOMetadata( nativeDoc);
                IIOMetadataNode commonNode = dom2IIOM.getRootIIONode();
                // have something like this which converts Document to IIOMetadata
                // generic converter ???
                // DocumentToIIOMetadataNode
                // VicarLabelToIIOMetadata vl2IIOM = new VicarLabelToIIOMetadata(vicarLabel);
        
                // commonNode = vl2IIOM.getRoot();
                
                /**
                // VicarLabelToDOM vl2DOM = new VicarLabelToDOM(vicarLabel, nativeMetadataFormatName);
                // vl2DOM.setNativeMetadataFormatName( nativeMetadataFormatName );
        
                // nativeDoc = vl2DOM.getDocument();
                ***/
            }
            else {
                System.out.println("no FITS Document set");
                System.out.println("OR nativeDoc must be set using setFromTree()");
            }
        }
        
        return commonNode; // IIOMetadataNode is an implementation of Node
    }
    
    
  
  
   
    // Shorthand for throwing an IIOInvalidTreeException
    private void fatal(Node node, String reason)
        throws IIOInvalidTreeException {
        throw new IIOInvalidTreeException(reason, node);
    }
   
    public void mergeTree(String formatName, Node root)
        throws IIOInvalidTreeException {
        if (formatName.equals(nativeMetadataFormatName)) {
            if (root == null) {
                throw new IllegalArgumentException("root == null!");
            }
            mergeNativeTree(root);
        } else {
            throw new IllegalArgumentException("Not a recognized format!");
        }
    }

    private void mergeNativeTree(Node root)
    
        // this seems to want the root node to be the String in 
        // nativeMetadataFormatName
        // we'll try that later
        // since we aren't doing anything with merge yet....
        throws IIOInvalidTreeException {
        Node node = root;
        // also check for commomMetadataFormatName
        // check that I do this stuff correctly
        if (!node.getNodeName().equals(nativeMetadataFormatName)) {
            fatal(node, "Root must be " + nativeMetadataFormatName);
        }
        
        node = node.getFirstChild();
        while (node != null) {
            String name = node.getNodeName();
        }
     }
    
    
    /************************
    *
    * The Tree is created elsewhere. It is passed in here 
    * for FITSMetadata to hold.
    *
    *************************/
    public void setFromTree(String formatName, Node root) {
        if (debug) System.out.println("FITSMetadata.setFromTree() "+formatName);
        // should we also check:
        // root instanceof Document 
        // root instanceof IIOMetadataNode
        if (formatName.equals(nativeMetadataFormatName)) {
            nativeDoc = (Document) root;
        } else if (formatName.equals(commonMetadataFormatName)) {
        //  do something with the "common" metaData
            commonNode = (IIOMetadataNode) root;
        } else {
            throw new IllegalArgumentException("Not a recognized format!");
        }
        
        
    }
    
    
     // Reset all instance variables to their initial state
     public void reset() {
        // set any default values
        // setDefaultController() ???
     }
     
     // controller methods
     public boolean activateController() {
     	return false;
     }
     
     // the DomEcho4 may become a controller ???
     /* there is a problem here
     public IIOMetadataController getController() {
        return controller;
     }
     
     public IIOMetadataController getDefaultController() {
        return defaultController;
     }
     
     public booelean hasController() {
        if (controller != null) {
            return true;
        }
        else {
            return false;
        }
     }
     **/
     
     public void setController(IIOMetadataController aController) {
        controller = aController;
     }
     
     public IIOMetadataController getController() {
        return controller ;
     }
     
     
     
     
     
    
}
