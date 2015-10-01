/*
 * @(#)PDSMetadata.java	1.0 00/08/30
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
 * 
 * <br>
 * The metdata object is needed by an IIOImage to be a container for the metadata
 * for an image. The reader will create the Document and put it in here.
 * If the "common" IIOMetadataNode verdion of the metedata is requested it will then be built
 * using the native Document as the input. 
 * VicarBinaryLinePrefix has been added to support HRSC images which do contain Binary prefix data.
 * The metadata is used to transfer this data since prefix data is normally not read for an
 * image to be displayed.
 */
public class PDSMetadata extends IIOMetadata implements Cloneable {

    // package scope
    public static final String 
        dtd = "pds_label.dtd";
        
    public static final String 
        nativeStreamMetadataFormatName = ""; // "jpl.mipl.io.plugins.vicar.pdsimage";
        
    public static final String 
        nativeStreamMetadataFormatClassName = "" ; // "jpl.mipl.io.plugins.vicar.pdsimage";    
        
    public static final String nativeImageMetadataFormatName = "PDS_LABEL";    
      //   nativeImageMetadataFormatName = "jpl.mipl.io.plugins.pdsimage";    
        // this isn't defined yet - this is a placeholder
        
        public static final String 
        nativeImageMetadataFormatClassName = "jpl.mipl.io.plugins.PDSMetadataFormat";
        
        // this uses a DOM (currently xerces)
        // do we need one like this?? or is the native the DOM one
     // or is native really the common one ???
     // org.apache.xerces.dom.DocumentImpl
       
        public static final String 
        vicarImageMetadataFormatClassName = "jpl.mipl.io.plugins.VicarMetadataFormat";
     
        
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
    }; // vicarImageMetadataFormatClassName
    
    // we will hold our "native" metadata in a simple DOM Document object
    // since Document descends from Node we can return it as a Node with no problem
    org.w3c.dom.Document nativeDoc = null;
    
    // this will be created ON DEMAND from the nativeDoc
    IIOMetadataNode commonNode = null;
    
    // used to hold any prefix data. It is kept in the metadata since it isn't part of a displayable image
    VicarBinaryLinePrefix vicarBinaryLinePrefix = null;
    
    VicarBinaryHeader vicarBinaryHeader = null;
    
    // boolean debug = true;
    boolean debug = false;
    
    // flag which could be used to control if FILE_RECORDS is use3d in a detached label
    boolean delete_FILE_RECORDS = false;
 	
    // can be used by a detached_label writer to know where the data starts in the input data fie
    int front_label_size = 0; 
    
    /** from IIOMetadata we inherit these fields:
    
    IIOMetadataController controller;
    IIOMetadataController defaultController;
    // setDefaultController() ???
    **/

    
    

 public PDSMetadata() {
        super(false,
        nativeImageMetadataFormatName, 
        nativeImageMetadataFormatClassName,
        // metadataFormatNames,
        null, null);
    }
    
 public PDSMetadata(IIOMetadata metadata) {
        
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
    
 	public PDSMetadata(Document doc) {
        
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
    
    public void setDelete_FILE_RECORDS(boolean d) {
 		
 		delete_FILE_RECORDS = d;
     	if (debug) {
     		System.out.println("PDSImageWriter.setDelete_FILE_RECORDS("+d+") ");
     	}
 	}
 	
 	public boolean getDelete_FILE_RECORDS(boolean d) {
 		
 		return delete_FILE_RECORDS;
 	}
    
	/**
		* The VicarBinaryLinePrefix is an Object which holds all the data of the image
		* It is collected by the reader or set from somewhere else. The writer has the option of 
		* putting the prefix data into the output file.
		* A null VicarBinaryLinePrefix indicates a lack of prefix data
		* @return
		*/
	   public VicarBinaryLinePrefix getVicarBinaryLinePrefix() {
		return vicarBinaryLinePrefix;
	   }
   
	   public void setVicarBinaryLinePrefix( VicarBinaryLinePrefix vpf) {
		   vicarBinaryLinePrefix = vpf;
		  }
	
	/**
			* The VicarBinaryLinePrefix is an Object which holds all the data of the image
			* It is collected by the reader or set from somewhere else. The writer has the option of 
			* putting the prefix data into the output file.
			* A null VicarBinaryLinePrefix indicates a lack of prefix data
			* @return
			*/
		   public VicarBinaryHeader getVicarBinaryHeader() {
			return vicarBinaryHeader;
		   }
   
		   public void setVicarBinaryHeader( VicarBinaryHeader vbh) {
			   vicarBinaryHeader = vbh;
			  }
		  	  
		  
 // Deep clone
    public Object clone() {
        PDSMetadata metadata = null;
        try {
            metadata = (PDSMetadata)super.clone();
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
    	if (debug) System.out.println("PDSMetadata.getAsTree() "+formatName);
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
        
    	if (debug) System.out.println("PDSMetadata.getNativeTree() "+nativeDoc);
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
                System.out.println("no PDS Document set");
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
    * for PDSMetadata to hold.
    *
    *************************/
    public void setFromTree(String formatName, Node root) {
        if (debug) {
        	System.out.println("PDSMetadata.setFromTree() "+formatName+" *************************************");
        	System.out.println("PDSMetadata.setFromTree() root "+root+" *************************************");
        }
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
     
     /**
 	 * set the value for the image label(s) size in bytes
 	 * Will be set by PdsInputFile when this metadata object is created
 	 * @param size
 	 */
 	public void setFront_label_size(int size) {
 		front_label_size = size;
 	}
 	/**
 	 * get the value for the input image label size in bytes
 	 * may be useful for creating PDS detached labels
 	 * @return
 	 */
 	public int getFront_label_size() {
 		return front_label_size ;
 	}
     
     
     
    
}
