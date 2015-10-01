/*
 * @(#)VicarMetadata.java	1.0 00/08/30
 *
 
 */

package jpl.mipl.io.plugins;

import java.awt.image.ColorModel;
import java.awt.image.IndexColorModel;
import java.awt.image.SampleModel;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.Vector;
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

/**
 * @version 1.0 
 * jdk1.4 compatable version
 * <br>
 * There are a few ways to use this class. If the image was read from a 
 * The VicarReader then it will have a VicarLabel Object available.
 * Once the VicarLabel is set in this VicarMetadata Object then requests
 * for trees will be servered by creating the requested tree with private methods
 * using the VicarLabel as input. <br>
 * If there is no VicarLabel set no trees will be returned unless they have been 
 * externally created and then set.
 * a native or common tree can be externally created and then set.
 *<p>
 * TO DO: <br>
 * 2 possible ways to handle output by the writer.<br>
 * 1) Use an XSL transform on the tree to format the stream of bytes written
 * to the image file as its header.<br>
 * 2) Have a method which will convert a tree (naitive and common) to a VicarLabel
 * Currently the writer passes a VicarLabel object to the VicarIO routines and it 
 * handles the writing/formating of the label. <p>
 * In the transcoder world we should have a label writer which combines the tree and 
 * an XSL script to produce the label output stream.
 * The transcoder may have used an XSL script to modify the DOM tree before it is handed 
 * to the tree -> VicarLabel converter.
 */
public class VicarMetadata extends IIOMetadata implements Cloneable {

    // package scope
    public static final String 
        dtd = "vicar_label.dtd";
        
        boolean debug = false;
		// boolean debug = true ;
        
        Vector fromXmlErrorList = null;
        
    public static final String 
        nativeImageMetadataFormatName = "VICAR_LABEL";  
        // nativeMetadataFormatName = "jpl.mipl.io.plugins.vicar.vicarimage_1.0";
        // this uses a DOM (currently xerces)
        // do we need one like this?? or is the native the DOM one
     // or is native really the common one ???
     // org.apache.xerces.dom.DocumentImpl
    //     nativeImageMetadataFormatName = "jpl.mipl.io.plugins.vicarimage";    
        // this isn't defined yet - this is a placeholder
        
        public static final String 
        nativeImageMetadataFormatClassName = "jpl.mipl.io.plugins.VicarMetadataFormat";
        
    //    nativeMetadataFormatName = "VICAR_LABEL"; // FOR TESTING     TO BE CONSISTENT 
        // WITH THE TEST XSL SCRIPT AND PERL vic2xml script
        // nativeMetadataFormatName = "jpl.mipl.io.plugins.vicar.vicar_label";
        
     // this one uses IIOMetadata to hold all the data
     public static final String commonMetadataFormatName = "com.sun.imageio_1.0";
     
     public static final String nativeStreamMetadataFormatName ="";    
     public static final String nativeStreamMetadataFormatClassName ="";    
     
     // do we need one like this?? or is the native the DOM one
     // or is native really the common one ???
     // org.apache.xerces.dom.DocumentImpl
     // public static final String commonMetadataFormatName = "com.sun.imageio_1.0";

    // public static final String commonMetadataFormatName="xml";// or default or DOM ??
    public static final String[] metadataFormatNames = {
        nativeImageMetadataFormatName, commonMetadataFormatName
    };
    
    // we will hold our "native" metadata in a simple DOM Document object
    // since Document descends from Node we can return it as a Node with no problem
    org.w3c.dom.Document nativeDoc;
    
    IIOMetadataNode commonNode = null;
    
    VicarBinaryLinePrefix vicarBinaryLinePrefix = null; 
    
	VicarBinaryHeader vicarBinaryHeader = null;
	
	// these values are only valid for the image read from file
	// these are to used by a PDS detached label writer
	int record_length = 0; // vicar recsize
	int front_label_size = 0; // 
	
	 // PDS label needs to know how many records are in the vicar file
	int fileRecordCount = 0;
	
	// calculated _front_label_size / record_length
	int labelRecordCount = 0;
    
    // our real "native" metadata format is a VicarLabel object created by the reader
    protected VicarLabel vicarLabel;
    // protected VicarLabelSet _system;
    // protected VicarLabelCategory _property;
    // protected VicarLabelCategory _history;
    
    // use to determine the format of the input data file
    int  vicarPixelSize = 0;
    String vicarFormat = null; // PDS 
    String vicarIntFmt = null;
    String vicarRealFmt = null;
    String vicarOrg = null; // PDS BAND_STORAGE_TYPE
    
    
    // how to set default controller ???
    /** from IIOMetadata we inherit these fields:
    
    IIOMetadataController controller;
    IIOMetadataController defaultController;
    // setDefaultController() ???
    **/

    
    

 public VicarMetadata() {
         super(false,
        nativeImageMetadataFormatName, 
        nativeImageMetadataFormatClassName,
        // metadataFormatNames,
        null, null);
    }
    
 public VicarMetadata(IIOMetadata metadata) {
        
         super(false,
        nativeImageMetadataFormatName, 
        nativeImageMetadataFormatClassName,
        // metadataFormatNames,
        null, null);
        
        
        // TODO -- implement check node name ?? before allowing ??
        setFromTree(commonMetadataFormatName, (Node) metadata);
        
    }
    
    public VicarMetadata(Document doc) {
        
         super(false,
        nativeImageMetadataFormatName, 
        nativeImageMetadataFormatClassName,
        // metadataFormatNames,
        null, null);
        
		if (debug) { System.out.println("**** VicarMetadata Document *************"); }
		
        fromXmlErrorList = new Vector();
        vicarLabel = new VicarLabel();
        
        Element el = doc.getDocumentElement();
        vicarLabel.fromXML(el, fromXmlErrorList );
        
		if (debug) { 
        	printErrorList(fromXmlErrorList);
		}
        
        
        // TODO -- implement check node name ?? before allowing ??
		setVicarLabel(vicarLabel);
        // setFromTree(nativeMetadataFormatName, doc);
        // instead do nativeDoc = null;
        // getNativeTree() / this will create the doc from the vicar label ???
        // or 
        
    }
    
   public VicarMetadata(VicarLabel vl) {
   	 super(false,
        nativeImageMetadataFormatName, 
        nativeImageMetadataFormatClassName,
        // metadataFormatNames,
        null, null);
        
        vicarLabel = vl;
    }
    
	public void initialize(ImageTypeSpecifier imageType) {
        ColorModel colorModel = imageType.getColorModel();
        SampleModel sampleModel = imageType.getSampleModel();
       
	}

	
	public void printErrorList(Vector fromXmlErrorList) {
		int len = fromXmlErrorList.size();
		System.out.println("******************************");
		System.out.println("VicarMetadata.printErrorList length="+len);
		for (int i=0 ; i< len ; i++) {
			
			System.out.println(i+") "+fromXmlErrorList.elementAt(i));
		}
		System.out.println("******************************");
	}
	
	public boolean isReadOnly() {
        return false;
    }
    
	public void setDebug( boolean d) {
			debug = d;
		}
   
	// int record_length = 0; // vicar recsize
	// int front_label_size = 0; // 
	
	// calculated front_label_size / record_length
	// int labelRecordCount = 0;
	
	/**
	 * returns the label record count for the vicar label
	 * in the input image file. It is calculated from the values
	 * front_label_size / record_length
	 * that's why there is no setter
	 */
	public int getLabelRecordCount() {
		return front_label_size / record_length ;
	}
	
	/**
	 * returns the label record count for the vicar label
	 * in the input image file. It is calculated from the values
	 * front_label_size / record_length
	 */
	public void setFileRecordCount(int ct) {
		fileRecordCount = ct;
	}
	/**
	 * returns the label record count for the vicar label
	 * in the input image file. It is calculated from the values
	 * front_label_size / record_length
	 */
	public int getFileRecordCount() {
		return fileRecordCount ;
	}
	/**
	 * set the value for the vicar image label record length (recsize)
	 * Will be set by VicarInputFile when this metadata object is created
	 * @param len
	 */
	public void setRecord_length(int len) {
		record_length = len;
	}
	/**
	 * get the value for the vicar image label record length in bytes (recsize)
	 * may be useful for creating PDS detached labels
	 * @return
	 */
	public int getRecord_length() {
		return record_length ;
	}
	
	/**
	 * set the value for the vicar image label size in bytes
	 * Will be set by VicarInputFile when this metadata object is created
	 * @param size
	 */
	public void setFront_label_size(int size) {
		front_label_size = size;
	}
	/**
	 * get the value for the vicar image label size in bytes
	 * may be useful for creating PDS detached labels
	 * @return
	 */
	public int getFront_label_size() {
		return front_label_size ;
	}
	
	
	
	/**
	 * set the value for the vicar image label size in bytes
	 * Will be set by VicarInputFile when this metadata object is created
	 * @param size
	 */
	public void setVicarPixelSize(int size) {
		vicarPixelSize = size;
	}
	/**
	 * get the value for the vicar image label size in bytes
	 * may be useful for creating PDS detached labels
	 * @return
	 */
	public int getVicarPixelSize() {
		return vicarPixelSize ;
	}
	
	
	/**
	 * set the value for the vicar image label size in bytes
	 * Will be set by VicarInputFile when this metadata object is created
	 * @param size
	 */
	public void setVicarFormat(String format) {
		vicarFormat = format;
	}
	/**
	 * get the value for the vicar image label size in bytes
	 * may be useful for creating PDS detached labels
	 * @return
	 */
	public String getVicarFormat() {
		return vicarFormat ;
	}
	
	/**
	 * set the value for the vicar image label size in bytes
	 * Will be set by VicarInputFile when this metadata object is created
	 * @param size
	 */
	public void setVicarIntFmt(String format) {
		vicarIntFmt = format;
	}
	/**
	 * get the value for the vicar image label size in bytes
	 * may be useful for creating PDS detached labels
	 * @return
	 */
	public String getVicarIntFmt() {
		return vicarIntFmt ;
	}
	
	/**
	 * set the value for the vicar image label size in bytes
	 * Will be set by VicarInputFile when this metadata object is created
	 * @param size
	 */
	public void setVicarRealFmt(String format) {
		vicarRealFmt = format;
	}
	/**
	 * get the value for the vicar image label size in bytes
	 * may be useful for creating PDS detached labels
	 * @return
	 */
	public String getVicarRealFmt() {
		return vicarRealFmt ;
	}
	
	
	
	
 // Deep clone
    public Object clone() {
        VicarMetadata metadata = null;
        try {
            metadata = (VicarMetadata)super.clone();
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
            // return (IIOMetadataFormat) nativeDoc;
        } else if (formatName.equals(commonMetadataFormatName)) {
            return null;
        } else {
            throw new IllegalArgumentException("Not a recognized format!");
        }
    }

    public Node getAsTree(String formatName) {
        // commonMetadataFormatName
        if (formatName.equals(nativeImageMetadataFormatName)) {
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
        if (nativeDoc == null ) {
            if (vicarLabel != null) {
            	if (debug) {
                	System.out.println("VicarMetadata.getNativeTree() calling VicarLabelToDOM");
                	System.out.println("nativeMetadataFormatName "+nativeMetadataFormatName);
            	}
                VicarLabelToDOM vl2DOM = new VicarLabelToDOM(vicarLabel, nativeMetadataFormatName);
                // vl2DOM.setNativeMetadataFormatName( nativeMetadataFormatName );
        
                nativeDoc = vl2DOM.getDocument();
            }
            else {
            	if (debug) {
                	System.out.println("vicarLabel is null - It must be set using setVicarLabel() before this call");
                	System.out.println("OR nativeDoc must be set using setFromTree()");
            	}
            }
        }
        
        return nativeDoc; // Document is a subclass of Node
    }
  
  
  /**
   * getCommonTree
   *
   * create or return the tree
   * the common tree is a IIOMetadataNode
   *
   * To do this right I think I also need to supply a
   * IIOMetadataFormat for my IIOMetadata that I return here
   */
    private Node getCommonTree() {
        
        // if nativeDoc is null go create it now
        if (commonNode == null ) {
            if (vicarLabel != null) {
                
                VicarLabelToIIOMetadata vl2IIOM = new VicarLabelToIIOMetadata(vicarLabel);
        
                commonNode = vl2IIOM.getRoot();
                
                /**
                // VicarLabelToDOM vl2DOM = new VicarLabelToDOM(vicarLabel, nativeMetadataFormatName);
                // vl2DOM.setNativeMetadataFormatName( nativeMetadataFormatName );
        
                // nativeDoc = vl2DOM.getDocument();
                ***/
            }
            else {
            	if (debug) {
                System.out.println("vicarLabel is null - It must be set using setVicarLabel() before this call");
                System.out.println("OR nativeDoc must be set using setFromTree()");
            	}
            }
        }
        
        return commonNode; // IIOMetadataNode is an implementation of Node
    }
    
    
  /**
  * this is the first try for "native" metadtata, I don't think it really fits with the intent ???
  * should I enclose the VicarLabel Object as a node of a tree instead?? Maybe in nativeDoc ??
  
  **/
    public void setVicarLabel(VicarLabel vl) {
        vicarLabel = vl;
        // when getNativeTree() is called a Document will be created from the VicarLabel
        // the VicarLabel is the TRUE "native" metadata object which can be obtained directly
        // for use by a VicarImageWriter
        // VicarLabelToDOM converts a VicarLabel to a DOM Document object
        // for the transcoder part I'll want a DOMtoVicarLabel so the VicarImageWriter can use
        // it to write the output image's label
    }
    
  /**
  * Maybe getAsTree("VicarLabel")
  * returns a tree with one node whose value is the VicarLabel Object
  **/
   public VicarLabel getVicarLabel() {
        return vicarLabel; 
   }
   
   /**
    * The VicarBinaryLinePrefix is an Object which holds all the data of the image
    * It is collected by the reader or set somewhere else. The writer has the option of 
    * putting the prefix data into the output file.
    * A null VicarBinaryLinePrefix indicates a lack of prefix data
    * @return
    */
   public VicarBinaryLinePrefix getVicarBinaryLinePrefix() {
   	return vicarBinaryLinePrefix;
   }
   
   public void setVicarBinaryLinePrefix( VicarBinaryLinePrefix vpf) {
   	   if (debug) System.out.println("VicatIMetadata.setVicarBinaryLinePrefix");
	   vicarBinaryLinePrefix = vpf;
	  }
	 
	 
	/**
		* The VicarBinaryLHeader is an Object which holds the Binary Header data of the image
		* It is collected by the reader or set somewhere else. The writer has the option of 
		* putting the header data into the output file.
		* A null VicarBinaryHeaderx indicates a lack of binary header data.
		* The binary header is a special case. MOST vicar images do NOT have any 
		* binary header.
		* @return vicarBinaryHeader
		*/
	   public VicarBinaryHeader getVicarBinaryHeader() {
		return vicarBinaryHeader;
	   }
   
	   public void setVicarBinaryHeader( VicarBinaryHeader vbh) {
		   if (debug) System.out.println("VicarMetadata.setVicarBinaryLHeader");
		   vicarBinaryHeader = vbh;
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
    * for VicarMetadata to hold.
    *
    *************************/
    public void setFromTree(String formatName, Node root) {
        if (debug) System.out.println("VicarMetadata.setFromTree() "+formatName);
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
     
     
     
     
     
     
    
}
