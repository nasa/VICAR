/*
 * Metadata object for Magellan Resamp format.
 *
 * Based on PdsMetadata.java
 *
 * @author Bob Deen, JPL
 */

package jpl.mipl.io.plugins;

import javax.imageio.metadata.*;
import org.w3c.dom.*;
import java.util.*;

import jpl.mipl.io.vicar.*;

/**
 * @version 0.1
 * 
 * Metadata object for Magellan Resamp format.
 * <p>
 * The metdata object is needed by an IIOImage to be a container for the
 * metadata for an image. The reader will create the Document and put it in
 * here.  If the "common" IIOMetadataNode version of the metedata is requested
 * it will then be built using the native Document as the input. 
 * <p>
 * The underlying MgnResampHeader object can be obtained directly from this
 * class.
 */
public class MgnResampMetadata extends IIOMetadata implements Cloneable {

    public static final String 
        nativeStreamMetadataFormatName = "";
    public static final String 
        nativeStreamMetadataFormatClassName = "";
    public static final String
       nativeImageMetadataFormatName = "jpl.mipl.io.vicar.MgnResampMetadata_1.0";
    public static final String 
      nativeImageMetadataFormatClassName = "jpl.mipl.io.vicar.MgnResampMetadata";

    // this one uses IIOMetadata to hold all the data
    public static final String commonMetadataFormatName = "com.sun.imageio_1.0";

    public static final String[] metadataFormatNames = {
        nativeStreamMetadataFormatName,
	nativeImageMetadataFormatName,
	commonMetadataFormatName
    };

    // we will hold our "native" metadata in a simple DOM Document object
    // since Document descends from Node we can return it as a Node

    Document _nativeDoc = null;

    // this will be created ON DEMAND from the _nativeDoc
    IIOMetadataNode _commonNode = null;

    // Store the header in case the client wants to get at it
    MgnResampHeader _resamp_header = null;

    boolean debug = true;

    public MgnResampMetadata() {
	super(false,
		nativeImageMetadataFormatName, 
		nativeImageMetadataFormatClassName,
		null, null);
    }
    
    public MgnResampMetadata(IIOMetadata metadata) {
	super(false,
		nativeImageMetadataFormatName,
		nativeImageMetadataFormatClassName,
		null, null);

        setFromTree(commonMetadataFormatName, (Node) metadata);
    }

    public MgnResampMetadata(Document doc, MgnResampHeader hdr) {
	super(false,
		nativeImageMetadataFormatName, 
		nativeImageMetadataFormatClassName,
		null, null);

        setFromTree(nativeMetadataFormatName, doc);
	_resamp_header = hdr;
    }
    
   
    public boolean isReadOnly() {
        return false;
    }

    public void setDebug(boolean d) {
    	debug = d;
    }

/** Deep clone */
    public Object clone() {
        MgnResampMetadata metadata = null;
        try {
            metadata = (MgnResampMetadata)super.clone();
        } catch (CloneNotSupportedException e) {
            return null;
        }

        return metadata;
    }

/**
 * Build IIOMetadataFormat classes for my formats
 */
    public IIOMetadataFormat getMetadataFormat(String formatName) {
        if (formatName.equals(nativeMetadataFormatName)) {
            return null;
        } else if (formatName.equals(commonMetadataFormatName)) {
            return null;
        } else {
            throw new IllegalArgumentException("Not a recognized format!");
        }
    }

/**
 * Returns an XML DOM Node object with the metadata in the specified format.
 */

    public Node getAsTree(String formatName) {
        if (formatName.equals(nativeMetadataFormatName)) {
            return getNativeTree();
        } else if (formatName.equals(commonMetadataFormatName)) {
            return getCommonTree();
        } else {
            throw new IllegalArgumentException("Not a recognized format!");
        }  
    }
   
/**
 * Return the "native" format tree
 */
    protected Node getNativeTree()
    {

        return _nativeDoc; // Document is a subclass of Node
    }

/**
 * Return the "common" format tree.  Probably much more should be done here...
 */
    protected Node getCommonTree()
    {
        if (_commonNode == null ) {
            if (_nativeDoc != null) {	// Create it from native

                DOMtoIIOMetadata dom2IIOM = new DOMtoIIOMetadata(_nativeDoc);
                IIOMetadataNode _commonNode = dom2IIOM.getRootIIONode();
            }
            else {
                System.out.println("no XML Document set");
                System.out.println("OR _nativeDoc must be set using setFromTree()");
            }
        }
        
        return _commonNode; // IIOMetadataNode is an implementation of Node
    }
  
/**
 * mergeTree() is non-functional.  If we need this capability, implement it.
 */
   
    public void mergeTree(String formatName, Node root)
        throws IIOInvalidTreeException
    {
        if (formatName.equals(nativeMetadataFormatName)) {
            if (root == null) {
                throw new IllegalArgumentException("root == null!");
            }
            mergeNativeTree(root);
        } else {
            throw new IllegalArgumentException("Not a recognized format!");
        }
    }

    protected void mergeNativeTree(Node root) throws IIOInvalidTreeException
    {
        Node node = root;
        if (!node.getNodeName().equals(nativeMetadataFormatName)) {
            throw new IIOInvalidTreeException(
		"Root must be " + nativeMetadataFormatName, node);
        }
        
        node = node.getFirstChild();
        while (node != null) {
            String name = node.getNodeName();
        }
     }

/************************
 * The Tree is created elsewhere. It is passed in here 
 * for MgnResampMetadata to hold.
 *
 */
    public void setFromTree(String formatName, Node root) {
        if (debug) System.out.println("MgnResampMetadata.setFromTree() "+formatName);

        if (formatName.equals(nativeMetadataFormatName)) {
            _nativeDoc = (Document) root;
        } else if (formatName.equals(commonMetadataFormatName)) {
            _commonNode = (IIOMetadataNode) root;
        } else {
            throw new IllegalArgumentException("Not a recognized format!");
        }

    }

/**
 * Reset all instance variables to their initial state
 */
     public void reset() {
	_nativeDoc = null;
	_commonNode = null;
     }

////////////////////////////////////////////////////////////////////////
// SPECIFIC METHODS FOR THIS CLASS - to get things specific to the Resamp
////////////////////////////////////////////////////////////////////////


/**
 * Get the MgnResampHeader list for this file.  It contains the same
 * information as the XML but as a simple object with get methods.
 */
    public MgnResampHeader getMgnResampHeader()
    {
	return _resamp_header;
    }

}

