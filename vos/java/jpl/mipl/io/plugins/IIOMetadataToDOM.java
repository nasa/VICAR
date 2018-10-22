/*
*
* @(#)IIOMetadataToDOM.java	1.0 00/12/15
 *
 * Steve Levoe
 * Jet Propulsion Laboratory
 * Multimission Image Processing Laboratory
 * 12-2000 ImageIO EA2 version
*
***************************************/
package jpl.mipl.io.plugins;

import org.w3c.dom.Document;
import org.w3c.dom.DOMException;
import org.w3c.dom.Element;
import org.w3c.dom.*;

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
 * This class builds a DOM Document from an IIOMetadata
 * object.<BR>
 * 
 * This will be called from inside a reader, writer or transcoder.
 * The Document created by this class is intended for use with 
 * the Apache group's xerces tools. <br>
 * In particular the tools can be used to modify/transform DOM 
 * using XSL and use output format processors to serialize them. <br>
 * The resultant Document can also be displayed in a graphical TreeViewer
 * java gui. <BR>
 * This may also be used to add more Metadata to an existing Document.<br>
 * Other implementations may use other libraries.
 * @version 0.5
 */
public class IIOMetadataToDOM {
    
    private boolean debug = false;
    
    private String _metadataFormatName = "Document"; 
    // use implentation class name ??
    // private String metadataFormatName = DocumentImpl.class.getName();
    // allow this to be changed with get/set 
    private IIOMetadataNode _iioMetadata = null;
    private Document _document = null;
    
    // keep track of any UserData Objects we are forced to ignore in this conversion
    int userObjectCount = 0;
    // keep some sort of iterator with the class of each Object
    // or even all the Objects?
    // let the user ask for the list, or a count
    // That way they know if they are missing something in this conversion
    
    // Constructor
    public IIOMetadataToDOM (IIOMetadataNode metadata) {
        // _iioMetadata = metadata;
        // _document = buildDocument(_iioMetadata);
        // or 
        setIIOMetadata (metadata);
    }
    
    /**
    * empty, user can add an existing document from a previous instantiation
    * Then more metadata can be added to the document
    **/
    public IIOMetadataToDOM (Document doc) {
        _document = doc;
    }
    
    /**
    * Adds Metadata to an existing document from a previous instantiation.
    **/
    public IIOMetadataToDOM (Document doc, IIOMetadataNode metadata) {
        // should check to see if this a proper Document (check name)
        String docName = getDocumentName();
        // System.out.println("docName "+docName);
        // System.out.println("doc "+doc.getNodeName());
        
        Node newRoot = doc.getDocumentElement();
        String inRootName = newRoot.getNodeName();
        // System.out.println("doc.root " + newRoot.getNodeName());
        
        if (inRootName.equals(docName) ) {
            // we can add IIOMetadata to this Document
            if (debug) System.out.println("The input is a proper Document implentation: "+docName);
            _document = doc;
            Node root = _document.getDocumentElement();
            addMetadata(root, metadata);
        }
        else {
        _document = null;
        }
        
    }
    
    /**
    * Allows the metadata to be changed.
    * Probably don't allow this, just let user creat a new one of these
    * instead.
    **/
    public void setIIOMetadata (IIOMetadataNode metadata) {
        _iioMetadata = metadata;
        _document = buildDocument(_iioMetadata);
    }
    
    public IIOMetadataNode getIIOMetadata() {
        return _iioMetadata;
    }
    
    public Document getDocument() {
        return _document;
    }
    
    public void setDocument(Document doc) {
        // check for proper Document class?
        // throw exception if it isn't correct
        _document = doc;
    }
    
    public void setDebug(boolean d) {
    	debug = d;
    }
    
    public void addNewMetadata(IIOMetadataNode metadata) {
        
        Node root = _document.getDocumentElement();
        addMetadata(root, metadata);
        
    }
    
    public String getMetadataFormatName() {
        return _metadataFormatName;
    }
    
    public  void setMetadataFormatName(String name) {
        _metadataFormatName = name;
    }
    
    /* use this to find out if the Document implemantation class is compatable
    * with the other XML processing classes ???
    public Object getDocumentClass() {
        return Document.class;
    }
    */
    
    
    private String getDocumentName() {
    	jpl.mipl.io.util.DOMutils domUtil = new jpl.mipl.io.util.DOMutils();
    	Document document = domUtil.getNewDocument();
        // Document document = new org.apache.xerces.dom.DocumentImpl();
        Class c = document.getClass();
        String DocumentName = c.getName();
        return DocumentName;
    }
    
    public Document buildDocument (IIOMetadataNode metadata) {
        
        /* this way of creating a new Document isn't compatable
        * with the xerces XSLTProcessor
        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        try {
          // DocumentBuilder builder = factory.newDocumentBuilder();
          // document = builder.newDocument();  // Create from whole cloth
        }
        catch (Exception e) {}
       
        **/
        userObjectCount = 0;
        jpl.mipl.io.util.DOMutils domUtil = new jpl.mipl.io.util.DOMutils();
    	_document = domUtil.getNewDocument();
        // _document = new org.apache.xerces.dom.DocumentImpl();
        Class c = _document.getClass();
        _metadataFormatName = c.getName();
        // or
        // _metadataFormatName = getDocumentName() ;
        if (debug) {
        	System.out.println("IIOMetadataToDOM "+_document);
        	System.out.println("IIOMetadataToDOM "+_metadataFormatName);
        }
            // VicarMetadata
          // metadataFormatName = "jpl.mipl.io.plugins.vicar.vicar_DOC";
          Element root = (Element) _document.createElement(_metadataFormatName); 
          
          _document.appendChild (root);
          
          // walk thru the IIOMetadata tree and put everything possible in.
          // This will only deal with text data. All UserData Objects will be lost
          
          addMetadata(root, metadata);
         if (debug)  System.out.println("IIOMetadataToDOM userObjectCount="+userObjectCount);
          return (_document);
    }
    
    
    /**
    * add all the text elements from the IIOMetadataNode to the document
    **/
    void addMetadata(Node root, IIOMetadataNode metadata) {
        if (root == null) {
            if (debug) System.out.print("null metadata");
        }
        else {
            addMetadata(root, metadata, 0);
        }
    }

    void indent(int level) {
        for (int i = 0; i < level; i++) {
            System.out.print("  ");
        }
    }
    
    
    private void addMetadata(Node node, IIOMetadataNode metadata, int level) {
        //                   output            input             level (for indentation)
        // Print node name and attribute names and values
        
        indent(level);
        System.out.print("<" + metadata.getNodeName());
        Element element = (Element) _document.createElement(metadata.getNodeName() );
        NamedNodeMap map = metadata.getAttributes();
        
        if (map != null) {
            int length = map.getLength();
            for (int i = 0; i < length; i++) {
                Node attr = map.item(i);
                element.setAttribute(attr.getNodeName(), attr.getNodeValue());
                System.out.print(" " + attr.getNodeName() +
                                 "=\"" +
                                 attr.getNodeValue() +
                                 "\"");
            }
        }

        // If the node is an IIOMetadataNode, print information
        // about the user object
        /****************
        if (node instanceof IIOMetadataNode) {
            userObjectCount++;
            Object o = ((IIOMetadataNode)node).getUserObject();
            if (o != null) {
                
                System.out.print(" userObject=\"");
                System.out.print(o.getClass().getName());
                element.setAttribute("UserObject", o.getClass().getName() );
                
                // could capture the byte stream into a string and set it as the value
                if (o instanceof byte[]) {
                    byte[] b = (byte[])o;
                    for (int i = 0; i < b.length; i++) {
                        System.out.print(" ");
                        System.out.print(b[i] & 0xff);
                    }
                } else {
                }
                System.out.print("\")");
            }
        }
        ************/
        String nodeValue = null;
        
            Object o = metadata.getUserObject();
            if (o != null) {
                userObjectCount++;
                if (debug) {
                	System.out.print(" userObject=\"");
                	System.out.print(o.getClass().getName());
                }
                element.setAttribute("UserObject", o.getClass().getName() );
                
                // could capture the byte stream into a string and set it as nodeValue ?
                if (o instanceof byte[]) {
                    byte[] b = (byte[])o;
                    for (int i = 0; i < b.length; i++) {
                        System.out.print(" ");
                        System.out.print(b[i] & 0xff);
                    }
                } else {
                }
                System.out.print("\")");
            }
        // should string from the UserObject as the value ??
        /****
        if (nodeValue != null) {
            System.out.print(" userObject=\"");
            element.setNodeValue(nodeValue);
        } ***/
        
        
        // nodeValue = metadata.getNodeValue();
        if (metadata.getNodeValue() != null) {
                // System.out.print(" ##nodeValue#"+metadata.getNodeValue()+"## ");
                // element.setNodeValue(metadata.getNodeValue() );
                Text text = (Text) _document.createTextNode(metadata.getNodeValue());
	            element.appendChild(text);
            }

        node.appendChild(element);
        
        // Visit the children recursively
        Node child = metadata.getFirstChild();
        if (child != null) {
            System.out.println(">");
            while (child != null) {
                addMetadata(element, (IIOMetadataNode) child, level + 1);
                child = child.getNextSibling();
            }
            indent(level);
          if (debug) System.out.println("</" + metadata.getNodeName() + ">");
        } else {
           if (debug) System.out.println(">"+ metadata.getNodeValue()+"<"+metadata.getNodeName()+"/>");
            
        }
    }
    
    
    
    
}