/*
*
* @(#)DOMtoXercesDocument.java	1.0 00/12/15
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
 * This class converts a Document (DOM) object into a 
 * org.apache.xerces.dom.DocumentImpl() Document<BR>
 * 
 * This allows the Document to be processed by 
 * apache's xerces processors. 
 * <p>
 * This method will try to copy everything "as is".
 * 
 * @version 0.5
 */
public class DOMtoXercesDocument {
    
    private static final boolean debug = false;
    private static final boolean addNodeTypeAttribute = false;
    private static final boolean copyUserData = false;
    int indentMax = 1;
   
    private Document _document = null;
    private Document _xercesDocument = null;
    String _documentName = null;
    
    // string to hold the output
    boolean outputToString = false;
    String outputString = null;
    StringBuffer sb = null;
    
    static final int ELEMENT_TYPE =   1;
    static final int ATTR_TYPE =      2;
    static final int TEXT_TYPE =      3;
    static final int CDATA_TYPE =     4;
    static final int ENTITYREF_TYPE = 5;
    static final int ENTITY_TYPE =    6;
    static final int PROCINSTR_TYPE = 7;
    static final int COMMENT_TYPE =   8;
    static final int DOCUMENT_TYPE =  9;
    static final int DOCTYPE_TYPE =  10;
    static final int DOCFRAG_TYPE =  11;
    static final int NOTATION_TYPE = 12;
    
    String[] nodeType = { 
            "NO-TYPE = 0", 
            "ELEMENT_TYPE = 1",
            "ATTR_TYPE = 2",
            "TEXT_TYPE = 3",
            "CDATA_TYPE = 4",
            "ENTITYREF_TYPE = 5",
            "ENTITY_TYPE = 6",
            "PROCINSTR_TYPE = 7",
            "COMMENT_TYPE = 8",
            "DOCUMENT_TYPE = 9",
            "DOCTYPE_TYPE = 10",
            "DOCFRAG_TYPE = 11",
            "NOTATION_TYPE = 12" };
    
    // Constructor
    public DOMtoXercesDocument(Document d) {
        _document = d;
    }
    
    
    public void convert() {
        
        if (_document != null) {
            Node root = _document.getDocumentElement();
            // strip off the document node??
            // check if there is onlt one child??
            // child the name of the root??
            Node childOfDocRoot = root.getFirstChild();
            
            /***
            _xercesDocument = new org.apache.xerces.dom.DocumentImpl();
            Class c = _xercesDocument.getClass();
            _documentName = c.getName();
        
        
        
            Element newRoot = (Element) _xercesDocument.createElement(_documentName); 
          
            _xercesDocument.appendChild (newRoot);
            **/
            // all of this may not be needed
            
            DOMutils domUtils = new DOMutils();
           Document newDoc =  domUtils.getNewDocument();
           _xercesDocument = newDoc;
          // Document doc\\\ =   newDoc.getImplementation();
          Class c = newDoc.getClass();
           _documentName = c.getName();
            Element newRoot = newDoc.createElement(_documentName);
            convertNodes(root, newRoot);
            // convertNodes(childOfDocRoot, newRoot);
        }
    }
    
    
    /**
    * converts a document to an xerces Document Node tree
    **/
    void convertNodes(Node root, Node newNode) {
        if (root == null) {
            System.out.print("null metadata");
        }
        else {
            convertNodes(root, newNode, 0);
        }
    }

    public void setDocumentName( String s) {
        _documentName = s;
        // force new convert??
        convert();
    }
    
    public String getDocumentName() {
       return _documentName ;
    }
    
    // return Node and let user cast it ??
    public Document getXercesDocument() {
        if (_xercesDocument == null) convert();
        return _xercesDocument;
    }
    
    /**
    * do a convert and caprure a string of the contents of the node
    * see ImageDumper.
    * Just use that instead
    **/
    public String toString() {
        outputToString = true;
        outputString = null;
        sb = new StringBuffer();
        convert();
        outputToString = false;
        // return outputString;
        sb.append("END\n");
        String s = sb.toString();
        sb = null;
        return s;
    }
    
    void indent(int level) {
        for (int i = 0; i < level; i++) {
            System.out.print(" ");
            if (sb != null) { sb.append(" ") ; }
        }
    }

    
    
    String getPadding(String s, int indent) {
        int x = indentMax - s.length();
        x -= indent;
        // x -= indent; // indent is 2 spaces
        // String r = x+"";
        String r = "";
        for (int i=1 ; i < x ; i++ ) {
            r = r+" ";
        }
       return r; 
    }
    
    
    /**
    * input Node node <br>
    * output IIOMetadataNode iioNode <br> 
    * int level - used only for the formatting of the StringBuffer used by toString
    *
    **/
    private void convertNodes(Node node, Node newNode, int level) {
        // Print node name and attribute names and values
        // indent(level);
        // System.out.print("<" + node.getNodeName());
        String nodeName = node.getNodeName();
        String nodeValue = node.getNodeValue();
        String pad ;
        
        String attrNodeName = null;
        String attrKey = null;
        String attrNodeValue = null;
        
        String objectName = null;
        
        
        
        int type = node.getNodeType();
          // create the same type of node??
          // I don't know how to do that with IIOMetadaNode
          // if this node is ANYTHING but an Element we should ignore it
          // Attribute nodes should fall out below. We sghould never see
          // an Attribute node at this level.
          Element element = (Element) _xercesDocument.createElement( nodeName );
          element.setNodeValue(nodeValue);
          newNode.appendChild(element); // this is probably null
          // the value is really in a text node
          
          // this should always be Element
          // I'll throw this away after some testing
          // iioNode.setAttribute("nodeType", ""+nodeType);
          if (addNodeTypeAttribute) {
            element.setAttribute("nodeType", nodeType[type]); 
          }
          
            
          // get all the attributes and add them to this node
          NamedNodeMap map = node.getAttributes();
          if (map != null) {
            int length = map.getLength();
            for (int i = 0; i < length; i++) {
                Node attr = map.item(i);
                attrNodeName = attr.getNodeName();
                attrNodeValue = attr.getNodeValue();
                // this will be an Attribute nodeType
                element.setAttribute(attrNodeName, attrNodeValue);
             } 
            }

            // org.apache.xerces.dom.NodeImpl
            // setUserData(java.lang.Object data)
            // xerces nodes have something equivalent to userObject
            // look for them and add them to the tree
            /***
            if (copyUserData) {
              Object data = node.getUserData();
              if (data != null) {
                element.setUserData(data);
              }
            }
            ***/
            
            // Visit the children recursively
            // I THINK a node which has TEXT or CDATA children will not have
            // other children
            // check somewhere to be sure. I will not check for more children  ???
            Node child = node.getFirstChild();
            if (child != null) {
                // nodeValue = "";
                // System.out.println(">");
                while (child != null) {
                    int nType = child.getNodeType();
                    // check node type - store the data in a TEXT or CDATA node
                    // as the node value (IIOMetadata doesn't implement TEXT and CDATA nodes)
                    // these aren't "REAL" children
                    // 
                    if (nType == TEXT_TYPE) {
                        nodeValue = child.getNodeValue(); 
                        Text text = (Text) _xercesDocument.createTextNode(nodeValue);
                        element.appendChild(text);
                    }
                    else {
                        // IIOMetadataNode iioChild = new 
                        convertNodes(child, element, level + 1);
                    }
                    child = child.getNextSibling();
                    nodeValue = "";
                }
                
            } 
       
    } // end of convertMetadata
}
