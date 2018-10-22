/*
*
* @(#)DOMtoIIOMetadata.java	1.0 00/12/15
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

/**
 * This class converts a Document (DOM) object into an 
 * IIOMetadata tree<BR>
 * 
 * This will be called from inside a reader, writer, transcoder or display
 * program. One use of this method will be to create the generic
 * IIOMetadata tree from a Document (native?) created by a reader or transcoder.
 * This tree can be used by someone else's writer or transcoder.
 * Documents are used because they can be fed thru xml xsl xslt processors.
 * IIOMetadata nopdes are not comapatable and cause errors when I have used
 * them with processors. So far I have used apache's xerces processors. Maybe 
 * someone else's won't throw exceptions when they are fed an IIOMetadataNode.
 * The Document used to drive the output may have come from a reader,
 * transcoder, or created programatically.
 * <p>
 * This method will try to copy everything "as is". Since IIOMetadataNode doesn't
 * implement TEXT nodes all text from TEXT and CDATA nodes will be copied into
 * the value of the IIO node.
 * IIOMetadataNodes are either Element or Attrubte nodes.
 * They can't be anything else.
 * 
 * @version 0.5
 */
public class DOMtoIIOMetadata {
    
    private static final boolean debug = false;
    private static final boolean addNodeTypeAttribute = false;
    
    private static final int indentMax = 32; // 31
    private static final int wrapStart = 35; // 34
    private static final int lineMax = 80;  // 80
    private static final int valueMax = 43; // 44
    
    boolean inObject = false;
    private Document _document = null;
    private IIOMetadataNode _rootIioNode  = null;
    String _rootNodeName = "com.sun.imageio_1.0" ;
    
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
    public DOMtoIIOMetadata(Document d) {
        _document = d;
    }
    
    
    public void convert() {
        
        if (_document != null) {
            Node root = _document.getDocumentElement();
            // strip off the documnet node??
            // check if there is onlt one child??
            // child the name of the root??
            Node childOfDocRoot = root.getFirstChild();
            _rootIioNode = new IIOMetadataNode(_rootNodeName);
            // convertMetadata(root, _rootIioNode);
            convertMetadata(childOfDocRoot, _rootIioNode);
        }
    }
    
    /** 
    * Could allow user to set the IIOMetadataNode to convert into.
    * This would allow a document to added to the supplioed node as 
    * a child of that node.
    **/
    /**
    * converts a document to an IIOMetadaNode tree
    **/
    void convertMetadata(Node root, IIOMetadataNode iioNode) {
        if (root == null) {
            System.out.print("null metadata");
        }
        else {
            convertMetadata(root, iioNode, 0);
        }
    }

    public void setRootNodeName( String s) {
        _rootNodeName = s;
        // force new convert??
        convert();
    }
    
    public String getRootNodeName() {
       return _rootNodeName ;
    }
    
    // return Node and let user cast it ??
    public IIOMetadataNode getRootIIONode() {
        if (_rootIioNode == null) convert();
        return _rootIioNode;
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
    private void convertMetadata(Node node, IIOMetadataNode rootIioNode, int level) {
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
        
        IIOMetadataNode iioNode;
        
        int type = node.getNodeType();
          // create the same type of node??
          // I don't know how to do that with IIOMetadaNode
          // if this node is ANYTHING but an Element we should ignore it
          // Attribute nodes should fall out below. We sghould never see
          // an Attribute node at this level.
          iioNode = new IIOMetadataNode(nodeName); // this IS an Element nodeType
          iioNode.setNodeValue(nodeValue);
          rootIioNode.appendChild(iioNode);
          
          // this should always be Element
          // I'll throw this away after some testing
          // iioNode.setAttribute("nodeType", ""+nodeType);
          if (addNodeTypeAttribute) {
            iioNode.setAttribute("nodeType", nodeType[type]); 
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
                iioNode.setAttribute(attrNodeName, attrNodeValue);
             } 
            }

            // org.apache.xerces.dom.NodeImpl
            // setUserData(java.lang.Object data)
            // xerces nodes have something equivalent to userObject
            // look for them and add them to the tree
            
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
                    if (type == TEXT_TYPE) {
                        nodeValue = nodeValue+child.getNodeValue(); 
                        iioNode.setNodeValue(nodeValue); // replace nodeValue
                    } else if (type == CDATA_TYPE) {
                        nodeValue = nodeValue+child.getNodeValue();
                        iioNode.setNodeValue(nodeValue); // replace nodeValue
                    }else {
                        // IIOMetadataNode iioChild = new 
                        convertMetadata(child, iioNode, level + 1);
                    }
                    child = child.getNextSibling();
                    nodeValue = "";
                }
                
            } 
       
    } // end of convertMetadata
}
