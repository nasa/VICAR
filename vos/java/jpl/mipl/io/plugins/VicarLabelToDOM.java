/*
*
* @(#)VicarLabelToDOM.java	1.0 00/12/15
 *
 * Steve Levoe
 * Jet Propulsion Laboratory
 * Multimission Image Processing Laboratory
 * 12-2000 ImageIO EA2 version
 * 8-2002 JDK 1.4 version
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
 * This class builds a DOM Document from a vicar image
 * label object.<BR>
 * A later version MAY handle reading a String which contains
 * the label contents and parses that text.
 * This will be called from inside a reader.
 * @version 0.5
 */
public class VicarLabelToDOM {
    
    private boolean debug = false;
    private VicarInputFile _vif;
    
    private Document _document = null;
    
    protected VicarLabel _label;
    protected VicarLabelSet _system;
    protected VicarLabelCategory _property;
    protected VicarLabelCategory _history;
    boolean useAttributeForValue = false;
    
    String keyTag = "name"; // "key"
    
    DOMutils domUtil = null;
    
    String _documentName = null;
    // VicarMetadata
    // String nativeMetadataFormatName = "jpl.mipl.io.plugins.vicar.vicarimage_1.0";
    String nativeMetadataFormatName = "VICAR_LABEL";
    // Constructor
    // add another constructor to set the 
    // VicarMetadata
    public VicarLabelToDOM (VicarInputFile v, String formatName) {
        
        if (formatName != null) {
            nativeMetadataFormatName = formatName;
        }
        
        _vif = v;
        _label = null;
        if (debug)     
          System.out.println("VicarLabelToDOM constructor");
        buildDom();
    }
    
    
    /* 
    * This constructor is for the case where the VicarLabel may NOT have 
    * come from a VicarInputFile. The VicarLabel may have been created 
    * by hand OR it may have been modified from the original.
    **/
    public VicarLabelToDOM (VicarLabel vl, String formatName) {
        
        if (formatName != null) {
            nativeMetadataFormatName = formatName;
        }
        
        _vif = null;
        _label = vl;
        if (debug)     
          System.out.println("VicarLabelToDOM(VicarLabel)"+formatName+" constructor");
        buildDom(); // only build on demand
    }
    
    /* 
    * This constructor is for the case where the VicarLabel may NOT have 
    * come from a VicarInputFile. The VicarLabel may have been created 
    * by hand OR it may have been modified from the original.
    **/
    public VicarLabelToDOM (VicarLabel vl) {
        
        _vif = null;
        _label = vl;
        if (debug)     
          System.out.println("VicarLabelToDOM(VicarLabel)  constructor "+nativeMetadataFormatName);
        buildDom(); // only build on demand
    }
    
    // --------------------------------------------------------
    
    public Document getDocument() {
        // should check for null document??
        if (_document == null) {
            // buildDom();
        }
        return _document;
    }
    
    /***
                       getMetadataFormatNames() 
                             Returns an array of Strings containing the names of the metadata formats recognized
                   by this plug-in's getAsTree, setFromTree, and mergeTree methods.
             String
                   getNativeMetadataFormatName() 
    ***/
    
    public void setNativeMetadataFormatName(String name) {
        nativeMetadataFormatName = name;
    }
    
    public String getNativeMetadataFormatName() {
        return nativeMetadataFormatName;
    }
    
    public void setDebug(boolean d) {
    	debug = d;
    }
    
    public void buildDom () 
    {
        if (debug) System.out.println("--------------- buildDom -------- ");
        // SystemLabel sys = _vif.getSystemLabel();
        // System.out.println("System label:"+sys);
        // VicarLabel label; // global so it can be used elsewhere
        // if Constructor supplied the VivarLabel use it
        // otherwise get the VicarLabel from the VicarInputFile
        try {
            if (_label == null) {
                if (_vif != null ) {
                    _label = _vif.getVicarLabel();
                // System.out.println(label.toString());
                }
            }
            
            if (_label == null) {
                System.out.println("VicarLabelToDOM: No VicarLabel object available!");
                // throw new Exception("VicarLabelToDOM: No VicarLabel object available!");
            }
        }
        catch (IOException ex)
        {
            System.out.println("VicarLabelToDOM: IOException Error getting vicar label:"+ex.getMessage());
            ex.printStackTrace();
            return;
        }
        
        
        _system = _label.getSystem();
	    _property = _label.getProperty();
	    _history = _label.getHistory();
	    
        
        toDom();
        
        
        
    } // buildDom
  
  // from VicarLabel.java    
    public String toString()
    {

	System.out.println( _system.toString() );
	System.out.println( _property.toString() );
	System.out.println( _history.toString() );

	return "--- VicarLabelToDOM.toString() Done -----";
    }
    // from VicarLabelCategory.java

    
// from VicarLabel.java    
    public String toDom() // make this void ???
    {
    // try using the xerces Document for our DOM
    // the commented out Document is not compatable
    // with the XSLTProcessor
    // create tree,ubparts willl add to this tree
    // DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        try {
          // DocumentBuilder builder = factory.newDocumentBuilder();
          // document = builder.newDocument();  // Create from whole cloth
          
          
          // _document = new org.apache.xerces.dom.DocumentImpl();
          // use DOMUtils to get the new document
          
          domUtil = new DOMutils();
          useAttributeForValue = domUtil.useAttributeForValue;
          _document = domUtil.getNewDocument();
          
          
          /* we don't need this if DOMuitils makes all the Documents they 
          * will all be compatable
          Class c = _document.getClass();
          _documentName = c.getName();
          
          Element documentNameNode = (Element) _document.createElement(_documentName); 
          
          _document.appendChild (documentNameNode);
          
          ****/
          
          // or 
          // Document getNewDocument(String rootName, String creatorName) {
          // _document = domUtil.getNewDocument(nativeMetadataFormatName, this.toString());
            
          Element root = (Element) _document.createElement(nativeMetadataFormatName); 
          
          // documentNameNode.appendChild(root);
          _document.appendChild (root);
          
          
          // put in a node which holds _label
          // can I put an Object into a node ??
          // no Document implementations support this feature, it will return null and be ignored
          // eventually it may be taken out altogether, for now it is a placeholder for the thought
          Node userNode = null;
          // userNode = domUtil.createUserNode(_document, "jpl.mipl.io.vicar.VicarLabel", _label);
          
          /**
          Element vicarNode = _document.createElement("jpl.mipl.io.vicar.VicarLabel"); 
          // this will only work for the xerces Implementation
          org.apache.xerces.dom.ElementImpl e = (org.apache.xerces.dom.ElementImpl) vicarNode;
          e.setUserData(_label);
          **/
          if (userNode != null) {
            root.appendChild (userNode);
          }
          
          String sys, prop, hist;
          sys = toDomS(_system, root);
          prop = toDomC(_property, root);
          hist = toDomC(_history, root);
          
          
          if (_label == null) {
            System.out.println( "_label is NULL");
          }
          
          if (debug) {
          	System.out.println( "toDom() ---------------");
            System.out.println( "---- _system ------");
            
	        System.out.println( sys );
	        System.out.println( "---- _property ------");
	        
	        System.out.println( prop );
	        System.out.println( "---- _history ------");
	        
	        System.out.println( hist );
	        
	        System.out.println( "toDom() !@#$%^&*(!@#$%^&*!@#$%^&*-----");
          }
	        /***
	        VicarIIOMetadataNode element = new VicarIIOMetadataNode(_label);
           // System.out.println( "VicarIIOMetadataNode "+element);
           System.out.println( "<VicarIIOMetadataNode> "+element.toString());
           root.appendChild (element);
           
           **/
           
           // _document.appendChild (element);
	        
	       // place a node which holds the VicarLabel Object
            // addVicarLabelToDOM(root); 

	        return "--- VicarLabelToDOM.toDom() Done -----";
	        // } catch (ParserConfigurationException pce) { // for factory
	        } catch (Exception pce) {
            // Parser with specified options can't be built
            System.out.println( "Exception "+pce);
            pce.printStackTrace();

        }
        
	return "--- VicarLabelToDOM.toString() Done -----";
	// this should really be a void
    }
    
    
    /**
    * add a node which is a VicarIIOMetadataNode and holds the 
    * VicarLabel object
    * IIOMetadaNode is NOT compatable with the xerces implementation I am using so I can't
    * add this node to my document or one of its children
    **/
    private void addVicarLabelToDOM(Node node) {
        // this sets the UserObject to the label and adds it to the root
        // VicarIIOMetadataNode element = new VicarIIOMetadataNode(_label);
        
        IIOMetadataNode vicarLabel_node = new IIOMetadataNode("jpl.mipl.io.vicar.VicarLabel");
        // this is also its class name
        vicarLabel_node.setNodeValue("VicarLabel"); // not needed ???
        vicarLabel_node.setUserObject(_label);
        if (debug) {
        	System.out.println( "addVicarLabelToDOM "+vicarLabel_node);
        	System.out.println( "addVicarLabelToDOM "+vicarLabel_node.getNodeName());
        }
        node.appendChild (vicarLabel_node);
        
    }
    

    
    
    // from VicarLabelCategory.java
/**
*  Add all of the data in this Category to the DOM
***/
public String toDomC(VicarLabelCategory cat, Node root)
    {
	StringBuffer buf = new StringBuffer(cat.getNumSets() * 500);

    String s, key, value;
    Element item;
    VicarLabelSet set = null;
	for (Iterator it = cat.iterator(); it.hasNext(); ) {
	    set = (VicarLabelSet) it.next();
	    s = set.toString();
	    // s = ((VicarLabelSet)(it.next())).toString();
	    // set = 
	    // buf.append("C="+s);
	    buf.append(s);
	    toDomS(set, root);	    
	    
	    
	}
	return buf.toString();
    }
    
// from VicarLabelSet.java    
    
     public String toDomS(VicarLabelSet set, Node root)
    {
	StringBuffer buf = new StringBuffer(set.size() * 20);
	
	Node node;
	Element element = null;
	Element item = null;
	Element subItem = null;
	
    
	
    int flavor = set.getFlavor();
    String name = set.getName();
    int instance = set.getInstance();
    
	switch (flavor) {
	    case VicarLabelSet.LABEL_SYSTEM :  // set.LABEL_SYSTEM:
		buf.append("---- System Label ----\n");
	    element = (Element) _document.createElement("SYSTEM"); 
		break;
	    case VicarLabelSet.LABEL_PROPERTY : // set.LABEL_PROPERTY:
		buf.append("---- Property: ");
		// element = (Element) _document.createElement("OBJECT"); 
		element = (Element) _document.createElement("PROPERTY"); 
		break;
	    case VicarLabelSet.LABEL_HISTORY : // set.LABEL_HISTORY:
		buf.append("---- Task: ");
		element = (Element) _document.createElement("TASK"); 
		break;
	    default:			// shouldn't happen
		buf.append("---- Unknown Label Set: ");
		break;
	}
	
	
	if (flavor != VicarLabelSet.LABEL_SYSTEM)  { // set.LABEL_SYSTEM) {
	    // these are ATTRIBUTES of the ELEMENT
	    element.setAttribute("name", name);
	    element.setAttribute("instance", ""+instance);
	    
	    buf.append(name);
	    buf.append(" ---- Instance: ");
	    buf.append(instance);
	    buf.append(" ----\n");
	}

    root.appendChild (element);
    
    // these are all the items inside this element
    String s, key, value;
    String[] subValues = null;
	for (Iterator it = set.iterator(); it.hasNext(); ) {
	    s = ((VicarLabelItem)(it.next())).toString();
	    // buf.append("S="+s);
	    buf.append(s);
	    buf.append("\n");
	    //key is before = value(s) are after
	    
	    // System.out.println( "iterator >"+s+"<");
	    StringTokenizer st = new StringTokenizer(s,"=",false);
	    key = st.nextToken();
	    key = key.trim();
	    value = st.nextToken();
	    
	    
	    // break the value into subValues
	    if (value.indexOf(',') != -1 && value.indexOf('(') != -1) {
	    	if (debug) System.out.println( keyTag+" >"+key+"< value >"+value+"< ");
	   		 item = (Element) _document.createElement("item"); 
	   		 item.setAttribute(keyTag, key);
	    
	    	value = value.replaceAll("\\(",""); // replace all right parens
	    	value = value.replaceAll("\\)",""); // replace all left parens
	    	String subValue = null;	
	    	subValues = value.split(",");
	    	for (int i=0 ; i< subValues.length ; i++) {
	    		// should we call entity encoder ??
	    		// String eValue = domUtil.encodeEnitiy(value);
	    		subValue = subValues[i];
	    		String quoted = "false"; // could also use "false" "single" "double"
	    		if (subValue.indexOf('"') != -1 ) {
	    			quoted ="true"; // "double"
	    			subValue = subValue.replaceAll("\"",""); // replace all double quotes
	    		}    		
	    		else if (subValue.indexOf('\'') != -1)  {
	    			quoted ="true"; // "single"
	    			subValue = subValue.replaceAll("\'",""); // replace all single quotes
	    		}
	    		else {
	    			quoted = "false";
	    		}
	    		subValue = subValue.trim();
	    		
	    		 subItem = (Element) _document.createElement("subitem"); 
	   			 subItem.setAttribute(keyTag, key);
	   			 subItem.setAttribute("quoted", quoted);
	    		
	   			 if (useAttributeForValue) {
	       			 subItem.setAttribute("value", subValue);
	        		}
	   		 	else {
	    	
	    			// look for subelements
	        		Text text = (Text) _document.createTextNode(subValue);
	       			 subItem.appendChild(text);
	     			}
	     		item.appendChild(subItem);
	    	}
	    	
	    }
	    else {
	     // NO subValues
	    
	    // value.replace('(', ' ');
	    // value.replace(')', ' ');
	    // remove or change single double quotes ???
	    // this should become configurable
	    // filter quotes from Text areas or NOT
	    // System.out.print( "value >"+value+"< ");
	    
	    // value = value.replace('`', ' ');
	    // value = value.replace('\'', ' ');
	   //  value = value.replace('\"', ' ');
	    
	    	String quoted = "false"; // could also use "false" "single" "double"
	    		if (value.indexOf('"') != -1 ) {
	    			quoted ="true"; // "double"
	    			value = value.replaceAll("\"",""); // replace all double quotes
	    		}    		
	    		else if (value.indexOf('\'') != -1)  {
	    			quoted ="true"; // "single"
	    			value = value.replaceAll("\'",""); // replace all single quotes
	    		}
	    		else {
	    			quoted = "false";
	    		}
	    
	    value = value.trim(); // remove spaces at front and back of the line
	    // System.out.println( " >"+value+"< ");
	    // could also remove "()" in value string
	    // key = ((VicarLabelItem)(it.next())).getKeyword();
	    // value = ((VicarLabelItem)(it.next())).getKeyword();
	    // create an element for this item
	    // create the textnode of this element to hold the value
	    
	    if (debug) System.out.println( keyTag+" >"+key+"< value >"+value+"< ");
	    item = (Element) _document.createElement("item"); 
	    item.setAttribute(keyTag, key);
	    item.setAttribute("quoted", quoted);
	    
	    // should we call entity encoder ??
	    // String eValue = domUtil.encodeEnitiy(value);
	    if (useAttributeForValue) {
	        item.setAttribute("value", value);
	        }
	    else {
	    	
	    	// look for subelements
	        Text text = (Text) _document.createTextNode(value);
	        item.appendChild(text);
	     }
	    }
	    
	    
        // use CDATA instead of Text so that quotes etc don't get turned into
	    // &apos
	    // CDATASection cdata = (CDATASection) document.createCDATASection(value);
	    // item.appendChild(cdata);
	    element.appendChild(item);	    
	}

	return buf.toString();
    }



}