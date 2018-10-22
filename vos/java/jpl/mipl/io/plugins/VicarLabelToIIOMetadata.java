/*
*
* @(#)VicarLabelToIIOMetadata.java	1.0 00/12/15
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
 * This class builds a IIOMetadata  from a vicar image
 * label object.<BR>
 * A later version MAY handle reading a String which contains
 * the label contents and parses that text.
 * This will be called from inside a reader.
 * @version 0.5
 */
public class VicarLabelToIIOMetadata {
    
    private boolean debug = false;
    private VicarInputFile _vif;
    
    // private Document _document = null;
    IIOMetadataNode _iioNode = null;
    
    protected VicarLabel _label;
    protected VicarLabelSet _system;
    protected VicarLabelCategory _property;
    protected VicarLabelCategory _history;
    
    String _documentName ; // implementation class string
    // VicarMetadata
    String commonMetadataFormatName = "com.sun.imageio_1.0";
    // String nativeMetadataFormatName = "jpl.mipl.io.plugins.vicar.vicarimage_1.0";
    // String nativeMetadataFormatName = "VICAR_LABEL";
    // Constructor
    // add another constructor to set the 
    // VicarMetadata
    public VicarLabelToIIOMetadata (VicarInputFile v, String formatName) {
        
        if (formatName != null) {
            commonMetadataFormatName = formatName;
        }
        
        _vif = v;
        _label = null;
        if (debug)     
          System.out.println("VicarLabeltoIIOM(VivarInputFile  constructor");
        buildIIOM();
    }
    
    /* 
    * This constructor is for the case where the VicarLabel may NOT have 
    * come from a VicarInputFile. The VicarLabel may have been created 
    * by hand OR it may have been modified from the original.
    **/
    public VicarLabelToIIOMetadata (VicarLabel vl) {
        
        
        _vif = null;
        _label = vl;
        if (debug)     
          System.out.println("VicarLabeltoIIOM(VicarLabel)  constructor");
        buildIIOM();
    }
    
    /* 
    * This constructor is for the case where the VicarLabel may NOT have 
    * come from a VicarInputFile. The VicarLabel may have been created 
    * by hand OR it may have been modified from the original.
    **/
    public VicarLabelToIIOMetadata (VicarLabel vl, String formatName) {
        
        if (formatName != null) {
            commonMetadataFormatName = formatName;
        }
        
        _vif = null;
        _label = vl;
        if (debug)     
          System.out.println("VicarLabeltoIIOM(VicarLabel)  constructor");
        buildIIOM();
    }
    
    // --------------------------------------------------------
    
    
    
    // public Node getRoot() {
    public IIOMetadataNode getRoot() {
        // should check for null document??
        return  _iioNode;
    }
    
    public void setCommonMetadataFormatName(String name) {
        commonMetadataFormatName = name;
    }
    
    public String getCommonMetadataFormatName() {
        return commonMetadataFormatName;
    }
    
    public void setDebug( boolean d) {
    	debug = d;
    }
    
    public void buildIIOM () 
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
                System.out.println("VicarLabeltoIIOM: No VicarLabel object available!");
                // throw new Exception("VicarLabeltoIIOM: No VicarLabel object available!");
            }
        }
        catch (IOException ex)
        {
            System.out.println("VicarLabeltoIIOM: IOException Error getting vicar label:"+ex.getMessage());
            ex.printStackTrace();
            return;
        }
        
        
        _system = _label.getSystem();
	    _property = _label.getProperty();
	    _history = _label.getHistory();
	    
        
        toIIOM();
        
        
        
    } // buildDom
  
  // from VicarLabel.java    
    public String toString()
    {

	System.out.println( _system.toString() );
	System.out.println( _property.toString() );
	System.out.println( _history.toString() );

	return "--- VicarLabeltoIIOM.toString() Done -----";
    }
    // from VicarLabelCategory.java

    
// from VicarLabel.java    
    public String toIIOM() // make this void ???
    {
    
        try {
          // create the root node 
          
          _iioNode = new IIOMetadataNode(commonMetadataFormatName);
          // IIOMetadtataNode root = _iioNode ;
          IIOMetadataNode vicarNode = new IIOMetadataNode("VicarLabel");
          vicarNode.setUserObject(_label);
          
          _iioNode.appendChild(vicarNode);
          
          String sys = toIIOMS(_system,_iioNode);
          String prop = toIIOMC(_property, _iioNode) ;
          String hist = toIIOMC(_history, _iioNode) ;
          
          if (_label == null) {
            System.out.println( "_label is NULL");
          }
          
          if (debug) {
          	System.out.println( "toIIOM() ---------------");
            System.out.println( "---- _system ------");
	        
	        System.out.println( sys);
	        System.out.println( "---- _property ------");        
	        System.out.println( prop );	        
	        System.out.println( "---- _history ------");
	        System.out.println( hist );	        
	        System.out.println( "toIIOM() !@#$%^&*(!@#$%^&*!@#$%^&*-----");
          }

	        return "--- VicarLabeltoIIOM.toIIOM() Done -----";
	        // } catch (ParserConfigurationException pce) { // for factory
	        } catch (Exception pce) {
            // Parser with specified options can't be built
            pce.printStackTrace();

        }
        
	return "--- VicarLabeltoIIOM.toString() Done -----";
	// this should really be a void
    }
    
    
    
    
    
    
    // from VicarLabelCategory.java
/**
*  Add all of the data in this Category to the DOM
***/
public String toIIOMC(VicarLabelCategory cat, IIOMetadataNode root)
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
	    toIIOMS(set, root);	    
	    
	    
	}
	return buf.toString();
    }
    
// from VicarLabelSet.java    
    
     public String toIIOMS(VicarLabelSet set, IIOMetadataNode root)
    {
	StringBuffer buf = new StringBuffer(set.size() * 20);
	
	// Node node;
	// Element element = null;
	// Element item = null;
	IIOMetadataNode node = null;
	IIOMetadataNode element = null;
	IIOMetadataNode item = null;
    
	
    int flavor = set.getFlavor();
    String name = set.getName();
    int instance = set.getInstance();
    
	switch (flavor) {
	    case VicarLabelSet.LABEL_SYSTEM:
		buf.append("---- System Label ----\n");
	    // element = (Element) _document.createElement("SYSTEM"); 
	    element = new IIOMetadataNode("SYSTEM"); 
		break;
	    case VicarLabelSet.LABEL_PROPERTY:
		buf.append("---- Property: ");
		// element = (Element) _document.createElement("PROPERTY"); 
		element = new IIOMetadataNode("PROPERTY");
		break;
	    case VicarLabelSet.LABEL_HISTORY:
		buf.append("---- Task: ");
		// element = (Element) _document.createElement("TASK"); 
		element = new IIOMetadataNode("TASK");
		break;
	    default:			// shouldn't happen
		buf.append("---- Unknown Label Set: ");
		break;
	}
	
	
	if (flavor != VicarLabelSet.LABEL_SYSTEM) {
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
	    // value.replace('(', ' ');
	    // value.replace(')', ' ');
	    // remove or change single double quotes ???
	    // this should become configurable
	    // filter quotes from Text areas or NOT
	    // System.out.print( "value >"+value+"< ");
	    value = value.replace('`', ' ');
	    value = value.replace('\'', ' ');
	    value = value.replace('\"', ' ');
	    value = value.trim();
	    // System.out.println( "<"+key+">"+value+"< ");
	    // could also remove "()" in value string
	    // key = ((VicarLabelItem)(it.next())).getKeyword();
	    // value = ((VicarLabelItem)(it.next())).getKeyword();
	    // create an element for this item
	    // create the textnode of this element to hold the value
	    
	    if (debug) System.out.println( "key >"+key+"< value >"+value+"< ");
	    // item = (Element) _document.createElement("item"); 
	    item = new IIOMetadataNode("item");
	    item.setAttribute("key", key);
	    item.setNodeValue(value);
	    
	    // Text text = (Text) _document.createTextNode(value);
	    // item.appendChild(text);
	    
        // use CDATA instead of Text so that quotes etc don't get turned into
	    // &apos
	    // CDATASection cdata = (CDATASection) document.createCDATASection(value);
	    // item.appendChild(cdata);
	    element.appendChild(item);	    
	}

	return buf.toString();
    }



}