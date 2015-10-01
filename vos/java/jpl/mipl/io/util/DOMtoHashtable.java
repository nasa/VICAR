
/*
*
* @(#)DOMtoHashtable.java	1.0 03/04/10
 *
 * Steve Levoe
 * Jet Propulsion Laboratory
 * Multimission Image Processing Laboratory
 * 4-10-2003
 * updated to add FITS support 5-25-2005
*
***************************************/
package jpl.mipl.io.util;

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
 * Hashtable tree<BR>
 * The Document is expected to come from an ImageIO plugin readers Metadata Object.
 * Keywords specific to ISIS, FITS, VICAR and PDS Documents from the Vicario plugins are searched 
 * for. It may work for Documents from other ImageIO plugins Metadata. We'll see.
 * Documents from some other arbitrary source will likely produce something but I can't
 * currently image what.<br>
 * A Hashtable is easy to search. Sub Nodes will be stored as Hashtables.
 * 
 * @version 0.5
 */
public class DOMtoHashtable  {
    
    boolean debug = false;
    
    boolean inObject = false;
    private Document _document = null;
    private IIOMetadataNode _rootIioNode  = null;
    
    
    Hashtable rootHashtable = null;
    
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
    public DOMtoHashtable(Document d) {
        _document = d;
    }
    
    public void setDebug(boolean d) {
    	debug = d;
    }
    
    public void convert() {
        
        if (_document != null) {
            Node root = _document.getDocumentElement();
            // strip off the documnet node??
            // check if there is onlt one child??
            // child the name of the root??
            rootHashtable = new Hashtable();
            Hashtable childHashtable = new Hashtable();
            
            
            convertMetadata(root, rootHashtable);
            // rootHashtable.put("")
            /*
            NodeList nl = root.getChildNodes();            
            int len = nl.getLength();
            System.out.println("***** root.getChildNodes "+len+" ***********");
            for (int i=0 ; i<len ; i++) {
            	Node n = nl.item(i);
            	System.out.println(i+" ********************************* "+n);
            	convertMetadata(n, rootHashtable);
            }
            **/
            
            
            
        }
    }
    
    
    /**
    * converts a document to an Hashtable tree
    **/
    void convertMetadata(Node root, Hashtable inHash) {
        if (root == null) {
            System.out.print("DOMtoHashtable.convertMetadata() null metadata");
        }
        else {
            convertMetadata(root, inHash, 0);
        }
    }

	
    
    
    /* 
     * forces creation of the hashtable     */
    public Hashtable getHashtable() {
    	if (rootHashtable == null) {
    		convert();
    	}
    	return rootHashtable;
    }
    
    /**
    * do a convert and capture a string of the contents of the node
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

    
    
    
    
   /**
    * Convert an XML Node to a Hashtable. Each subnode which is not a value will become
    * a new hashtable with the name of the subnode as the key.    * @param node node Node, usually the roor Node from a Document Object    * @param rootHash Hashtable to put data into, sub Hashtables will be created as needed    * @param level keep track of the level of subnode, used for pretty printing    */
   private void convertMetadata(Node node, Hashtable rootHash, int level) {
        // Print node name and attribute names and values
        // indent(level);
        // System.out.print("<" + node.getNodeName());
        String nodeName = node.getNodeName();
        String nodeValue = node.getNodeValue();
        
        String pad ;
        
        String attrNodeName = null;
        String attrKey = null;
        String attrNodeValue = null;
        
        String itemName = null;
        String childItemName = null;
        
        String objectName = null;
        
        Hashtable nodeHash = null;
        Hashtable childHash = null;
        // IIOMetadataNode iioNode;
        
        int type = node.getNodeType();
         
        
          
          nodeHash = new Hashtable();        
          childHash = new Hashtable();
          
          String nValue = getNodeValueString(node);
          
          if (debug) System.out.println(level+") convertMetadata "+nodeName+"."+nodeValue+" . "+nValue);
          
          
          if (nodeName.equalsIgnoreCase("SYSTEM")) {
          	
          		rootHash.put(nodeName, childHash); // should this be childName ?
          		if (debug) System.out.println("put to rootHash: "+nodeName+" childHash %%%%%%%%%%%");
          		Node child = node.getFirstChild();
            	int childCt = 0;
            	String childValue = "";
            	String childName = "";
            	if (child != null) {
                // nodeValue = "";
                	
                	// indent(level);
                	// System.out.println("child != NULL ");
                	// rootHash.put(nodeName, childHash); // should this be childName ?
                	while (child != null) {
                		
                		convertMetadata(child, childHash, level + 1);
          				child = child.getNextSibling();
                    	
                	} // end of while
            	}
          	
          } else if (nodeName.equalsIgnoreCase("TASK")) {
          	
          		String taskName = getNameAttribute(node);
          		rootHash.put(taskName, childHash); // should this be childName ?
          		if (debug) System.out.println("put to rootHash: "+nodeName+" "+taskName+" childHash %%%%%%%%%%%");
          		Node child = node.getFirstChild();
            	int childCt = 0;
            	String childValue = "";
            	String childName = "";
            	if (child != null) {
                // nodeValue = "";
                	
                	// indent(level);
                	// System.out.println("child != NULL ");
                	// rootHash.put(nodeName, childHash); // should this be childName ?
                	while (child != null) {
                		
                		convertMetadata(child, childHash, level + 1);
          				child = child.getNextSibling();
                    	
                	} // end of while
            	}
          	
          } else if (nodeName.equalsIgnoreCase("OBJECT")) {
          	
          		String aName = getNameAttribute(node);
          		rootHash.put(aName, childHash); // should this be childName ?
          		if (debug) System.out.println("put to rootHash: "+nodeName+" "+aName+" childHash %%%%%%%%%%%");
          		Node child = node.getFirstChild();
            	int childCt = 0;
            	String childValue = "";
            	String childName = "";
            	if (child != null) {
                	while (child != null) {
                		
                		convertMetadata(child, childHash, level + 1);
          				child = child.getNextSibling();
                    	
                	} 
            	} 	
          } else if (nodeName.equalsIgnoreCase("PROPERTY")) {
          	
          		String aName = getNameAttribute(node);
          		rootHash.put(aName, childHash); // should this be childName ?
          		if (debug) System.out.println("put to rootHash: "+nodeName+" "+aName+" childHash %%%%%%%%%%%");
          		Node child = node.getFirstChild();
            	int childCt = 0;
            	String childValue = "";
            	String childName = "";
            	if (child != null) {
                	while (child != null) {
                		
                		convertMetadata(child, childHash, level + 1);
          				child = child.getNextSibling();
                    	
                	} 
            	} 	
          } else if (nodeName.equalsIgnoreCase("GROUP")) {
          	
          		String aName = getNameAttribute(node);
          		rootHash.put(aName, childHash); // should this be childName ?
          		if (debug) System.out.println("put to rootHash: "+nodeName+" "+aName+" childHash %%%%%%%%%%%");
          		Node child = node.getFirstChild();
            	int childCt = 0;
            	String childValue = "";
            	String childName = "";
            	if (child != null) {
                	while (child != null) {
                		
                		convertMetadata(child, childHash, level + 1);
          				child = child.getNextSibling();
                    	
                	} 
            	} 	
          
          } else if (nodeName.equalsIgnoreCase("IMAGE")) {
          	
          		String aName = getNameAttribute(node);
          		rootHash.put(aName, childHash); // should this be childName ?
          		if (debug) System.out.println("put to rootHash: "+nodeName+" "+aName+" childHash %%%%%%%%%%%");
          		Node child = node.getFirstChild();
            	int childCt = 0;
            	String childValue = "";
            	String childName = "";
            	if (child != null) {
                	while (child != null) {
                		
                		convertMetadata(child, childHash, level + 1);
          				child = child.getNextSibling();
                    	
                	} 
            	} 	
          
          } else if (nodeName.equalsIgnoreCase("IMAGE_HEADER")) {
          	
          		String aName = getNameAttribute(node);
          		rootHash.put(aName, childHash); // should this be childName ?
          		if (debug) System.out.println("put to rootHash: "+nodeName+" "+aName+" childHash %%%%%%%%%%%");
          		Node child = node.getFirstChild();
            	int childCt = 0;
            	String childValue = "";
            	String childName = "";
            	if (child != null) {
                	while (child != null) {
                		
                		convertMetadata(child, childHash, level + 1);
          				child = child.getNextSibling();
                    	
                	} 
            	} 	
          } else if (nodeName.equalsIgnoreCase("VICAR_LABEL")) {
          	
          	if (debug) System.out.println("nodeName is VICAR_LABEL  "+nodeName+" ******************************");
          		String aName = getNameAttribute(node);
          		rootHash.put(nodeName, childHash); // should this be childName ?
          		if (debug) System.out.println("put to rootHash: "+nodeName+" "+aName+" childHash %%%%%%%%%%%");
          		Node child = node.getFirstChild();
            	int childCt = 0;
            	String childValue = "";
            	String childName = "";
            	if (child != null) {
                	while (child != null) {
                		
                		convertMetadata(child, childHash, level + 1);
          				child = child.getNextSibling();
                    	
                	} 
            	} 	
          } else if (nodeName.equalsIgnoreCase("PDS_LABEL")) {
          	
          		String aName = getNameAttribute(node);
          		rootHash.put(nodeName, childHash); // should this be childName ?
          		if (debug) System.out.println("put to rootHash: "+nodeName+" "+aName+" childHash %%%%%%%%%%%");
          		Node child = node.getFirstChild();
            	int childCt = 0;
            	String childValue = "";
            	String childName = "";
            	if (child != null) {
                	while (child != null) {
                		
                		convertMetadata(child, childHash, level + 1);
          				child = child.getNextSibling();
                    	
                	} 
            	} 	
          } else if (nodeName.equalsIgnoreCase("ISIS_LABEL")) {
          	
          		String aName = getNameAttribute(node);
          		rootHash.put(nodeName, childHash); // should this be childName ?
          		if (debug) System.out.println("put to rootHash: "+nodeName+" "+aName+" childHash %%%%%%%%%%%");
          		Node child = node.getFirstChild();
            	int childCt = 0;
            	String childValue = "";
            	String childName = "";
            	if (child != null) {
                	while (child != null) {
                		
                		convertMetadata(child, childHash, level + 1);
          				child = child.getNextSibling();
                    	
                	} 
            	}
          } else if (nodeName.equalsIgnoreCase("FITS_LABEL")) {
          	
          		String aName = getNameAttribute(node);
          		rootHash.put(nodeName, childHash); // should this be childName ?
          		if (debug) System.out.println("put to rootHash: "+nodeName+" "+aName+" childHash %%%%%%%%%%%");
          		Node child = node.getFirstChild();
            	int childCt = 0;
            	String childValue = "";
            	String childName = "";
            	if (child != null) {
                	while (child != null) {
                		
                		convertMetadata(child, childHash, level + 1);
          				child = child.getNextSibling();
                    	
                	} 
            	} 	  	
          } else if (nodeName.equalsIgnoreCase("item")) {
          
          		String iName = getItemNameAttribute(node);
          		// if (iName.equalsIgnoreCase("ARTICULATION_DEVICE_ANGLE_NAME")) {
          		// if (iName.startsWith("ARTICULATION_DEVICE_ANGLE")) {
	            //   		System.out.println("!!!! iName =  "+iName+"  ######################");
          		// }
          		
          		nodeValue = getNodeValueString(node);
			//                indent(level);
                
                // if (iName.equalsIgnoreCase("ARTICULATION_DEVICE_ANGLE_NAME")) {
                // if (iName.startsWith("ARTICULATION_DEVICE_ANGLE")) {
                // 		System.out.println("!!!!");
	            //    		System.out.println("!!!! nodeName="+nodeName+"  iName="+iName+"  nodeValue="+nodeValue);
	            //   }
                
          		if (debug) System.out.println("put to rootHash: item  "+nodeName +" iName="+iName+" nodeValue="+nodeValue);
          		if (nodeValue != "" && iName != "" && nodeValue != null && iName != null) {
          			rootHash.put(iName, nodeValue);
          		}
          		else {
          			rootHash.put(iName, "NULL");
          			if (debug) System.out.println("**** ERROR *** put to rootHash: item  "+nodeName +" iName="+iName+" nodeValue="+nodeValue);
          		}
				
          } else {
          	// hopefully this will get everything else in the label
          		if (nodeValue == "" || nodeValue == null) {
          			nodeValue = getNodeValueString(node);
          		}
            	
            	if (nodeValue != "" && nodeName != "" && nodeValue != null && nodeName != null) {
            		if (debug) System.out.println("put to rootHash: "+nodeName+" = "+nodeValue+"  %%%%%%%%%%%%%%%%%");
            		rootHash.put(nodeName, nodeValue);
            	}
            }
            // ^IMAGE
            // ^IMAGE_HEADER
            // may still need some way to find something which has 
            // subelements not already identified above ??
          
          
       
    } // end of convertMetadata
    
   
    
    
    /**
     * Given a Node this will check to see if the nodeName is "item"
     * If it is, the attributes will be searched to get the String associated 
     * with the "name" or "key" attribute.     * @param n     * @return String null or the value of the "name" or "key" attribute     */
    
    public String getItemNameAttribute(Node n) {
    	
    		NamedNodeMap cmap = n.getAttributes();
    		String nodeName = n.getNodeName();
    		String attrNodeName = "";
    		String attrNodeValue = "";
    		String itemName = "";
    		if (nodeName.equalsIgnoreCase("item") == false) return itemName;
    		
          	if (cmap != null) {
            	int length = cmap.getLength();
            	for (int i = 0; i < length; i++) {
                	Node attr = cmap.item(i);
                	attrNodeName = attr.getNodeName();
                	attrNodeValue = attr.getNodeValue();
                	// this will be an Attribute nodeType
                	// iioNode.setAttribute(attrNodeName, attrNodeValue);
                		
          			// System.out.println("child attr "+attrNodeName+" "+attrNodeValue);
          			// might also look for "key"
          			if (attrNodeName.equalsIgnoreCase("name") || attrNodeName.equalsIgnoreCase("key")) {
          					itemName = attrNodeValue;
          			}
             	} 
            }
           return itemName;
    }
    
    
    /**
     * Given a Node the attributes will be searched to get the String associated with the
     * "name" or "key" attribute.
     * @param n
     * @return String
     */
    public String getNameAttribute(Node n) {
    	
    		NamedNodeMap cmap = n.getAttributes();
    		String nodeName = n.getNodeName();
    		String attrNodeName = "";
    		String attrNodeValue = "";
    		String itemName = "";
          	if (cmap != null) {
            	int length = cmap.getLength();
            	for (int i = 0; i < length; i++) {
                	Node attr = cmap.item(i);
                	attrNodeName = attr.getNodeName();
                	attrNodeValue = attr.getNodeValue();
                	// this will be an Attribute nodeType
                	// iioNode.setAttribute(attrNodeName, attrNodeValue);
                		
          			// System.out.println("child attr "+attrNodeName+" "+attrNodeValue);
          			// might also look for "key"
          			if (attrNodeName.equalsIgnoreCase("name")) {
          					itemName = attrNodeValue;
          			}
             	} 
            }
           return itemName;
    }
    
    /**
     * Given a Node the attributes will be searched to get the String associated with the
     * "key" attribute.
     * @param n
     * @return String
     */
    public String getKeyAttribute(Node n) {
    	
    		NamedNodeMap cmap = n.getAttributes();
    		String nodeName = n.getNodeName();
    		String attrNodeName = "";
    		String attrNodeValue = "";
    		String itemName = "";
          	if (cmap != null) {
            	int length = cmap.getLength();
            	for (int i = 0; i < length; i++) {
                	Node attr = cmap.item(i);
                	attrNodeName = attr.getNodeName();
                	attrNodeValue = attr.getNodeValue();
                	// this will be an Attribute nodeType
                	// iioNode.setAttribute(attrNodeName, attrNodeValue);
                		
          			// System.out.println("child attr "+attrNodeName+" "+attrNodeValue);
          			// might also look for "key"
          			if (attrNodeName.equalsIgnoreCase("key")) {
          					itemName = attrNodeValue;
          			}
             	} 
            }
            return itemName;
    }
    
  /** 
  * Used to get the value from an "item" or "subitem" node
  * Will look for attributes to create a String with the value
  * assume this is an item with NO subitems OR it is a subitem. <br>
  * Will return null if there is no Value. Caller should assume this means 
  * there are child nodes to walk through.
  * @param node
  * @return String
  * */
   private String getNodeValueString(Node node) {
   	
   		String nodeName = node.getNodeName();
   		String value = "";
   		boolean hasSubItems = false;
   		
   		if (nodeName.equalsIgnoreCase("item") || nodeName.equalsIgnoreCase("subitem") ) {
   			// continue processing
   		} else {
   			// Visit the children recursively
            Node child = node.getFirstChild();
            String childName = "";
            String nodeValue = null;
            String tv = "";
            if (child != null) {
                nodeValue = "";
                // System.out.println(">");
                while (child != null) {
                    int type = child.getNodeType();
                    // displayMetadata(child, level + 1);
                    if (type == ELEMENT_TYPE) {
                    	// check for "subitem" get value from the text node
                    	childName = child.getNodeName();
                    	if (childName.equalsIgnoreCase("subitem") ) {
                    		tv = getNodeValueString(child);
                    		if (tv != null) {
                    			hasSubItems = true;
                    			if (nodeValue == "") {
                    				nodeValue = tv;
                    			}
                    			else {
                    				nodeValue = nodeValue+","+tv ;
                    			}
                    		}
                    		// return null;
                    	}
                    	else {
                    		// tv = getNodeValueString(child);
                    		// if (tv != null) nodeValue = nodeValue + tv;
                    		// return null; 
                    	}
                    }
                    else if (type == TEXT_TYPE) {
                       nodeValue = nodeValue+child.getNodeValue(); 
                    } else if (type == CDATA_TYPE) {
                        nodeValue = nodeValue+child.getNodeValue(); 
                    }
                    child = child.getNextSibling();
                }
                
            } else {
                nodeValue = "??"; // might use null instead
            }
   			return nodeValue;
   		}
   		
   		NamedNodeMap map = node.getAttributes();
   		
   		String nodeValue = "";
   		String attrKey = "";
   		String   attrQuotedValue = null;
   		String attrUnitValue = null;
         if (map != null) {
           	int length = map.getLength();
            for (int i = 0; i < length; i++) {
                Node attr = map.item(i);
                String attrNodeName = attr.getNodeName();
                if (attrNodeName.equalsIgnoreCase("key")) {
                    attrKey = attr.getNodeValue();
                	}
               	else  if (attrNodeName.equalsIgnoreCase("name")) {
                    attrKey = attr.getNodeValue();
                	}
                	
                if (attrNodeName.equalsIgnoreCase("quoted")) {
                    attrQuotedValue = attr.getNodeValue();
               	}
               	
               	if (attrNodeName.equalsIgnoreCase("unit") || attrNodeName.equalsIgnoreCase("units")) {
                    attrUnitValue = attr.getNodeValue();
               	}
             } 
          // get value from the text or CDATA node
         }
         else {
         // do nothing, no attributes
         }
       
        
         // Visit the children recursively
            Node child = node.getFirstChild();
            String childName = "";
            String tv = "";
            if (child != null) {
                nodeValue = "";
                // System.out.println(">");
                while (child != null) {
                    int type = child.getNodeType();
                    // displayMetadata(child, level + 1);
                    if (type == ELEMENT_TYPE) {
                    	// check for "subitem" get value from the text node
                    	childName = child.getNodeName();
                    	if (childName.equalsIgnoreCase("subitem") ) {
                    		tv = getNodeValueString(child);
                    		if (tv != null) {
                    			hasSubItems = true;
                    			if (nodeValue == "") {
                    				nodeValue = tv;
                    			}
                    			else {
                    				nodeValue = nodeValue+","+tv ;
                    			}
                    		}
                    		// return null;
                    	}
                    	else {
                    		// tv = getNodeValueString(child);
                    		// if (tv != null) nodeValue = nodeValue + tv;
                    		// return null; 
                    	}
                    }
                    else if (type == TEXT_TYPE) {
                       nodeValue = nodeValue+child.getNodeValue(); 
                    } else if (type == CDATA_TYPE) {
                        nodeValue = nodeValue+child.getNodeValue(); 
                    }
                    child = child.getNextSibling();
                }
                
            } else {
                nodeValue = "";
            }
        
        // nodeValue is just the text part
        
        // now add any units and quotes
        if (attrQuotedValue != null && attrQuotedValue.equalsIgnoreCase("true") ) {
        	if (hasSubItems) {
        		value = "("+nodeValue+")" ;
        		// subItems will already be properly quoted
        	}
        	else {
        		value = "\""+nodeValue+"\"" ;
        	}
        }
        else {
        	if (hasSubItems) {
        		value = "("+nodeValue+")" ;
        	}
        	else {
        		value = nodeValue;
        	}
        }
        
         if (attrUnitValue != null ) {
         	// something with Units will NEVER have quotes ???
         	// add a space between the value and the <unit>
        	value = value+" <" +attrUnitValue+">";// append the units value (capitalize ???)
        }
        
        
   return value;	
   }
}

