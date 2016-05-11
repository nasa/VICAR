package jpl.mipl.io.util;

import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.util.HashMap;

import jpl.mipl.io.util.*;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.w3c.dom.traversal.DocumentTraversal;
import org.w3c.dom.traversal.NodeFilter;
import org.w3c.dom.traversal.TreeWalker;
import org.w3c.dom.*;

// import jpl.mipl.util.*;

/**
 * This program using tree-walker for
 * non-linear traverse of DOM-document.
 * 
 * prints information about the XPath and variables for the velocity template
 */

public class TreeInfo {
	
	// currently a placeholder. It might be useful to build up a HashMap of the xml tree
	// with the information we want to use in further processing or filtering
	HashMap treeInfoMap;
	
	boolean debug = false;

	public TreeInfo() {
		// TODO Auto-generated constructor stub
	}
	
	
	
	public static void main(String[] args) {
		
		boolean debug = false;
		int level = 0;		
		String outfile = "-";
		int argc = args.length;
		
		if (argc == 0) {
			System.out.println("TreeInfo infile outfile [debug]");
			System.out.println(" infile  - input filename");
			System.out.println(" outfile - output filename, - will print to stdout");
			System.out.println(" debug   - adds debug printing to output, no arg disables debug printing");
			System.exit(0);			
		}
		
		String infile = args[0];
		if (argc > 1) {
			outfile = args[1];
		}
		System.out.println("TreeInfo");
		System.out.printf("# infile = %s, outfile = %s,  argc = %d \n",infile, outfile, argc );
		
		if (argc > 2) {
			if (args[2].equalsIgnoreCase("debug")) {
				debug = true;
				System.out.printf("# debug printing is ON \n");
			}
		}
		
		TreeInfo treeInfo = new TreeInfo();
		treeInfo.setDebug(debug);
		String outs = treeInfo.showTree(infile);
		
		if (outfile.equalsIgnoreCase("-") || outfile.equalsIgnoreCase("none") ||
			outfile.equalsIgnoreCase("stdout") ) {
			System.out.println(outs);
		} else {
			// write to a file
			PrintWriter writer;
			try {
				writer = new PrintWriter(outfile, "UTF-8");
				writer.write(outs);			
				writer.close();
			} catch (FileNotFoundException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (UnsupportedEncodingException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		

	}

	/**
	 * showTree
	 * 
	 * 
	 * @param file
	 * return a String of the output
	 * 
	 */
	public String showTree(String file) {
		
		// create a StringBuffer do sprinfs and add to it??
		// also put everything into a HashMap??
		
		StringBuilder sb = new StringBuilder("select id1, ");
		sb.append("TreeInfo\n");
		
		sb.append("infile =  "+ file+"\n" );
		if (debug) sb.append("debug printing is ON \n" );
		int level = 0;
		
		jpl.mipl.io.util.DOMutils domUtils = new jpl.mipl.io.util.DOMutils();
		try {
		    // load the document from a file:
		    DocumentBuilderFactory factory = 
		      DocumentBuilderFactory.newInstance();
		    DocumentBuilder loader = factory.newDocumentBuilder();
		    Document document = loader.parse(file);

		    // this cast is checked on Apache implementation (Xerces):
		    document.normalizeDocument();
		    DocumentTraversal traversal = (DocumentTraversal) document;

		    TreeWalker walker = traversal.createTreeWalker(
		      document.getDocumentElement(), 
		      NodeFilter.SHOW_ELEMENT, null, true);

		    traverseLevel(walker, "", level + 1, sb);

		  } catch (Exception ex) {
		    ex.printStackTrace();
		  }
		return sb.toString();
	}
	
	
	/**
	 * traverseLevel
	 * 
	 * @param walker
	 * @param indent
	 * @param level
	 * return a String of all the prints
	 */
	
	private void traverseLevel(TreeWalker walker, 
		    String indent, int level, StringBuilder sb) {
		  
			jpl.mipl.io.util.DOMutils domUtils = new jpl.mipl.io.util.DOMutils();
		    // describe current node:
		    Node parend = walker.getCurrentNode();
		   // get value and xPath
		    String nodeValue = parend.getNodeValue();
		    String nodeText = parend.getTextContent();
		    String localName = parend.getLocalName();
		    String nodeName = parend.getNodeName();
		    boolean hasChildren = parend.hasChildNodes();
		    boolean hasAttributes = parend.hasAttributes();
		    String info = "";
		    if (hasChildren) { info += "C+" ;}
		    if (hasAttributes) { info += "A+" ;}
		    
		    		    
		    String namespaceURI = parend.getNamespaceURI();
		    String baseURI = parend.getBaseURI();
		    short nodeType = parend.getNodeType();
		    String nodeTypeS = nodeTypeString(nodeType);
		    
		    if (level == 1) {
		    	sb.append(" baseURI = "+baseURI+"\n" );
		    	sb.append(" namespaceURI = "+namespaceURI+"\n" );
		    	sb.append("\n" );
		    	
		    }
		    
		    // String[] getNodeXPath(Node node, boolean includeAttributes, boolean includeParent )
		    String[] xpaths = domUtils.getNodeXPath(parend, true, true);
		    String xpath = getXPath(parend) ;
		    
		    // find out if this is a single Element or an array
		    // 
		    Document ownerDoc = parend.getOwnerDocument();
		    Node firstChild = ownerDoc.getFirstChild();
	        
		    // find out if the parent is an array
		    
		    int len = xpaths.length;
		    String nodeText2 = nodeText.trim().replaceAll("\n","##");
		    nodeText2 = nodeText2.replaceAll("\\s+","_");
		    
		    // st.replaceAll("\\s+","") removes all whitespaces and non visible characters such as tab, \n .

		    // st.replaceAll("\\s+","") and st.replaceAll("\\s","") produce the same result.
		    
		    // anything with ## in it is not a final ELEMENT, text is inclusive of multiple levels
		    // have a flag from the user to decide if values which are not variables should also be printed
		    if (nodeText2.contains("$") && !nodeText2.contains("##") ) {
		    	/**
		    	System.out.print(indent +level+") "+nodeTypeS+" "+localName+","+nodeName+"," + ((Element) parend).getTagName()+" >"+nodeText2+"< ");
		    	System.out.print("xp["+len+"]");
		    	for (int i=0 ; i<len ; i++) { 
		    		// System.out.println(indent +" "+level+"- " + xpaths[i]);	
		    		System.out.print(">"+xpaths[i]+", ");	
		    	}	
		    	System.out.println(" $$"+level+" "+info);
		    	**/
		    	// System.out.println(" ");
		    	// ((Element) parend).getTagName() and nodeName = parend.getNodeName(); give the same String
		    	
		    	sb.append(indent +" "+level+") "+nodeTypeS+" "+nodeName+" <"+nodeName+">"+nodeText2+"</"+nodeName+"> \n");
		    	sb.append(indent +" "+level+")       source "+nodeName+" "+nodeText2+"  \n");
		    	sb.append(indent +" "+level+")        XPath "+nodeName+" /"+xpath+"\n");
		    	// System.out.println(" $$"+level+" "+info);
		    	// sb.append("\n");
		    	
		    	// check if this is an array of this item
		    	NodeList nl = domUtils.getNodeList(ownerDoc, xpath);
		        int nlen = nl.getLength();
		        
		        // check if parent is also an array. If not we can get all of them from the first instance
		        // see below where the parent is an array7
		        if (debug) sb.append(String.format("%s %d) nlen=%d \n", indent, level, nlen));
		        if (nlen > 1) {
		        	for (int k=0; k< nlen ; k++) {
		        		String array_xpath = String.format("%s[%d]", xpath, k+1);
		        		NodeList nla = domUtils.getNodeList(ownerDoc, array_xpath);
		        		int nalen = nla.getLength();
		        		if (debug) {
		        			sb.append(String.format("%s %d) nalen=%d %s\n", indent, level, nalen, array_xpath));
		        			sb.append(printNode(indent, nla.item(0)));
		        		}
		        	}
		        }
		        
		        // if (nlen > 1) { // we have an array, keep track of index and nodeName (lastNodeName ??)
		        // store in an array which uses the level as the index??
		    	
		        // once I have created a final xpath, test the query to check that it gets what I expect.
		        // xpath should start with //Product_Observartional (not single /)
		    	
		    	int plen = 0;
		    	String parentxp = parentXPath(xpath);
		        if (parentxp != null && parentxp.length() > 0) {
		        		        	
		        	NodeList pnl = domUtils.getNodeList(ownerDoc, parentxp);
		        	plen = pnl.getLength();
		        	if (plen > 1) {
		        		// System.out.printf("%s %d)- %d -- xp %s parentxp %s \n", indent, level, plen, xpath, parentxp);
		        		sb.append(String.format("%s %d) parent XPath /%s \n", indent, level, parentxp));
		        				        		
		        		for (int i=0 ; i<plen ; i++) {
		        			Node n = pnl.item(i);
		        			// System.out.printf("%s %d-%d/%d)P %s %s %s\n",  indent, level, i, plen, n.getNodeName(), parentxp, xpath);	            
		        		}
		        	}
		        }
		    	
		    	
		        
		        if (nlen > 1 && plen > 1) {
		        for (int i=0 ; i<nlen ; i++) {
		            Node n = nl.item(i);
		            // System.out.printf("%s %d-%d/%d)X %s %s\n",  indent, level, i, nlen, n.getNodeName(), xpath);
		            // get the parent of this node. get all the children of this node
		            Node p = n.getParentNode();
		            NodeList cnl = p.getChildNodes() ;
		            int cnlen = cnl.getLength();
		            boolean sameParent = false;
		            for (int j=0 ; j<cnlen ; j++) {
			            Node cn = cnl.item(j);
			            String cnNodeName = cn.getNodeName();
			            String cnodeText = cn.getTextContent();
			            cnodeText = cnodeText.trim().replaceAll("\n","##");
					    cnodeText = cnodeText.replaceAll("\\s+","_");
					    if (cnodeText.equals(nodeText2)) {
					    	sameParent = true;
					    }
		            }
		            
		           
		            for (int j=0 ; j<cnlen ; j++) {
			            Node cn = cnl.item(j);
			            String cnNodeName = cn.getNodeName();
			            String cnodeText = cn.getTextContent();
			            cnodeText = cnodeText.trim().replaceAll("\n","##");
					    cnodeText = cnodeText.replaceAll("\\s+","_");
					    if (cnNodeName.equalsIgnoreCase("#comment") || cnNodeName.equalsIgnoreCase("comment")) {
					    	// comments must be ignored. They may not be valid content in an XPath expresion
					    	if (debug) {
					    		sb.append(String.format("%s %d) %s COMMENT ****************\n", indent, level, cnNodeName));
					    	}
					    }
					    else if (! cnNodeName.equalsIgnoreCase("#text") && sameParent) {
					    	// System.out.printf("%s %d-%d/%d)PC %s = %s \n",  indent, level, j, cnlen, cnNodeName, cnodeText);
					    	sb.append(String.format("%s %d) %s = %s \n",  indent, level, cnNodeName, cnodeText));
					    	// System.out.printf("%s %d) %s[../%s/text()='%s'] \n",  indent, level, xpath, cnNodeName, cnodeText);
					    	
					    	String xp = String.format("%s[../%s/text()='%s']", xpath, cnNodeName, cnodeText) ;
					    	sb.append(String.format("%s %d) %s \n",  indent, level, xp));
					    	// put a try catch block around this. catch an xpath Exception
					    	// don't bomb - show the error
					    	NodeList nlx = domUtils.getNodeList(ownerDoc, xp);
					        int nxlen = nlx.getLength();
					        
					        if (debug) sb.append(String.format("%s %d) nxlen=%d cnlen=%d\n", indent, level, nxlen, cnlen));
					        for (int ii=0 ; ii< nxlen ; ii++) {
					        	sb.append(String.format("%s %d) ",  indent, level));
					        	sb.append(printNode(" i="+ii+" ", nlx.item(ii)));
					        	// sb.append(printNode(indent, nlx.item(ii)));
					        }
		            	}
		            }
		            
		            
		            
		            /**
		            if (nlen > 1) {
		            	Node sibling = n.getNextSibling();
		            	while (sibling != null ) {	
		            		if (!sibling.getNodeName().equals("#text")) {
		            			String t = sibling.getTextContent().trim().replaceAll("\n"," ");
		            			
		            			t = t.replaceAll("\\s+",",");
		            			int ti = t.indexOf(',');
		            			if (ti == -1) {
		            				System.out.printf("%s %d-%d/%d)S %s = %s --------\n",  
		            						indent, level, i, nlen, sibling.getNodeName(), t);
		            			} else {
		            				System.out.printf("%s %d-%d/%d)S %s = [%s] --------\n",  
		            						indent, level, i, nlen, sibling.getNodeName(), t);
		            			}
		            		}
		            		// sibling = n.getNextSibling();	
		            		sibling = sibling.getNextSibling();	
		            	}
		            } 
		            **/
		        }
		        } // nlen > 1
		        
		        sb.append(" ");
		    	// System.out.println(" ");
		    }
		    
		    /**
		    if (nodeText2.contains("$")) {
		    	System.out.println(" $$"+level);
		    } else {
		    	System.out.println(" X"+level);
		    }
		    **/
		    // traverse children:
		    for (Node n = walker.firstChild(); n != null; 
		      n = walker.nextSibling()) {
		      
		      traverseLevel(walker, indent + '\t', level + 1, sb);
		    }

		    // return position to the current (level up):
		    walker.setCurrentNode(parend);

		    // return all the prints in a String
	}
	
	/**
	 * getHashMap
	 * 
	 * If we start putting the useful information into a HashMap it would be returned here
	 * @return
	 */
	public HashMap getHashMap() {
		
		return treeInfoMap;
	}
		  
	// public void printNode(String prefix, Node n) {	
	public String printNode(String prefix, Node n) {
		if (n != null) {
			short nodeType = n.getNodeType();
		    String nodeTypeS = nodeTypeString(nodeType);
		    String nodeName = n.getNodeName();
            String nodeText = n.getTextContent();
            nodeText = nodeText.trim().replaceAll("\n","##");
		    nodeText = nodeText.replaceAll("\\s+","_");
		    // System.out.printf("%s %s %s = %s \n", prefix, nodeTypeS, nodeName, nodeText);
			return String.format("%s %s %s = %s \n", prefix, nodeTypeS, nodeName, nodeText);
		} else {
			return "";
		}
		
	}
	
	/**
	 * parentXPath
	 * strip off the last part of the path
	 * @param xp
	 * @return
	 */
	// public static String parentXPath(String xp)	   {
	public String parentXPath(String xp)	   {
		
		String newXP = xp;
		int slash = newXP.lastIndexOf('/');
		if (slash != -1) {
			newXP = xp.substring(0, slash);
		}
		return newXP;
	}
	
	/**
	 * getXPath
	 * 
	 * @param node
	 * @return
	 */
	// public static String getXPath(Node node) {
	public String getXPath(Node node) {
			return getXPath(node, "");
		}

	/******
	 * getXPath
	 * 
	 * need to be able to identify if this is an array
	 * 2 possible cases:
	 * 1) multiple items like this, just need the index
	 * this element is an array
	 * 2) this is a part of an array of some Element.
	 * now we need to include the other parts of the Element/Structure so we can show which one this belongs to 
	 * parent is an array
	 * @param node
	 * @param xpath
	 * @return
	 */
	// public static String getXPath(Node node, String xpath) {
	public String getXPath(Node node, String xpath) {
		if (node == null) {
			return "";
		}
		String elementName = "";
		if (node instanceof Element) {
			elementName = ((Element) node).getNodeName();
			// getTagName()
		}
		Node parent = node.getParentNode();
		
		if (parent == null) {
			return xpath;
		}
		return getXPath(parent, "/" + elementName + xpath);
	}
		  
	/**
	 * nodeTypeString
	 * return the String name for a nodeType value
	 * @param nodeType
	 * @return
	 */
	public String nodeTypeString(short nodeType) {
		String s = "";
		switch (nodeType) {
			case Node.CDATA_SECTION_NODE:
			s = "CDATA_SECTION_NODE"; break;
			case Node.COMMENT_NODE:
				s = "COMMENT_NODE"; break;
			case Node.ATTRIBUTE_NODE:
				s = "ATTRIBUTE_NODE"; break;
			case Node.ELEMENT_NODE:
				s = "ELEMENT_NODE"; break;
			case Node.TEXT_NODE:
				s = "TEXT_NODE"; break;		
			default :
			s = "?"+nodeType;
		}
		
		return s;
	}
	
	/**
	 * setDebug
	 * set the debug flag to enable/disable debug printing
	 * @param d
	 */
	public void setDebug(boolean d) {		
		debug = d;
	}

}
