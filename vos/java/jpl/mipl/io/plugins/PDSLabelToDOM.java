/**
 * @(#)PDSLabelToDOM.java	1.0 
 *
 * Steve Levoe
 * Jet Propulsion Laboratory
 * Multimission Image Processing Laboratory
 * 2-2001 ImageIO EA2 version
 * 1-2003 JDK1,4 version
 * 4-2003 eliminate dependence on Jakarta oro package
 * use sun String methods instead
 *
 * 2-2015 SRL fixed ^IMAGE parsing
 **/

package jpl.mipl.io.plugins;

import org.w3c.dom.Document;
// import org.w3c.dom.DOMException;
import org.w3c.dom.Element;
import org.w3c.dom.*;

// import javax.xml.parsers.*;

import java.io.IOException;
// import java.util.*;

// import javax.imageio.metadata.*;
// import javax.imageio.*;
import javax.imageio.stream.*;

// import VicarIO stuff
// VicarInputFile  SystemLabel
// import jpl.mipl.io.streams.*;
// import jpl.mipl.io.vicar.*;
import jpl.mipl.io.util.*;


import java.io.*;
// import java.util.Vector;

import java.util.regex.*;
import java.util.HashMap;
import java.util.List;
import java.util.ArrayList;

// import org.apache.oro.text.perl.*;


/**
 * Parses a ISIS image label and creates a Document Object with the values.
 * 
 * Using the JDK 1.4 String Matcher and Pattern methods instead.
 *
 @author Steve Levoe
 @version $Id: PDSLabelToDOM.java
 
 * currently does NOT handle HISTORY part of the label <br>
 * to add HISTORY we need to get the label size, read the whole thing 
 * into a buffer and process the buffer.
 */
public final class PDSLabelToDOM {

  /**
   * Reads an input stream with a ISIS label
   */
  
  
  BufferedReader _bufferedReader = null;
  ImageInputStream _imageInputStream = null;
  DataInput _dataInput = null;
  
  PrintWriter _output = null;
  
  Document _document = null;
  Node currentNode = null;
  String keyTag = "key"; // could also be "name"
  boolean useSubitem = true;
  // boolean useSubitem = false;
  
  String unitsTag = "units"; // could also be "name"
  boolean useUnitsAttribute = true;
  String pushbackLineString = "";
    
  Pattern pdsVersionIdP = Pattern.compile("PDS_VERSION_ID",Pattern.CASE_INSENSITIVE);      
  Pattern odlVersionIdP = Pattern.compile("ODL_VERSION_ID",Pattern.CASE_INSENSITIVE);
  Pattern commentLineP  = Pattern.compile("^/\\*\\p{Print}+\\*/$");
    
  
  boolean debug = false;
  // boolean debug = true;
  
  /**
  * Constructors
  *
  * BufferedReader  and ImageInputStream  both have a readLine()
  * so either will work
  ***/
  public PDSLabelToDOM(BufferedReader input, PrintWriter output) {
    
    if (output == null) {
        _output = new PrintWriter(System.out) ;
    } else {
        _output = output ;
    }
    
    _bufferedReader = input;
    _document = buildDocument();
  }
   
  public PDSLabelToDOM(DataInput input, PrintWriter output) {
    
    if (output == null) {
        _output = new PrintWriter(System.out) ;
    } else {
        _output = output ;
    }
    
    _dataInput = input;
    _document = buildDocument();
  }
  
  public PDSLabelToDOM(ImageInputStream input, PrintWriter output) {
    
    if (output == null) {
        _output = new PrintWriter(System.out) ;
    } else {
        _output = output ;
    }
    _imageInputStream = input;
    _document = buildDocument();
  }
  
  /** 
  * isolates the type of stream being read from
  **/
  private String readLine() {
    String line = null;
    if (debug) {
        System.out.println("readLine() pushbackLineString = "+pushbackLineString);
    }
    if (pushbackLineString.equals("")) {
    	try {
    		if (_bufferedReader != null) {
    			line = _bufferedReader.readLine();
    		} else if (_imageInputStream != null) {
    			line = _imageInputStream.readLine();
    		} else if (_dataInput != null) {
    			line = _dataInput.readLine();
    		}
    	} catch(IOException e) {
    		System.err.println("PDSLabelToDOM Error reading from input: readLine()");
    		e.printStackTrace();
    		// System.exit(1);
    		return line;
    	}  
    } else {
    	line = pushbackLineString;
    	pushbackLineString = "";
    }
   return line; 
  }
  
  
  
  private void pushbackLine(String line)  {
	  pushbackLineString = line;
  }
  
  public void setDebug(boolean d) {
  	debug = d;
  }
  
  public boolean getDebug() {
  	return debug ;
  }
  
  public Document getDocument() {
    return _document;
  }
  
  public void setUseSubitem(boolean f) {
  	useSubitem = f;
  }
  
  public boolean getUseSubitem(boolean f) {
  	return useSubitem ;
  }
  
  public void setUseUnitsAttribute(boolean f) {
  	useSubitem = f;
  }
  
  public boolean getUseUnitsAttribute(boolean f) {
  	return useUnitsAttribute ;
  }
  
  public int getLabelSize() {
  	int labelSize = 0;
  	String labelBuf;
  	return labelSize;	
  }
  
  // nodeLevelHM.put("currentNode", theCurrentNode);
  // nodeLevelHM.put("level", level);
  // private Node processLine(String line, int lineCt, int level, Node currentNode) {
  /**********
   *
   * processLine
   * takes a fully formed line from the parser.
   * creates a node for the content and adds it in
   * return HashMap
   ***************************/
  private HashMap processLine(String line, int lineCt, HashMap nodeLevelHM) {
      Matcher pdsVersionIdM, odlVersionIdM, commentLineM ;
      Element element = null;
      String key = "";
      String value = "";
      String units = "";
      int bytesRead = 0;
      int labelSize = -1; // don't check until labelSize != -1
      int recordBytes = -1;
      int qube = -1;
      
      DOMutils domUtils = new DOMutils();
	  Node currentNode = (Node) nodeLevelHM.get("currentNode");
	  Integer levelInt = (Integer) nodeLevelHM.get("level");
      int level = levelInt.intValue();
	  
	  if (debug) {
          System.out.println(lineCt+") ProcessLine "+level+">"+line+"<");
          System.out.println(lineCt+") ProcessLine    currentNode = "+currentNode+ " ");          
      }
      
      if (debug) {
          String xmlFile = "x"+lineCt+".xml";
          // serializeNode(Node node, String file, String type)
          System.out.println("  serializeNode "+ xmlFile);
          domUtils.serializeNode(currentNode, xmlFile, "xml");
      } 
      
      // Pattern commentP = Pattern.compile("\\Q/*\\E");
      // Pattern sfduP = Pattern.compile("SFDU_LABEL",Pattern.CASE_INSENSITIVE);
      commentLineM = commentLineP.matcher(line);
      pdsVersionIdM = pdsVersionIdP.matcher(line);
      odlVersionIdM = odlVersionIdP.matcher(line);
            
      // level may change depending on what we are processing
      // currentNode may change too
      // put the new values back in the HashMap
      
      // handle all the non = special cases
      String[] sv = line.split("=",2);
      key = "";
      if (sv.length > 0) {  
    	  key = sv[0].trim();
      }
      value = "";
      if (sv.length > 1) {          
          value = sv[1].trim();
      }
          
      if (line.startsWith( "END_OBJECT")) {
          if (debug) System.out.println("END_OBJECT *****************************");
          // close the current object. Move UP one level
          level--;
          if (debug) {
              for (int i=0 ; i< level ; i++) { _output.print(" "); }
              _output.println("</OBJECT>" );
          }          
          // move back up one level
          currentNode = currentNode.getParentNode();          
      }
      else if (line.startsWith( "OBJECT ")) {
          // open a new object, put new items inside this one
          if (debug) {
              for (int i=0 ; i< level ; i++) { _output.print(" "); }
              _output.println("<OBJECT name=\""+value+"\">" );
          }
          level++;
          // add nodes for all this stuff
          element = (Element) _document.createElement(key);
          element.setAttribute("name",value);
          // the xsl to filter unwanted OBJECT nodes is easier if attributes are used
          // use an attribute to carry the OBJECT name instead of a TEXT node
          // Text text = (Text) _document.createTextNode(value);
          // element.appendChild(text);
          if (debug) {
              System.out.println("OBJECT level = "+level+"  ++++++++++++++++");
              System.out.println("  currentNode = "+currentNode );
              System.out.println("  element = "+element );
          }
          currentNode.appendChild(element);
          if (debug) {
              System.out.println(lineCt+")"+level+">appendChild  key="+key+" value="+value+"<  OBJECT");
          }
          currentNode = (Node) element;
      }
      else if (line.startsWith( "END_GROUP")) {
          // close the current object. Move UP one level
          level--;
          if (debug) {
              for (int i=0 ; i< level ; i++) { _output.print(" "); }
              _output.println("</GROUP>" );
          }
          
          if (debug) {
              System.out.println("END_GROUP level = "+level+"  #########");
              System.out.println("currentNode = "+currentNode );
          }
          // move back up one level
          currentNode = currentNode.getParentNode();          
      }
      else if (line.startsWith( "GROUP ")) {
    	  // it is possibel to have an item which is GROUP_SOMETHING
          // open a new object, put new items inside this one
          if (debug) {
              for (int i=0 ; i< level ; i++) { _output.print(" "); }
              _output.println("<GROUP name=\""+value+"\">" );
          }
          level++;
          // add nodes for all this stuff
          element = (Element) _document.createElement(key);
          element.setAttribute("name",value);
          // the xsl to filter unwanted OBJECT nodes is easier if attributes are used
          // use an attribute to carry the OBJECT name instead of a TEXT node
          // Text text = (Text) _document.createTextNode(value);
          // element.appendChild(text);
          if (debug) {
              System.out.println("GROUP level = "+level+"  +++++++++++++++++");
              System.out.println("  currentNode = "+currentNode );
              System.out.println("  element = "+element );
          }
          currentNode.appendChild(element);
          if (debug) {
              System.out.println(lineCt+")"+level+">appendChild  key="+key+" value="+value+"<  3");
          }          
          currentNode = (Node) element;
      }      
      else if (commentLineM.find()) {
          if (debug) {
              System.out.println(lineCt+") ProcessLine COMMENT>"+line);
              // System.out.println("currentNode = "+currentNode+ " # ");
              // _output.println("<comment>"+line+"</comment>" );
          }
          
          element = (Element) _document.createElement("COMMENT");
          Text text = (Text) _document.createTextNode(line);
          element.appendChild(text);
          currentNode.appendChild(element);
          if (debug) {
              System.out.println("  currentNode.appendChild(element); COMMENT ");
          }
      } else if (pdsVersionIdM.find() ) {
          sv = line.split("=");
          if (debug) {
              System.out.println(lineCt+") ProcessLine pdsVersionIdM>"+line);
          }
          if (sv.length > 1) {
              
              key = sv[1];
              
              key = key.replaceFirst("^\\s*","");
              key = key.replaceFirst("(\\s+)$",""); 
              
              value = sv[0];
              value = value.replaceFirst("^\\s*","");
              value = value.replaceFirst("(\\s+)$",""); 
              /****
              if (debug) {
                  _output.println("<?xml version=\"1.0\"?>");
                  _output.println("<!DOCTYPE PDS_LABEL SYSTEM \"pds_label.dtd\">");
                  _output.println("<PDS_LABEL>");
                  _output.println("<"+key+">"+value+"<"+key+">" );
              } 
               ****/
                            
              // add nodes for all this stuff
              element = (Element) _document.createElement(key);
              Text text = (Text) _document.createTextNode(value);
              element.appendChild(text);
              currentNode.appendChild(element);
              if (debug) {
                  System.out.println(lineCt+")"+level+">appendChild  key="+key+" value="+value+"< 1");
              }
          }
          else {
        	  if (debug) {
        		  System.out.println("PDSLabelToDOM PDS_VERSION_ID error line="+line);
        	  }
          }
          
      }
      else if (odlVersionIdM.find() ) {
          sv = line.split("=");
          if (debug) {
              System.out.println(lineCt+") ProcessLine odlVersionIdM>"+line);
          }
          
                    
          if (sv.length > 1) {
              
              key = sv[1];
              
              key = key.replaceFirst("^\\s*","");
              key = key.replaceFirst("(\\s+)$",""); 
              
              value = sv[0];
              value = value.replaceFirst("^\\s*","");
              value = value.replaceFirst("(\\s+)$",""); 
        
              if (debug) {
                  _output.println("<?xml version=\"1.0\"?>");
                  _output.println("<!DOCTYPE ODL_LABEL SYSTEM \"pds_label.dtd\">");
                  _output.println("<ODL_LABEL>");
                  _output.println("<"+key+">"+value+"<"+key+">" );
              } 
              // add nodes for all this stuff
              element = (Element) _document.createElement(key);
              Text text = (Text) _document.createTextNode(value);
              element.appendChild(text);
              currentNode.appendChild(element);
              if (debug) {
                  System.out.println(lineCt+")"+level+">appendChild  key="+key+" value="+value+"< 2");
              }
              
          }
          else {
        	  if (debug) {
        		  System.out.println("PDSLabelToDOM ODL_VERSION_ID error line="+line);
        	  }
          }
          
      } else if (line.indexOf("=") != -1) {
          // need to know if we are inside of a quote ? insideValue
          // output.println("=> "+line);
          // split up the line
          // Vector v = perl.split("/=/", line);
          
          
          // only split on the first =, any others will be inside the value
          sv = line.split("=", 2);
          key = sv[0].trim();
          value = "";
          if (sv.length > 1) {
              value = sv[1].trim();
          } else {
        	  if (debug) {
        		  System.out.println("ERROR ='s but no value line="+line);
        	  }
          }
          
          if (debug) {
              System.out.println(lineCt+") ProcessLine EQUALS >"+line+"<");
              System.out.println(lineCt+")"+level+"> key="+key+" value="+value+"<");
          }
          
              
          String quoted = "false";
          
          // cases
          // if (value.startsWith("(") ) {
          // (v,v,v) 
          // (v <units>,v  <units>,v  <units>) 
          // find units, get the value, remove from the string
          // if (value.startsWith("(\"") ) {
          // ("v","v","v")
          // "ksdfkk"
          if (value.startsWith("\"")) {
              // remove quotes
              // UNITS ???
              if (debug) {
                  System.out.println(lineCt+") ProcessLine QUOTES >"+line);
              }
              
              quoted = "true";
              value = value.replaceAll("\"","");
              element = (Element) _document.createElement("item");
              element.setAttribute(keyTag,key);
              element.setAttribute("quoted",quoted);
              
              Text text = (Text) _document.createTextNode(value);
              
              element.appendChild(text);
              currentNode.appendChild(element);
              if (debug) {
                  System.out.println(lineCt+")"+level+">appendChild  key="+key+" value="+value+"<  \\\"");
              }
          } else if (value.startsWith("(")) {
              if (debug) {
                  System.out.println(lineCt+") ProcessLine MULTI >"+line);
              }
              if(useUnitsAttribute) {
                  units = "";
                  // String units = "";
                  // look for <units> pattern
                  int u1 = value.indexOf("<");
                  int u2 = value.indexOf(">");
                  if (u1 != -1 && u2 != -1 && u1 < u2) {
                      // get the units value, remove all instances from the value string
                      try {
                          units = value.substring(u1+1, u2);
                          
                          // remove all the instances from the value string
                          if (debug) {
                              System.out.println("UNITS units="+units);
                              System.out.println("UNITS value="+value);
                          }
                         
                          // value = value.replaceAll("(\\s*)<\\w+>",""); 
          				  // value = value.replaceAll("(\\s*)<\\p{Print}+>",""); 
                          // comma is \x2C, \3E is >
          				  // value = value.replaceAll("(\\s*)<\\p{Print}+>\\x2C",","); 
          				  value = value.replaceAll("(\\s*)<[\\p{Print}&&[^\\x3E]]+>\\,",","); 
          				  if (debug) System.out.println("UNITS value="+value+" *** 1a ***");
          				  value = value.replaceAll("(\\s*)<\\p{Print}+>\\)",")"); 
                          if (debug) System.out.println("UNITS value="+value+" *** 1b ***");
                      }
                      catch (IndexOutOfBoundsException e) {
                          units = "null";
                          // print an error message ??
                      }
                  }
              }
              else {
                  units = "";
              }
              
              /************************/
              // create the items or items and subitems
              if (useSubitem == false) {
                  if (debug) {
                      System.out.println("     useSubitem = "+useSubitem);
                  }
                  // remove any spaces before or after the backets
                  // value = value.replaceFirst("^\\s*\\(","(");
                  // value = value.replaceFirst("\\)(\\s*)$",")"); 
                  // value = sv[1].trim(); did what we need
                  
                  // remove spaces AND brackets
                  // value = value.replaceFirst("^\\s*\\(","");
                  // value = value.replaceFirst("\\)(\\s+)$",""); 
                  
                  quoted = "false";
                  element = (Element) _document.createElement("item");
                  element.setAttribute(keyTag,key);
                  element.setAttribute("quoted",quoted);
                  
                  if (useUnitsAttribute && !units.equals("")) {
                      element.setAttribute(unitsTag,units);
                  }	
                  
                  
                  
                  // no subitems, just use the backeted value quotes and all     
                  Text text = (Text) _document.createTextNode(value);	                        
                  element.appendChild(text);
                  
                  currentNode.appendChild(element);
                  if (debug) {
                      System.out.println(lineCt+")"+level+">appendChild  key="+key+" value="+value+"<  ( subitem = false");
                  }
                  
                  
              }
              else { // useSubitems == TRUE
                  // remove the brackets
                  if (debug) {
                      System.out.println("     useSubitem = "+useSubitem);
                  }
                  // should only remove start and end () and only if they are not inside of quotes
                  value = value.replaceFirst("\\(","");
                  value = value.replaceFirst("\\)","");
                  
                  // ignore commas inside of quotes
                  // "FF01.IMG",3
                  // find all commas outside of quotes, ignore commas inside of quotes
                  
                  String[] subitems;
                  String subv = "";
                  
                  if (value.indexOf("\"") == -1) {
                      quoted = "false";
                      subitems = value.split(",");
                      if (debug) {
                    	  System.out.println("  useSubitem NOT QUOTED subitems.length="+subitems.length);
                      }
                      for (int i =0; i<subitems.length ; i++) {
                          subv = subitems[i];
                          if (debug) { System.out.println(" subv >"+subv+"<"); }
                      }
                  }
                  else {
                      quoted = "true";
                      // remove any spaces outside of quotes
                      // ", " -> ","   or " , " -> ","  or " ," -> ","
                      value = value.replaceAll("\" *, *\"","\",\"");
                      // can't assume that all values are quoted
                      // subitems = value.split("\",\"");
                      subitems = value_split(value);
                      if (debug) {
                    	  System.out.println("  useSubitem QUOTED subitems.length="+subitems.length);
                    	  System.out.println(" subv Q >"+value+"<<");
                      }
                      for (int i =0; i<subitems.length ; i++) {
                          subv = subitems[i];
                          if (debug) { System.out.println(" subv >"+subv+"<"); }
                      }
                      
                  }
                  
                  element = (Element) _document.createElement("item");
                  element.setAttribute(keyTag,key);
                  element.setAttribute("quoted",quoted);
                  
                  if (useUnitsAttribute && !units.equals("")) {
                      element.setAttribute(unitsTag,units);
                  }
                  
                  // no text if we have subitems       
                  // Text text = (Text) _document.createTextNode(value);	                        
                  // element.appendChild(text);
                  
                  currentNode.appendChild(element);
                  if (debug) {
                      System.out.println(lineCt+")"+level+">appendChild  key="+key+" value="+value+"<  ( subitem = true  subitems.length="+subitems.length);
                  }
                  
                  
                  if (key.startsWith("^IMAGE") && debug) {
                	  System.out.println(" ");
                	  System.out.println(lineCt+")"+level+"> *****************************************");
                	  System.out.println(lineCt+")"+level+"> key = "+key+"<");     	  
                  }
                  
                  if (subitems.length == 1) {
                	  // special case for ^IMAGE pointers
            		  // determine if we have an  offset or a filename
                	  subv = subitems[0];
                	  if (key.startsWith("^IMAGE") && subv.contains("\"") ) {
                		  if (debug) {
                			  System.out.println(lineCt+")"+level+"> subv = "+subv+"<  contains(\") ");
                		  }
                		  // this is the filename, add a default offset
                		  // String[] subitems;
                		  String[] newSubitems = {subv,"0 <BYTES>"};
                		  subitems = newSubitems;
                	  } else {
                		  if (debug) {
                			  System.out.println(lineCt+")"+level+"> subv = "+subv+"<  does NOT contains(\") ");
                		  }
                		  // this is the offset (no filename), That is all we need
                	  }           		  
            	  }
                  
                  subv = "";
                  Element subElement ;
                  for (int i =0; i<subitems.length ; i++) {
                      subv = subitems[i];	               	
                      // remove quotes and spaces before or after quotes
                      // subv = subv.replaceFirst("^\\s*\"","");
                      // subv = subv.replaceFirst("\"(\\s*)$",""); 
                      subv = subv.trim();
                      // value = value.replaceAll("\"","");
                      subv = subv.replaceAll("\"","");
                      subv = subv.replaceAll("\\(","");
                      subv = subv.replaceAll("\\)","");
                      
                      subElement = (Element) _document.createElement("subitem");
                      subElement.setAttribute(keyTag,key);
                      subElement.setAttribute("quoted",quoted);
                      
                      if (useUnitsAttribute && !units.equals("")) {
                          subElement.setAttribute(unitsTag,units);
                      }
                      
                      if (debug) {
                    	  System.out.println(" SUBV >"+subv+"<");
                      }
                      Text text2 = (Text) _document.createTextNode(subv);
                      
                      subElement.appendChild(text2);
                      element.appendChild(subElement);
                  }
              } // end of if useSubitem
              
              
              
          } else { ///////////////// SINGLE value, no quotes
              if (debug) {
                  System.out.println(lineCt+") ProcessLine SINGLE >"+line);
              }
              if (key.matches( "END_OBJECT")) {
                  if (debug) System.out.println("END_OBJECT *****************************");
                  // close the current object. Move UP one level
                  level--;
                  if (debug) {
                      for (int i=0 ; i< level ; i++) { _output.print(" "); }
                      _output.println("</OBJECT>" );
                  }
                  
                  // move back up one level
                  currentNode = currentNode.getParentNode();
                  
              }
              else if (key.matches( "OBJECT")) {
                  // open a new object, put new items inside this one
                  if (debug) {
                      for (int i=0 ; i< level ; i++) { _output.print(" "); }
                      _output.println("<OBJECT name=\""+value+"\">" );
                  }
                  level++;
                  // add nodes for all this stuff
                  element = (Element) _document.createElement(key);
                  element.setAttribute("name",value);
                  // the xsl to filter unwanted OBJECT nodes is easier if attributes are used
                  // use an attribute to carry the OBJECT name instead of a TEXT node
                  // Text text = (Text) _document.createTextNode(value);
                  // element.appendChild(text);
                  if (debug) {
                      System.out.println("OBJECT level = "+level+"  ++++++++++++++++");
                      System.out.println("  currentNode = "+currentNode );
                      System.out.println("  element = "+element );
                  }
                  currentNode.appendChild(element);
                  if (debug) {
                      System.out.println(lineCt+")"+level+">appendChild  key="+key+" value="+value+"<  OBJECT");
                  }
                  currentNode = (Node) element;
              }
              else if (key.matches( "END_GROUP")) {
                  // close the current object. Move UP one level
                  level--;
                  if (debug) {
                      for (int i=0 ; i< level ; i++) { _output.print(" "); }
                      _output.println("</GROUP>" );
                  }
                  
                  if (debug) {
                      System.out.println("END_GROUP level = "+level+"  #########");
                      System.out.println("currentNode = "+currentNode );
                  }
                  // move back up one level
                  currentNode = currentNode.getParentNode();
                  
              }
              else if (key.matches( "GROUP")) {
                  // open a new object, put new items inside this one
                  if (debug) {
                      for (int i=0 ; i< level ; i++) { _output.print(" "); }
                      _output.println("<GROUP name=\""+value+"\">" );
                  }
                  level++;
                  // add nodes for all this stuff
                  element = (Element) _document.createElement(key);
                  element.setAttribute("name",value);
                  // the xsl to filter unwanted OBJECT nodes is easier if attributes are used
                  // use an attribute to carry the OBJECT name instead of a TEXT node
                  // Text text = (Text) _document.createTextNode(value);
                  // element.appendChild(text);
                  if (debug) {
                      System.out.println("GROUP level = "+level+"  +++++++++++++++++");
                      System.out.println("  currentNode = "+currentNode );
                      System.out.println("  element = "+element );
                  }
                  currentNode.appendChild(element);
                  if (debug) {
                      System.out.println(lineCt+")"+level+">appendChild  key="+key+" value="+value+"<  3");
                  }
                  
                  currentNode = (Node) element;
              } else {
                  // assume a single unquoted value
                  //  ksdfkk
                  // UNITS ???
                  if(useUnitsAttribute) {
                      // String units = "";
                      // look for <units> pattern
                      units = "";
                      int u1 = value.indexOf("<");
                      int u2 = value.indexOf(">");
                      if (u1 != -1 && u2 != -1 && u1 < u2) {
                          // get the units value, remove all instances from the value string
                          try {
                              units = value.substring(u1+1, u2);
                              
                              // remove all the instances from the value string
                              if (debug) {
                                  System.out.println("UNITS units="+units);
                                  System.out.println("UNITS value="+value);
                              }
                              
                              // value = value.replaceAll("(\\s*)<\\w+>",""); 
              				  value = value.replaceAll("(\\s*)<\\p{Print}+>",""); 
                              if (debug) System.out.println("UNITS value="+value+" *** 2 ***");
                          }
                          catch (IndexOutOfBoundsException e) {
                              units = "null";
                              // print an error message ??
                          }
                      }
                  }
                  else {
                      units = "";
                  }
              
                  quoted = "false";
                  element = (Element) _document.createElement("item");
                  element.setAttribute(keyTag,key);
                  element.setAttribute("quoted",quoted);
                  if (useUnitsAttribute && !units.equals("")) {
                      element.setAttribute(unitsTag,units);
                  }
                  Text text = (Text) _document.createTextNode(value);
              
                  element.appendChild(text);
                  currentNode.appendChild(element);
                  if (debug) {
                      System.out.println(lineCt+")"+level+">appendChild  key="+key+" value="+value+"< units="+units+" SINGLE");
                  }
                  
                  if (key.equalsIgnoreCase("qube") || key.equalsIgnoreCase("^qube") ) { // "^QUBE"
                      qube = Integer.parseInt(value);
                      if (recordBytes != -1) {
                          labelSize = qube * recordBytes;
                          if (debug) {
                              System.out.println("labeSize "+labelSize);
                          }
                      }
                  }
                  else if (key.equalsIgnoreCase("record_bytes")) {
                      recordBytes = Integer.parseInt(value);                      
                  }
              }
              
          }
          
          
          
          
      } else if (line.equals("END")) {
          // is there anything we need to do??
    	  if (debug) {
    		  System.out.println("END line="+line);
    	  }
      }else {
    	  if (debug) { 
    		  System.out.println("Why Are we here?? line="+line);
    	  }
      }
      
      
      
      nodeLevelHM.put("currentNode", currentNode);
      nodeLevelHM.put("level", new Integer(level) );
      return nodeLevelHM;
  }
  
  
  /***
   * value_split
   * Break a value into an array.
   * Commas separate elements, Commas inside of a quote are ignore.
   * Not all values may be quoted
   * @param v
   * @return
   */
  String[] value_split(String v) {
	  
	  // create a data structure we can add elements to
	  // then at the end dump to an array of Strings
	  List<String> strArrayList = new ArrayList<String>();
	  StringBuilder sb = new StringBuilder();
	  
	  // Loop thru the value
	  Boolean inQuote = false;
	  char cb[] = v.toCharArray();
	  int cbLen = cb.length;
	  int vLen = v.length();
	  if (debug) {
		  System.out.println("value_split vLen="+vLen+", cbLen="+cbLen +" >"+v+"<");
	  }
	  char c ;
	  int sCt = 0;
	  for (int i=0 ; i<cbLen; i++) {
		  c = cb[i];
		  if (c == '\"') {
			  // decide if we are inside a String or not. Don't include the quote char
			 if (inQuote) {
				 inQuote = false;
			 } else {
				 inQuote = true;
			 }
		  } else if (c == ',') {
			if (inQuote) {
				sb.append(c); // just another character in the string
			} else {
				// end of the subString, add it to the list, start a new StringBuilder
				strArrayList.add(sb.toString());
				sb = new StringBuilder(); 
				sCt++;
			}
		  } else if (c == ' ') {
			if (inQuote) {
				sb.append(c); // just another character in the string
			} else {
				// ignore, Isn't supposed to happen, just in case				
			}
		  } 
		  else {
			sb.append(c);	  
		  }		  
	  }
	  // add the last string (should only be if it wasn't quoted)
	  if (sb.length() > 0) {
		  strArrayList.add(sb.toString());
		  sCt++;
	  }
	  int strListLen = strArrayList.size();
	  if (debug) {
		  System.out.println("value_split vLen="+vLen+", cbLen="+cbLen +" sCt="+sCt+" strListLen="+strListLen);
	  }
	  
	  String[] vals = strArrayList.toArray(new String[strArrayList.size()]);
	  
	  int valsLen = vals.length;
	  if (debug) {
		  System.out.println("value_split valsLen="+valsLen+" ");
	  
		  for (int i=0 ; i< valsLen ; i++) {
			  System.out.println(">"+vals[i]+"<");
		  }
	  }
	  return vals;
  }
    
  /*
   * countStrInStr
   * count the number of times toFind appears in s
   * returns the count
   ************************/
  int countStrInStr(String toFind, String s) {
	  int ct = 0;
	  int pos = 0;
	  int i = 0;
	  int slen = s.length();
	  int flen = toFind.length();
	  // System.out.println("countStrInStr slen="+slen+" "+toFind+" | "+s);
	  while (pos < slen) {
		  // System.out.println("  i="+i+"  pos="+pos+" ct="+ct);
		  i = s.indexOf(toFind, pos);
		  if (i == -1) {
			  break;
		  } else {
			  ct++;
			  pos = i+1;
		  }
		  
	  }
	  
	  // System.out.println("# i="+i+"  pos="+pos+" ct="+ct);
	  return ct;
	  
  }
  
  private String joinLines(String line1, String line2) {
	  String aLine = "";
      // look at the content of the lines and decide how they should be joined
	  int qCt = countStrInStr("\"", line1);
	  int line1Len = line1.length();
	  if (debug) {
          System.out.println("joinLines qCt="+qCt+">"+line1+"<>"+line2+"<");
	  }
	  
	  // count quotes odd in a quote, even outside (do I care??)
	  if (line1.endsWith("-")) {
		  // remove the ending -
		  aLine = line1.subSequence(0,  line1Len-2)+line2;
	  } else if (line2.startsWith(",") ) {
		  aLine = line1+line2;			  
  	  } else if (line2.startsWith("\"") ) {
		  aLine = line1+line2;			  
  	  } else if (qCt == 1){
	     aLine = line1+" "+line2;
      } else {
 	     aLine = line1+line2;
       }
	  // do add a space, otherwise add one space
	  // look for - at end of a line, remove it, no spaces
	  // look for \w\s+,   remove any spaces
	   // commas space
	  // add a space after a comma
      // for now just do it
      
	  
	  return aLine;
  }
     
  /** 
  * Read the data in from the header and create the Document
  *
  * This has replaced what is now buildDocumentOld
  * The code has been reorganized
  **/
  public Document  buildDocument() {
    // Perl5Util perl = new Perl5Util();

    String key = "";
    String value = "";
    String v1 = "";
    String line = "";
    String trLine = "";
    String units = "";
    int startPosP = 0;
    int endPosP = 0;
    int startPosQ = 0;
    int endPosQ = 0;
    boolean inQuote = false;
    boolean inValue = false;
    boolean inParen = false;
    int level = 0;
    
    Element element = null;
    Element item = null;
    int bytesRead = 0;
    int labelSize = -1; // don't check until labelSize != -1
    int recordBytes = -1;
    int qube = -1;
    boolean keepReading = false;
    
    
    /***
    _document = new org.apache.xerces.dom.DocumentImpl();
    **/
    DOMutils domUtils = new DOMutils();
    _document = domUtils.getNewDocument();
    // this document already has _documentName in it as its root Element
                
    Element root = (Element) _document.createElement(PDSMetadata.nativeImageMetadataFormatName); 
          
    _document.appendChild(root);
    currentNode = root; // always appendChild to the currentNode
          
     // create the patterns we want to look for
     Pattern nonEmptyLineP = Pattern.compile("\\S");
     
     Pattern emptyLineP = Pattern.compile("\\s");
     // Pattern commentP = Pattern.compile("/\\*"); // "\Q/*\E"
     Pattern commentP = Pattern.compile("\\Q/*\\E");
     // Pattern sfduP = Pattern.compile("SFDU_LABEL",Pattern.CASE_INSENSITIVE);
     Pattern pdsVersionIdP = Pattern.compile("PDS_VERSION_ID",Pattern.CASE_INSENSITIVE);
     
     Pattern odlVersionIdP = Pattern.compile("ODL_VERSION_ID",Pattern.CASE_INSENSITIVE);
      
     // look for CHARS spaces = (nothing else)
     // common character classes: 
     // \S non-whitespace
     // \s whitespace
     // ^ start, $ end line
     // ^ is negation inside a [^ ] char class
     // could have one for key = value, another for only keyword =
     // Pattern keywordSpaceEqualsValueLineP = Pattern.compile("[A-Z_]+\\s+=\\s+\\w+");
     Pattern keywordSpaceEqualsValueLineP = Pattern.compile("[A-Z_0-9]+\\s+=\\s+\\p{Print}+");
     Pattern keywordSpaceEqualsLineP      = Pattern.compile("[A-Z_0-9]+\\s+=");
     
     // this is the only thing on the line
     // Pattern commentLineP = Pattern.compile("^/\\*\\S+\\*/$");
     Pattern commentLineP = Pattern.compile("^/\\*\\p{Print}+\\*/$");
      
     // trim front and back of string
     String pq1 = "\\(\\s*\"";
     Pattern openParenSpaceQuoteP = Pattern.compile(pq1);
     // starts with
     String pq2 = "\"\\s*\\)";
     Pattern endParenSpaceQuoteP = Pattern.compile(pq2);
     
     String qcq = "\"\\s*,\\s*\"";
     Pattern quoteCommaQuoteP = Pattern.compile(qcq);
     
     // replace String p = "\\(\\s+\""; 
     
     // create a bunch of Matchers 
     Matcher nonEmptyLineM, commentM, pdsVersionIdM, odlVersionIdM;
     Matcher openParenSpaceQuoteM, endParenSpaceQuoteM, quoteCommaQuoteM;
     Matcher keywordSpaceEqualsLineM, keywordSpaceEqualsValueLineM, commentLineM ;
      
      /***
       reorganize the parser
       while((line = readLine()) != null) {
       
       collect a complete line
       
       empty line
       comment
       look for char space =
       keep adding until a line with char space = is found or /* comment * /
       be careful of = inside of a quoted value
       trim the line first
        nonEmptyLineM = nonEmptyLineP.matcher(line);
         ( push back the line ?
          send the string to a sub which parses it
          get back key, value (value is all nicely trimmed)
            create the element inside this? 
              args include current level currentNode
       
       look for "END"
       
       
       }
       
       
       ****/
      int lineCt = 0;
      level = 1;
      String collectedLine = "";
      inValue = false;
      boolean keepReadingInner = false;
      boolean keepReadingOuter = true;
      String s = "";
      String strim = "";
      
      HashMap nodeLevelHM = new HashMap();
      Integer levelInt ;
      
      nodeLevelHM.put("currentNode", currentNode);
      nodeLevelHM.put("level", new Integer(level));
      
      while((line = readLine()) != null) {
          // is /CR already stripped by readLine() ????
          bytesRead += line.length();
          units = "";
          lineCt++;
          trLine = line.trim();
          levelInt = (Integer) nodeLevelHM.get("level");
          level = levelInt.intValue();
          if (debug) {
              System.out.println(lineCt+")"+level+">"+bytesRead+ " - "+line);
              System.out.println(lineCt+") inValue = "+inValue+" trLine ="+trLine+"<");
          }
          
          nonEmptyLineM = nonEmptyLineP.matcher(line);
          commentLineM = commentLineP.matcher(trLine);
          keywordSpaceEqualsLineM = keywordSpaceEqualsLineP.matcher(trLine);
          keywordSpaceEqualsValueLineM = keywordSpaceEqualsValueLineP.matcher(trLine);
          if (nonEmptyLineM.find()) {
        	                    
                  // if (trLine.equals("END")) {
               if (trLine.equals("END") ) {
                      
                	  if (debug) { System.out.println(lineCt+")  ####### END trLine >"+trLine+"<");}
                      // close the xml document
                      nodeLevelHM = processLine(collectedLine, lineCt, nodeLevelHM);
                      collectedLine = "";
                      inValue = false;
                      keepReadingInner = false;
                      keepReadingOuter = false;
                  } else if (trLine.equals("END_GROUP") || trLine.equals("END_OBJECT")) {
                      
                	  if (debug) { 
                		  System.out.println(lineCt+")  ####### END_GROUP or END_OBJECT trLine >"+trLine+"<");
                		  
                	  }
                      // close the xml document
                      nodeLevelHM = processLine(collectedLine, lineCt, nodeLevelHM);
                      collectedLine = trLine;
                      inValue = false;
                  }
                  else if (commentLineM.find()) {
                	  if (debug) { System.out.println(lineCt+")  ####### commentLine trLine >"+trLine+"<"); }
                      // the previous line is complete
                      nodeLevelHM = processLine(collectedLine, lineCt, nodeLevelHM);
                      // start a new line
                      collectedLine = trLine;
                      // could also process now, set collectedLine = "";
                      inValue = false;
                  }
                  else if (keywordSpaceEqualsValueLineM.find()) {
                      
                	  int qCt = countStrInStr("\"", trLine);
                	  int qCt2 = countStrInStr("\"", collectedLine);
                	  if (debug) { 
                		  System.out.println(lineCt+")  ####### keywordSpaceEqualsValue qCt="+qCt+" qCt2="+qCt2+"  inValue = "+inValue+"  trLine >"+trLine+"<"); 
                		  System.out.println(lineCt+")  ####### keywordSpaceEqualsValue collectedLine ="+collectedLine+"<"); 
                		  }
               	                 	 
                	  if (inValue) {
                		  // trLine is a key and the start of a value
                		  // process the previous line
                		  collectedLine = joinLines(collectedLine, trLine);
                	  } else {
                		  nodeLevelHM = processLine(collectedLine, lineCt, nodeLevelHM);
                		  collectedLine = trLine;
                	  }
                      
                	  qCt = countStrInStr("\"", trLine);
                	  qCt2 = countStrInStr("\"", collectedLine);
                	  if (debug) { 
                		  System.out.println(lineCt+")  ####### keywordSpaceEqualsValue qCt="+qCt+" qCt2="+qCt2+"  ");
                	  }
                      if (qCt2 == 1 ) {
                		  inValue = true;
                	  } else {
                      	inValue = false; 
                	  }
                	                       
                  } 
                  else if (keywordSpaceEqualsLineM.find()) {
                      
                	  if (debug) { System.out.println(lineCt+")  ####### keywordSpaceEquals trLine >"+trLine+"<"); }
                      // the previous line is complete
                      nodeLevelHM = processLine(collectedLine, lineCt, nodeLevelHM);
                      // start a new line
                      collectedLine = trLine;
                      inValue = false;
                  } 
                  else {			
                      
                      
                	  if (debug) { System.out.println(lineCt+")  ####### No Match trLine >"+trLine+"<"); }
                   // collectedLine += s;
                      collectedLine = joinLines(collectedLine, trLine);
                      int startParenCt = countStrInStr("(", collectedLine);
                      int endParenCt = countStrInStr(")", collectedLine);
                      
                      int firstQuoteIndex = collectedLine.indexOf('"');
                      int lastQuoteIndex = collectedLine.lastIndexOf('"');
                      int firstParenIndex = collectedLine.indexOf('(');
                      int lastParenIndex = collectedLine.lastIndexOf(')');
                      
                	  int qCt = countStrInStr("\"", collectedLine);
                      if (debug) { 
                    	  System.out.println(lineCt+")  startParenCt = "+startParenCt+"   endParenCt = "+endParenCt+"  qCt = "+qCt); 
                    	  System.out.println(lineCt+")  firstParenIndex = "+firstParenIndex+"   firstQuoteIndex = "+firstQuoteIndex+" "); 
                    	  System.out.println(lineCt+")  lastParenIndex = "+lastParenIndex+"   lastQuoteIndex = "+lastQuoteIndex+" "); 
                    	  System.out.println(lineCt+")  >"+trLine+"<"); 
                    	  System.out.println(lineCt+")  >"+collectedLine+"<");                     	  
                    	  }
                      
                      
                      if (endParenCt == 1) {
                    	  nodeLevelHM = processLine(collectedLine, lineCt, nodeLevelHM);
                    	  // start a new line
                    	  collectedLine = "";
                    	  inValue = false;
                      } else if (startParenCt == 0 && qCt == 2) {
                    	  // closed the quote and no parens
                    	  nodeLevelHM = processLine(collectedLine, lineCt, nodeLevelHM);
                    	  // start a new line
                    	  collectedLine = "";
                    	  inValue = false;
                      } else if (qCt == 2 && (startParenCt > 0 && endParenCt > 0 && 
                    		  		firstParenIndex>firstQuoteIndex && lastParenIndex<lastQuoteIndex)) {
                    	  // parens inside of quotes. Could call processLine here. Should be terminated by the next line read 
                    	  if (debug) { 
                    		  System.out.println(lineCt+") PARENS INSIDE OF ALL QUOTES ################################");   
                    		  System.out.println(lineCt+")  >"+collectedLine+"<"); 
                    	  }
                  	  } else {
                    	  inValue = true; 
                      }
                  }
                  
                  if (debug) {
                      System.out.println(" "+lineCt+")  s ="+s+"<");
                      System.out.println(" "+lineCt+")  collectedLine ="+collectedLine+"<");
                      System.out.println(" "+lineCt+")  inValue = "+inValue+"  keepReadingInner = "+keepReadingInner+" keepReadingOuter = "+keepReadingOuter);
                  }
            }
          
          
      if (keepReadingOuter == false) break;   
      } // end of the while
      
     /************************************************************/
      if (debug) {
          System.out.println("buildDocument EXIT");
      }
      return  _document;
              
     }
              
  /** 
  * Read the data in from the header and create the Document
  *
  * This function has been replaced
  * I haven't deleted yet
  **/
  public Document  buildDocumentOld() {
    // Perl5Util perl = new Perl5Util();

    String key = "";
    String value = "";
    String v1 = "";
    String line = "";
    String trLine = "";
    String units = "";
    int startPosP = 0;
    int endPosP = 0;
    int startPosQ = 0;
    int endPosQ = 0;
    boolean inQuote = false;
    boolean inValue = false;
    boolean inParen = false;
    int level = 0;
    
    Element element = null;
    Element item = null;
    int bytesRead = 0;
    int labelSize = -1; // don't check until labelSize != -1
    int recordBytes = -1;
    int qube = -1;
    boolean keepReading = false;
    
    
    /***
    _document = new org.apache.xerces.dom.DocumentImpl();
    **/
    DOMutils domUtils = new DOMutils();
          _document = domUtils.getNewDocument();
          // this document already has _documentName in it as its root Element
                
          Element root = (Element) _document.createElement(PDSMetadata.nativeImageMetadataFormatName); 
          
          _document.appendChild(root);
          currentNode = root; // always appendChild to the currentNode
          
     // create the patterns we want to look for
     Pattern nonEmptyLineP = Pattern.compile("\\S");
     
     Pattern emptyLineP = Pattern.compile("\\s");
     // Pattern commentP = Pattern.compile("/\\*"); // "\Q/*\E"
     Pattern commentP = Pattern.compile("\\Q/*\\E");
     // Pattern sfduP = Pattern.compile("SFDU_LABEL",Pattern.CASE_INSENSITIVE);
     Pattern pdsVersionIdP = Pattern.compile("PDS_VERSION_ID",Pattern.CASE_INSENSITIVE);
     
     Pattern odlVersionIdP = Pattern.compile("ODL_VERSION_ID",Pattern.CASE_INSENSITIVE);
      
     // look for CHARS spaces = (nothing else)
     // common character classes: 
     // \S non-whitespace
     // \s whitespace
     // ^ start, $ end line
     // ^ is negation inside a [^ ] char class
     // could have one for key = value, another for only keyword =
     // Pattern keywordSpaceEqualsValueLineP = Pattern.compile("[A-Z_]+\\s+=\\s+\\w+");
     Pattern keywordSpaceEqualsValueLineP = Pattern.compile("[A-Z_0-9]+\\s+=\\s+\\p{Print}+");
     Pattern keywordSpaceEqualsLineP      = Pattern.compile("[A-Z_0-9]+\\s+=");
     
     // this is the only thing on the line
     // Pattern commentLineP = Pattern.compile("^/\\*\\S+\\*/$");
     Pattern commentLineP = Pattern.compile("^/\\*\\p{Print}+\\*/$");
      
     // trim front and back of string
     String pq1 = "\\(\\s*\"";
     Pattern openParenSpaceQuoteP = Pattern.compile(pq1);
     // starts with
     String pq2 = "\"\\s*\\)";
     Pattern endParenSpaceQuoteP = Pattern.compile(pq2);
     
     String qcq = "\"\\s*,\\s*\"";
     Pattern quoteCommaQuoteP = Pattern.compile(qcq);
     
     // replace String p = "\\(\\s+\""; 
     
     // create a bunch of Matchers 
     Matcher nonEmptyLineM, commentM, pdsVersionIdM, odlVersionIdM;
     Matcher openParenSpaceQuoteM, endParenSpaceQuoteM, quoteCommaQuoteM;
     Matcher keywordSpaceEqualsLineM, keywordSpaceEqualsValueLineM, commentLineM ;
      
      /***
       reorganize the parser
       while((line = readLine()) != null) {
       
       collect a complete line
       
       empty line
       comment
       look for char space =
       keep adding until a line with char space = is found or /* comment * /
       trim the line first
        nonEmptyLineM = nonEmptyLineP.matcher(line);
         ( push back the line ?
          send the string to a sub which parses it
          get back key, value (value is all nicely trimmed)
            create the element inside this? 
              args include current level currentNode
       
       look for "END"
       
       
       }
       
       
       ****/
      int lineCt = 0;
      String s = "";
      String strim = "";
       
     lineCt = 0;
      while((line = readLine()) != null) {
      	// is /CR already stripped by readLine() ????
      	bytesRead += line.length();
      	units = "";
      	lineCt++;
      	if (debug) {
      		System.out.println(lineCt+")"+level+">"+bytesRead+ " - "+labelSize+" "+line);
      		System.out.println("   currentNode = "+currentNode+ " ### ");
      		String xmlFile = "x"+lineCt+".xml";
      		// serializeNode(Node node, String file, String type)
      		System.out.println("  serializeNode "+ xmlFile);
      		domUtils.serializeNode(currentNode, xmlFile, "xml");
      		} 
      	// if (lineCt > 30) {
		// 	return _document;
      	// }
	    
	    // if (line.matches( "/\\S/")) {
	    // pattern = Pattern.compile(REGEX,Pattern.CASE_INSENSITIVE);
	    
	    // create the matcher for all possible cases (This is really stupid)
	    // then try a bunch until I find the correct case
	    
      	
	    nonEmptyLineM = nonEmptyLineP.matcher(line);
	    
	    
	    if (nonEmptyLineM.find()) {
	    // if (line.matches( "\\S+")) {
	    // if (line.find( "\\S")) {
	        // System.out.println("*********** nonEmptyLine " +line);
	       
	        // sfduM = sfduP.matcher(line);
	        pdsVersionIdM = pdsVersionIdP.matcher(line);
	        odlVersionIdM = odlVersionIdP.matcher(line);
        
	        int c1 = line.indexOf("/");
	        int c2 = line.indexOf("*");
	        // System.out.println("  ----- " +c1+" "+c2+" "+line);
	        
	        
	        // if (commentM.find()) {
	        if (c1 != -1 && c2 != -1 && c2 == (c1 + 1)) {
	            if (debug) {
	            	System.out.println("currentNode = "+currentNode+ " # ");
	            	_output.println("<comment>"+line+"</comment>" );
	            }
	            // System.out.println("<comment>"+line+"</comment>" );
	            element = (Element) _document.createElement("COMMENT");
	            Text text = (Text) _document.createTextNode(line);
	            element.appendChild(text);
	            currentNode.appendChild(element);
                if (debug) {
                    System.out.println("  currentNode.appendChild(element); COMMENT ");
                }
	        }
	        else if (pdsVersionIdM.find() ) {
	        	// System.out.println("find SFDU_LABEL " +line);
	        	String[] sv = line.split("=");
	           
	           
	           if (sv.length > 1) {
	               
	               key = sv[1];
	                
	               key = key.replaceFirst("^\\s*","");
	               key = key.replaceFirst("(\\s+)$",""); 
	               
	               value = sv[0];
	               value = value.replaceFirst("^\\s*","");
	               value = value.replaceFirst("(\\s+)$",""); 
	               
	               // System.out.println("<SFDU_LABEL>"+value+"</SFDU_LABEL>" );
	               
	               if (debug) {
	               	_output.println("<?xml version=\"1.0\"?>");
                   	_output.println("<!DOCTYPE PDS_LABEL SYSTEM \"pds_label.dtd\">");
                   	_output.println("<PDS_LABEL>");
	               	_output.println("<"+key+">"+value+"<"+key+">" );
	               } 
	               /* document is already create with ISIS_LABEL as the root ?? **        
	               // add nodes for all this stuff
	               element = (Element) _document.createElement("ISIS_LABEL");
	               currentNode.appendChild(element);
	               currentNode = (Node) element;
	               ****/
	                        
	               // add nodes for all this stuff
	               element = (Element) _document.createElement(key);
	               Text text = (Text) _document.createTextNode(value);
	               element.appendChild(text);
	               currentNode.appendChild(element);
                   if (debug) {
                       System.out.println(lineCt+")"+level+">appendChild  key="+key+" value="+value+"< 1");
                   }
	           }
	           else {
	           	System.out.println("PDSLabelToDOM PDS_VERSION_ID error line="+line);
	           }
	                        
	        }
	        else if (odlVersionIdM.find() ) {
	        	// System.out.println("find SFDU_LABEL " +line);
	        	String[] sv = line.split("=");
	           
	           
	           if (sv.length > 1) {
	               
	               key = sv[1];
	                
	               key = key.replaceFirst("^\\s*","");
	               key = key.replaceFirst("(\\s+)$",""); 
	               
	               value = sv[0];
	               value = value.replaceFirst("^\\s*","");
	               value = value.replaceFirst("(\\s+)$","");
	               
	               if (debug) {
	               	_output.println("<?xml version=\"1.0\"?>");
                   	_output.println("<!DOCTYPE ODL_LABEL SYSTEM \"pds_label.dtd\">");
                   	_output.println("<ODL_LABEL>");
	               	_output.println("<"+key+">"+value+"<"+key+">" );
	               } 
	               /* document is already created with PDS_LABEL as the root ?? **        
	               // add nodes for all this stuff
	               element = (Element) _document.createElement("PDS_LABEL");
	               currentNode.appendChild(element);
	               currentNode = (Node) element;
	               ****/
	                        
	               // add nodes for all this stuff
	               element = (Element) _document.createElement(key);
	               Text text = (Text) _document.createTextNode(value);
	               element.appendChild(text);
	               currentNode.appendChild(element);
                   if (debug) {
                       System.out.println(lineCt+")"+level+">appendChild  key="+key+" value="+value+"< 2");
                   }
                   
	           }
	           else {
	           	System.out.println("PDSLabelToDOM ODL_VERSION_ID error line="+line);
	           }
	                        
	        } 
	        else if (line.indexOf("=") != -1) {
	        	// need to know if we are inside of a quote ? insideValue
	            // output.println("=> "+line);
	            // split up the line
	            // Vector v = perl.split("/=/", line);
	           String[] sv = line.split("=");
	           String quoted = "false";
	           String v1trim = "";
	           inValue = false;
	           // if (sv.length ==  1) {
	           	// System.out.println("#############################################");
	           if (debug) {
	        	System.out.print("line includes = ");
	           	System.out.print("sv.length = "+sv.length);
	           	System.out.println(" sv[0]="+sv[0]);
	           }
	           
	           if (sv.length > 1) {
	               // key = (String) v.elementAt(0);
	               key = sv[0];
	               
	               // key = key.substitute("s/^\\s*//"); 
	               key = key.replaceFirst("^\\s*","");
	               key = key.replaceFirst("(\\s+)$",""); 
	               
	               v1 = sv[1];
	               v1trim = v1.trim();
               } else {
                   key = sv[0];
	               
	               // key = key.substitute("s/^\\s*//"); 
	               key = key.replaceFirst("^\\s*","");
	               key = key.replaceFirst("(\\s+)$",""); 
	               
	               v1 = "";
	               v1trim = "";
                   
               }
	               
                int v1Len = v1.length();
                int v1trimLen = v1trim.length();
                if (debug) {
                    System.out.println("         ==      <v1 "+v1.length()+">"+v1+"<");
                    System.out.println("         ==  <v1trim "+v1trim.length()+">"+v1trim+"<");
                    System.out.println("         inValue "+inValue);           	   
                }
                
                if (v1trim.length()  == 0) {
                    v1 = readLine();
                    if (debug) {
                        System.out.println("$$$$$ new v1 "+v1);
                    }
                    v1trim = v1.trim();
                }
	               
                openParenSpaceQuoteM = openParenSpaceQuoteP.matcher(v1trim);
                if (v1trim.startsWith("(") ) {
                    if (debug) {
                        System.out.println("  startsWith(\"(\") key="+key+"<" + v1trim);
                        // System.out.println("  "+lineCt+") key ="+key+"<");
                        // System.out.println("  value ="+value+"<");
                    }
                }
                // odlVersionIdM = odlVersionIdP.matcher(line);
                // maybe only ltrim() ltrim doesn't exist
                // ltrim = key = key.replaceFirst("^\\s*","");
                // ltrim = key = key.replaceFirst("^(\\s*)","");
	               
                if (openParenSpaceQuoteM.find()) {
                    if (debug) {
                        System.out.println("  startsWith(\"(\") key="+key+"<" + v1trim);
                        System.out.println("  ####### openParenSpaceQuote v1     >"+v1+"<");
                        System.out.println("  ####### openParenSpaceQuote v1trim >"+v1trim+"<");
                    }
                // get rid of any spaces
                //  Matcher openParenSpaceQuoteM, endParenSpaceQuoteM, quoteCommaQuoteM ;
                endParenSpaceQuoteM = endParenSpaceQuoteP.matcher(v1trim);
                value = v1trim;
                if (endParenSpaceQuoteM.find()) {
                	if (debug) {
                		System.out.println("  ####### endParenSpaceQuote v1     >"+v1+"<");
                		System.out.println("  ####### endParenSpaceQuote v1trim >"+v1trim+"<");
                		System.out.println("  value ="+value+"<");
                	}      
                    // get rid of any spaces
                } else {
                    inValue = true;
                    keepReading = true;
                    s = "";
                    strim = "";
		                   
                    StringBuffer sb=new StringBuffer();
                    // we don't have the complete value, it is on more than one line
                    do {
                        s = readLine();
                        strim = s.trim(); // remove spaces front and back
                        endParenSpaceQuoteM = endParenSpaceQuoteP.matcher(strim);
                        if (endParenSpaceQuoteM.find()) {
                        	if (debug) {
                        		System.out.println("  ####### endParenSpaceQuote s     >"+s+"<");
                        		System.out.println("  ####### endParenSpaceQuote strim >"+strim+"<");
                        	}
                            // get rid of any spaces
                            inValue = false;
                            keepReading = false;
                        }
                   			
                        value += s;
                        if (debug) {
                            System.out.println("  s ="+s+"<");
                            System.out.println("  value ="+value+"<");
                            // String value2 = value.replaceAll("\"(\\s*),(\\s*)\"","\",\""); 
                            // System.out.println(" value2 ="+value2+"<");
                        }
                    } while (keepReading);

		            	   
                }
                       
                if (debug) {
                    System.out.println("  ### startsWith (\" openParenSpaceQuote #### createElement ### "+value);
                    // remove any spaces around any " , " sequence. now we do the split only on "," 
                    // this allows the text inside of a quoted string to contains a comma and not mess up the split
                    // then remove any remaining quotes in the split strings
                    String value2 = value.replaceAll("\"(\\s*),(\\s*)\"","\",\""); 
                    System.out.println("  ### startsWith (\"  "+value2);
                    }
                       
            }
	               /*********
	               if (debug) {
                       System.out.println("  ##### = ################");
                       System.out.println("  "+lineCt+") key ="+key+"<");
                       System.out.println("  value ="+value+"<");
                   }
                    *********/
	               
	               // multivalued should be enclosed in brackets
	               // go till we find the end backet, then parse it up
	               // check for multivalue separated by a comma
	               // check if the ( is first char in this string or inside of a quote
	               else if (v1.indexOf("(") != -1) {
	                    	// just commas with no quotes OR
	                    	// "," may also be " , " (spaces between comma and quotes)
	                    	// get all the values
	                   // remove the quotes from the quoted string
	                    startPosP = -1;
	                    endPosP = -1;
	                    quoted = "true";
	                    
                       if (debug) {
                           System.out.println("  startsWith ( "+lineCt+") key ="+key+"<>" + v1trim+"<");
                       }
	                    
	                    startPosP = v1.indexOf("(");
	                    // endPos = v.lastIndexOf("\"");
	                    // need to know if we are inside of a quote
	                    endPosP = v1.indexOf(")", startPosP +1);
	                    startPosQ = v1.indexOf("\"");
	                    if (debug) {
		               		System.out.println("     opening ( v1 "+v1);
		               		System.out.println("     startPosP "+startPosP+ "   endPosP "+endPosP);
		               	}
	                    s = "";
	                    String ss = "";
	                    StringBuffer sb=new StringBuffer();
	                    StringBuffer ssb=new StringBuffer();
	                    if (endPosP != -1) { // closing bracket is in this string
	                       
	                       // s = v1.substring(startPos+1, endPos); 
	                       // value = s;
	                       value = v1; // remove extra spaces ???
	                    }
	                    else { // no closing bracket, go to the next line
	                        keepReading = true;
	                        int qPos = -1;
	                        // s = v1.substring(startPos+1); // exclude "("
	                        s = v1.substring(startPosP);  // include "("
	                        sb.append(s);
	                        do { 
	                        	s = readLine();
	                        	// remove spaces before the quotes
	                            s = s.replaceFirst("^\\s*","");
	                            
	                            qPos = s.indexOf(")");
	                            
	                            if ( qPos != -1) {
	                                ss = s.substring(0,qPos+1); // include bracket
	                                sb.append(ss);
	                                value = sb.toString();
	                                keepReading = false;
	                            }
	                            else {
	                            	sb.append(s);
	                            }
	                        } while (keepReading) ;
	                         
	                    }
                       
                       if (debug) {
                           System.out.println("     opening ( v1 "+v1);
                           System.out.println("     startPosP "+startPosP+ "   endPosP "+endPosP);
                       }   
                       
	               /**
	               if (key.startsWith("ARTICULATION_DEV")) { 
	               
	               		System.out.println("!!!! key = "+key+" *********************");
	               		System.out.println("!!!! value="+value);
	               }
	               **/
	                
	               // now we have the complete value
	               // do subitems for each item ???   
	               // for now only if subitems are quoted ??? 
	               // check for quotes in the subitems
	               if(useUnitsAttribute) {
	                		// String units = "";
	                		// look for <units> pattern
	                		int u1 = value.indexOf("<");
	                		int u2 = value.indexOf(">");
	                		if (u1 != -1 && u2 != -1 && u1 < u2) {
	                			// get the units value, remove all instances from the value string
	                			try {
	                				units = value.substring(u1+1, u2);
	                				
	                				// remove all the instances from the value string
	                				if (debug) {
	                					System.out.println("UNITS units="+units);
	                					System.out.println("UNITS value="+value);
	                				}
	                				// value = value.replaceAll("(\\s*)<\\w+>",""); 
	                				value = value.replaceAll("(\\s*)<\\p{Print}+>",""); 
	                				if (debug) System.out.println("UNITS value="+value+" *** 3 ***");
	                			}
	                			catch (IndexOutOfBoundsException e) {
	                				units = "null";
	                				// print an error message ??
	                			}
	                		}
	               }
	               else {
	               	units = "";
	               }
	               
	               
	               if (debug) {
	            	   System.out.println(lineCt+")"+level+">"+line);
	            	   System.out.println("******************** key ="+key+"<");
	               	   System.out.println("******************** value ="+value+"<");
	               	}		
	                		
	               inValue = false; 
	               // if (value.indexOf("\"") != -1) {
	              if (value.indexOf("(") != -1) {
	               	if (useSubitem == false) {
	               		if (debug) {
		               		System.out.println("     useSubitem = "+useSubitem);
	               		}
	                	// remove any spaces before or after the backets
	                	value = value.replaceFirst("^\\s*\\(","(");
	               		value = value.replaceFirst("\\)(\\s*)$",")"); 
	               		
	                	// remove spaces AND brackets
	                	// value = value.replaceFirst("^\\s*\\(","");
	               		// value = value.replaceFirst("\\)(\\s+)$",""); 
	               		
	               		quoted = "false";
	               		element = (Element) _document.createElement("item");
	                	element.setAttribute(keyTag,key);
	                	element.setAttribute("quoted",quoted);
	                	
	                	if (useUnitsAttribute && !units.equals("")) {
	                		element.setAttribute(unitsTag,units);
	                	}	
	               			
	               		
	                
	                	// no subitems, just use the backeted value quotes and all     
	                	Text text = (Text) _document.createTextNode(value);	                        
	                	element.appendChild(text);
	                
	                	currentNode.appendChild(element);
                        if (debug) {
                            System.out.println(lineCt+")"+level+">appendChild  key="+key+" value="+value+"<  ( subitem = false");
                        }
                        
                        
	               	}
	               	else { // useSubitems == TRUE
	               	// remove the brackets
	               		if (debug) {
		               		System.out.println("     useSubitem = "+useSubitem);
	               		}
	               	// should only remove start and end () and only if they are not inside of quotes
	               	value = value.replaceFirst("\\(","");
	               	value = value.replaceFirst("\\)","");
	               	
	               	// ignore commas inside of quotes
	               	String[] subitems = value.split(",");
	               	
	               	if (value.indexOf("\"") == -1) {
	               		quoted = "false";
	               	}
	               	else {
	               		quoted = "true";
	               	}
	               	element = (Element) _document.createElement("item");
	                element.setAttribute(keyTag,key);
	                element.setAttribute("quoted",quoted);
	                
	                if (useUnitsAttribute && !units.equals("")) {
	                		element.setAttribute(unitsTag,units);
	                	}
	                
	                // no text if we have subitems       
	                // Text text = (Text) _document.createTextNode(value);	                        
	                // element.appendChild(text);
	                
	                currentNode.appendChild(element);
                        if (debug) {
                            System.out.println(lineCt+")"+level+">appendChild  key="+key+" value="+value+"<  ( subitem = true");
                        }
                       	                
	                String subv = "";
	                Element subElement ;
	                for (int i =0; i<subitems.length ; i++) {
	                	subv = subitems[i];	               	
	               		// remove quotes and spaces before or after quotes
	               		subv = subv.replaceFirst("^\\s*\"","");
	               		subv = subv.replaceFirst("\"(\\s*)$",""); 
	               		subElement = (Element) _document.createElement("subitem");
	                    subElement.setAttribute(keyTag,key);
	                    subElement.setAttribute("quoted",quoted);
	                    
	                    if (useUnitsAttribute && !units.equals("")) {
	                		subElement.setAttribute(unitsTag,units);
	                	}
	                        
	                    Text text2 = (Text) _document.createTextNode(subv);
	                        
	                    subElement.appendChild(text2);
	                    element.appendChild(subElement);
	                 }
	               	} // end of if useSubitem
	               }
                       
                       if (debug) {
                           System.out.println("  ### startsWith (  "+value);
                           String v2 = value.replaceAll("\"(\\s*),(\\s*)\"","\",\""); 
                           System.out.println("  ### startsWith (  "+v2);
                       }
	               
	               	               		
	               }
	               
	               // single value with quotes
	               else if (v1.indexOf("\"") != -1) {
	                     
                       if (debug) {
                           System.out.println("  #### startsWith \" "+lineCt+") key ="+key+"<>" + v1trim+"<");
                       }
                       
	                    // remove the quotes from the quoted string
	                    startPosQ = -1;
	                    endPosQ = -1;
	                    quoted = "true";
	                    startPosQ = v1.indexOf("\"");
	                    // endPos = v.lastIndexOf("\"");
	                    endPosQ = v1.indexOf("\"", startPosQ +1);
	                    s="";
	                    String ss="";
	                    if (endPosQ != -1) { // closing quote is in this string
	                       s = v1.substring(startPosQ+1, endPosQ); 
	                       value = s;
	                    }
	                    else { // no closing quote, go to the next line
	                        keepReading = true;
	                        int qPos = -1;
	                        s = v1.substring(startPosQ+1); 
	                        value = s;
	                        do { 
	                        	s = readLine();
	                            
	                            // s = perl.substitute("s/^\\s*//", s); 
	                            s = s.replaceAll("^\\s*","");
	                            // s = s.replaceAll("(\\s+)$","");
	                            qPos = s.indexOf("\"");
	                            
	                            if ( qPos != -1) {
	                                ss = s.substring(0,qPos);
	                                value += " " + ss;
	                                keepReading = false;
	                            }
	                            else {
	                                value += " " + s;
	                            }
	                        } while (keepReading) ;
	                         
	                    }
	               if (debug) {     
	               	for (int i=0 ; i< level ; i++) { _output.print(" "); }
	               	_output.println("<item key=\""+key+"\" quoted=\""+quoted+"\">"+value+"<item>" );
	               }
	               // add nodes for all this stuff
	                        element = (Element) _document.createElement("item");
	                        element.setAttribute(keyTag,key);
	                        element.setAttribute("quoted",quoted);
	                        
	                        Text text = (Text) _document.createTextNode(value);
	                        
	                        element.appendChild(text);
	                        currentNode.appendChild(element);
                       if (debug) {
                           System.out.println(lineCt+")"+level+">appendChild  key="+key+" value="+value+"<  \\\"");
                       }
                       
	                
	               }
	               else { // single value with no quotes in this value
                       if (debug) {
                           System.out.println("  #### startsWith no ( or \" "+lineCt+") key ="+key+"<>" + v1trim+"<");
                       }
	               	    
	                    // System.out.println("noQuotes = key <"+key+">");
	                    value = sv[1];
	                    // value = perl.substitute("s/^\\s*//", value);   // remove leading spaces
	                    value = value.replaceAll("^\\s*","");
	                    // value = perl.substitute("s/(\\s+)$//", value); // remove trailing spaces
	                    value = value.replaceAll("(\\s+)$","");
	                    
	                    if (key.matches( "END_OBJECT")) {
							if (debug) System.out.println("END_OBJECT *****************************");
	                        // close the current object. Move UP one level
	                        level--;
	                        if (debug) {
	                         for (int i=0 ; i< level ; i++) { _output.print(" "); }
	                         _output.println("</OBJECT>" );
	                        }
	                        
	                        // move back up one level
	                        currentNode = currentNode.getParentNode();
	                        
	                    }
	                    else if (key.matches( "OBJECT")) {
	                        // open a new object, put new items inside this one
	                        if (debug) {
	                        	for (int i=0 ; i< level ; i++) { _output.print(" "); }
	                        	_output.println("<OBJECT name=\""+value+"\">" );
	                        }
	                        level++;
	                        // add nodes for all this stuff
	                        element = (Element) _document.createElement(key);
	                        element.setAttribute("name",value);
	                        // the xsl to filter unwanted OBJECT nodes is easier if attributes are used
	                        // use an attribute to carry the OBJECT name instead of a TEXT node
	                        // Text text = (Text) _document.createTextNode(value);
	                        // element.appendChild(text);
	                        if (debug) {
	                        	System.out.println("OBJECT level = "+level+"  ++++++++++++++++");
	                        	System.out.println("  currentNode = "+currentNode );
	                        	System.out.println("  element = "+element );
	                        }
	                        currentNode.appendChild(element);
                            if (debug) {
                                System.out.println(lineCt+")"+level+">appendChild  key="+key+" value="+value+"<  OBJECT");
                            }
                           currentNode = (Node) element;
	                    }
	                    else if (key.matches( "END_GROUP")) {
	                        // close the current object. Move UP one level
	                        level--;
	                        if (debug) {
	                        	for (int i=0 ; i< level ; i++) { _output.print(" "); }
	                        	_output.println("</GROUP>" );
	                        }
	                        
	                        if (debug) {
	                        	System.out.println("END_GROUP level = "+level+"  #########");
	                        	System.out.println("currentNode = "+currentNode );
	                        }
	                        // move back up one level
	                        currentNode = currentNode.getParentNode();
	                        
	                    }
	                    else if (key.matches( "GROUP")) {
	                        // open a new object, put new items inside this one
	                        if (debug) {
	                        	for (int i=0 ; i< level ; i++) { _output.print(" "); }
	                        	_output.println("<GROUP name=\""+value+"\">" );
	                        }
	                        level++;
	                        // add nodes for all this stuff
	                        element = (Element) _document.createElement(key);
	                        element.setAttribute("name",value);
	                        // the xsl to filter unwanted OBJECT nodes is easier if attributes are used
	                        // use an attribute to carry the OBJECT name instead of a TEXT node
	                        // Text text = (Text) _document.createTextNode(value);
	                        // element.appendChild(text);
	                        if (debug) {
	                        	System.out.println("GROUP level = "+level+"  +++++++++++++++++");
	                        	System.out.println("  currentNode = "+currentNode );
	                        	System.out.println("  element = "+element );
	                        }
	                        currentNode.appendChild(element);
                            if (debug) {
                                System.out.println(lineCt+")"+level+">appendChild  key="+key+" value="+value+"<  3");
                            }
                            
	                        currentNode = (Node) element;
	                    }
	                    else { // this is an "item"
	                    	
	                    	if (debug) {
	                    		System.out.println("+++ item key <"+key+">"+value+"</>");
	                        	for (int i=0 ; i< level ; i++) { _output.print(" "); }
	                        	_output.println("<item key=\""+key+"\" quoted=\""+quoted+"\">"+value+"<item>" );
	                    	}
	                        
	                    	// check for a value across multiple lines 
	                    	// no quotes so all spaces are thrown away	                    	
	                    	// 
	                    	String vtrim;
	                    	vtrim = value.trim();
	                    	if (vtrim == "") {
	                    		if (debug) { 
	                    			System.out.println("+++ empty value >"+value+"</> ???????????????????????????");
	                    		}
	                    	    keepReading = true;
	                    		do {
	                    			s = readLine();
	                    			s = s.trim(); // remove spaces front and back
	                    			int slen = s.length();
	                    			int hPos = s.indexOf("-");
	                    			if (hPos != -1 && hPos == slen) {
	                    				value += s.replaceFirst("-$","");
	                    			} else {
	                    				value += s;
	                    				keepReading = false;
	                    			}
	                    		
	                    		} while (keepReading);
	                    		if (debug) {
	                    			System.out.println("+++ "+value );
	                    		}
	                    	}
	                    	
	                        if(useUnitsAttribute) {
	                		// String units = "";
	                		// look for <units> pattern
	                		int u1 = value.indexOf("<");
	                		int u2 = value.indexOf(">");
	                		if (u1 != -1 && u2 != -1 && u1 < u2) {
	                			// get the units value, remove all instances from the value string
	                			try {
	                				units = value.substring(u1+1, u2);
	                				
	                				// remove all the instances from the value string
	                				if (debug) {
	                					System.out.println("UNITS units="+units);
	                					System.out.println("UNITS value="+value);
	                				}
	                				 
	                				// value = value.replaceAll("(\\s*)<\\w+>",""); 
	                				value = value.replaceAll("(\\s*)<\\p{Print}+>",""); 
	                				if (debug) System.out.println("UNITS value="+value+" ******");
	                			}
	                			catch (IndexOutOfBoundsException e) {
	                				units = "null";
	                				// print an error message ??
	                			}
	                		  }
	                 		}
	                 		else {
	               	   			units = "";
	                 		}
	                 
	                        // add nodes for all this stuff
	                        element = (Element) _document.createElement("item");
	                        element.setAttribute(keyTag,key);
	                        element.setAttribute("quoted",quoted);
	                        if (useUnitsAttribute && !units.equals("")) {
	                			element.setAttribute(unitsTag,units);
	                		}
	                        
	                        
	                        
	                        Text text = (Text) _document.createTextNode(value);
	                        
	                        element.appendChild(text);
	                        currentNode.appendChild(element);
                            if (debug) {
                                System.out.println(lineCt+")"+level+">appendChild  key="+key+" value="+value+"<  4");
                            }
                            
	                        
	                        // ISIS only ?? look for some PDS specific items ???
	                        // labelSize = -1; // don't check until labelSize != -1
    						// recordBytes = -1;
    						// qube = -1;
    						if (key.equalsIgnoreCase("qube") || key.equalsIgnoreCase("^qube") ) { // "^QUBE"
    							qube = Integer.parseInt(value);
    							if (recordBytes != -1) {
    								labelSize = qube * recordBytes;
    								if (debug) {
    									System.out.println("labeSize "+labelSize);
    								}
    							}
    						}
    						else if (key.equalsIgnoreCase("record_bytes")) {
    							recordBytes = Integer.parseInt(value);
    							
    						}
    						// look for "^HISTORY" too ??
    						                       
	                    }                 
	               }

	            }
            /******
	           else {
	           	if (debug) {
	           		System.out.println("========== sv.length="+sv.length);
                    // need to get next line and collect value.
                    // should integrate with the code above that collects multiple lines
	           	}
	           }
            }
             *********/

	        else { // something on the line but no = "
                // all multiline values should have been collected inside of the line.indexOf("=") {} above
				line = line.replaceFirst("^\\s*","");
				line = line.replaceFirst("(\\s+)$",""); 
                if (debug) {
	           		System.out.println("  #### startsWith no = "+line);
	           	}
				if (line.matches( "END_OBJECT")) {
					if (debug) System.out.println("END_OBJECT *****************************");
					// close the current object. Move UP one level
					level--;
					if (debug) {
					  for (int i=0 ; i< level ; i++) { _output.print(" "); }
						_output.println("</OBJECT>" );
					}
	                        
						// move back up one level
						currentNode = currentNode.getParentNode();	                        
					}										
				else if (line.matches( "END_GROUP")) {
					// close the current object. Move UP one level
					if (debug) System.out.println("END_GROUP level = "+level+ "  *****************************");
					level--;
					if (debug) {
						for (int i=0 ; i< level ; i++) { _output.print(" "); }
							_output.println("</GROUP>" );
						}	                        
					// move back up one level
					currentNode = currentNode.getParentNode();	                        
					}
	        	else if (line.matches( "END") ){
	            // end the ISIS_LABEL tag
	            if (debug) _output.println("</PDS_LABEL>");
	            return _document;
	            }
	      }
	    }
	    // check for labelSize Bytes read
	    if (labelSize != -1 && bytesRead >= labelSize) {
	    	return _document;
	    }
      } // end of the while
     
    
   return  _document;
  }


public static final void main(String args[]) {
    String line;
    BufferedReader input = null;
    PrintWriter output    = null;
   

    if(args.length < 2) {
      System.err.println("Usage: pds2dom input output");
      System.exit(1); // error return
    }

    try {
      input = 
	new BufferedReader(new FileReader(args[0]));
    } catch(IOException e) {
      System.err.println("Error opening input file: " + args[0]);
      e.printStackTrace();
      System.exit(1);  // error return
    }

    try {
      output =
	new PrintWriter(new FileWriter(args[1]));
    } catch(IOException e) {
      System.err.println("Error opening output file: " + args[1]);
      e.printStackTrace();
      System.exit(1);  // error return
    } 

    PDSLabelToDOM pds = new PDSLabelToDOM(input, output) ;
    pds.setDebug(true);
    Document d = pds.getDocument();
  }  /// end of main. Used for testing



} // end of the class
