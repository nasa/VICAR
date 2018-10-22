/*
VicarIIOMetadataNode.java

*/

package jpl.mipl.io.plugins;

import javax.imageio.metadata.IIOMetadataNode;
import jpl.mipl.io.vicar.VicarLabel;

public class VicarIIOMetadataNode extends IIOMetadataNode {
   public  VicarIIOMetadataNode(String s)
   {
    super(s);
   }
    
    public  VicarIIOMetadataNode()
   {
    super();
   }
   
   /**
   *  Node which holds the VicarLabel object.
   *  Use node.getNodeName() to see if this is the node which holds the label.
   *  If it is use node.getUserObject()
   **/
   public  VicarIIOMetadataNode(VicarLabel label)
   {
    super("jpl.mipl.io.vicar.VicarLabel");
    // this is also its class name
    setNodeValue("VicarLabel");
    setUserObject(label);
   }
   
   public String toString() {
    String name = getNodeName();
    String value = getNodeValue();
    String s = "VicarIIOMetadataNode " + name+" "+value;
    return s;
   }
   // use node.getNodeName() to see if this is the node which holds the label
   // if it is node.getUserObject()
   
   // thisd is a DOM level 2 thing which ImageIO doesn't really support yet
   public boolean isSupported(java.lang.String feature, java.lang.String version) {
    return true;
   }
   
   public boolean hasAttributes() {
    return false;
   }
}