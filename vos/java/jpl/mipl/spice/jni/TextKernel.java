/*
 * TextKernel.java
 *
 * Created on January 19, 2001, 4:15 PM
 */

package jpl.mipl.spice.jni;

/**
 *
 * @author  Michael Brady
 * @version 
 */
public class TextKernel 
    extends java.io.File 
{
    
  /** Creates a Text Kernel object.
   * @param fileName the name of the file associated with this object.
   */
  public TextKernel(String fileName) 
  {
    super(fileName);
  }      

  /** Creates a Text Kernel object.
   */
  public TextKernel(String parent, String child) 
  {
    super(parent, child);
  } 

  /**
   *  Loads this text kernel into the kernel pool.
   */
  void load()
    throws SpiceException
  {
    SpiceLib.ldpool(getAbsolutePath());
  }
}