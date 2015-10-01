/*
 * EKernel.java
 *
 * Created on August 15, 2000, 8:24 AM
 */

package jpl.mipl.spice.jni;

import java.io.File;

/** An E ("Event") kernel file.
 * @author Michael Brady
 */
public class EKernel 
   extends java.io.File 
{
   /** READ permission for an E kernel. */
   static public int READ = 0;

   /** read and write permission for an E kernel. */
   static public int READ_WRITE = 1;
  
   private int m_handle = -1;

   /** Creates an EKernel object, but does not open a file.
    * <P>
    * The file may be opened with {@link #create(String, int) } or 
    * {@link #open(int) }.
    * @param fileName the name of the file associated with this object.
    */
   public EKernel(String fileName) 
   {
      super(fileName);
   }  

   /** Creates an EKernel object, but does not open a file.
    * <P>
    * The file may be opened with {@link #create(String, int) } or 
    * {@link #open(int) }.
    * @param fileName the name of the file associated with this object.
    */
   public EKernel(String parent, String child) 
   {
      super(parent, child);
   }    
  
   /** Opens an existing EKernel file
    * @param fileName the name of the file to open
    * @param permission can be READ or READ_WRITE
    * @throws SpiceException if an error was detected in the SPICE toolkit
    */
   public EKernel(String fileName, int permission) 
      throws SpiceException
   {
      super(fileName);
      open(permission);
   }

   /** Creates a new EKernel file
    * @param fileName The name of the file to create
    * @param internalName The name which will be recorded in the 
    *                      file for internal use.
    * @param numCommentChars the number of characters to reserve 
    *                         for use for comments.
    * @throws SpiceException if an error was detected in the SPICE toolkit
    */
   public EKernel(String fileName, String internalName, int numCommentChars) 
      throws SpiceException
   {
      super(fileName);
      create(internalName, numCommentChars);
   }
  
   /** Opens an existing file.
    * @param permission READ or READ_WRITE
    * @throws SpiceException if an error was detected in the SPICE toolkit
    */
   public void open(int permission)
      throws SpiceException
   {
      if (permission == READ)
      {
         m_handle = SpiceLib.ekopr(getAbsolutePath());
      }
      else if (permission == READ_WRITE)
      {
         m_handle = SpiceLib.ekopw(getAbsolutePath());
      }
      else
      {
         throw new java.security.InvalidParameterException(
           "EKernel.open:  permission must be READ or READ_WRITE");
      }
   }
   
   /** Creates a new file.
    * @param internalName the name which will be recorded in the file 
    *                      for internal use.
    * @param numCommentChars the number of characters reserved for 
    *                          use for comments in the file
    * @throws SpiceException if an error was detected in the SPICE toolkit
    */
   public void create(String internalName, int numCommentChars)
      throws SpiceException
   {
      m_handle = SpiceLib.ekopn(getAbsolutePath(), 
                                internalName, 
                                numCommentChars);
   }
  
   /** Closes the file associated with this E kernel object.
    * @throws SpiceException if an error was detected in the SPICE toolkit
    */
   public void close()
      throws SpiceException
   {
      SpiceLib.ekcls(m_handle);
      m_handle = -1;
   }
  
   /** Opens this kernel and loads it into the kernel pool.
    * @throws SpiceException if an error was detected in the SPICE toolkit
    */
   void load()
      throws SpiceException
   {
      m_handle = SpiceLib.eklef(getAbsolutePath());
   }
  
   /** Closes this kernel and removes its data from the kernel pool.
    * @throws SpiceException if an error was detected in the SPICE toolkit
    */
   void unload()
      throws SpiceException
   {
      SpiceLib.ekuef(m_handle);
      m_handle = -1;
   }
   
   /** Returns the SPICE handle for this kernel.
    * @return the handle
    */
   int getHandle()
   {
      return m_handle;
   }
    
   /** Returns the number of segments in this file.  
    *  If this has not been opened for reading, 
    *  a Spice Exception will be thrown.
    * @exception SpiceException if an error was detected in the SPICE toolkit
    * @return the number of segments
    */
   public int getNumSegments()
      throws SpiceException
   {
      return SpiceLib.eknseg(m_handle);
   }
  
   /** Returns MetaData for the table to which the specified segment belongs.
    * @param segment the index of the desired segment
    * @throws SpiceException if an error was detected in the SPICE toolkit
    * @return Metadata for the table
    */
   public Table getTable(int segment)
      throws SpiceException
   {
      return SpiceLib.ekssum(m_handle, segment);
   }
  
   /** Returns the MetaData for the first segment found which is part of
    * the specified table.
    * @return null if no such segment is found
    * @param tableName The table for which to search.
    * @throws SpiceException if an error was detected in the SPICE toolkit
    */
   public Table getTable(String tableName)
      throws SpiceException
   {
      return getTable(tableName, 0);
   }
  
   /** Returns the meta data for the ith segment found which is part of
    * the specified table.
    * @segmentCount the zero-based index of the segment to be found
    * @return null if no such segment is found.
    * @param tableName The name of the table for which to search
    * @param whichSegment The number of matching segments which will be skipped.  Zero returns the
    * the first segment.
    * @throws SpiceException if an error was detected in the SPICE toolkit
    */
   public Table getTable(String tableName, int whichSegment)
      throws SpiceException
   {
      int segmentCount = 0;
      int numSegments = getNumSegments();
      for (int i=0; i < numSegments; ++i)
      {
         Table table = getTable(i);
         if (table.getName().equals(tableName))
         {
            if (segmentCount == whichSegment)
            {
               return table;
            }
            ++segmentCount;
         }      
      }
      return null;    
   }

   
   /** Creates a new segment in this file which will be added
    *  to the specified table.
    */
   public SegmentRW createNewSegment(Table table)
      throws SpiceException
   {
      int segmentNumber = SpiceLib.ekbseg(m_handle,
                                          table.getName(),
                                          table.getColumns());

      return new SegmentRW(this, segmentNumber, table);
   }
}
