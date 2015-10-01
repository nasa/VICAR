/*
 * SpiceException.java
 *
 * Created on August 9, 2000, 1:20 PM
 */

package jpl.mipl.spice.jni;

/** An Exception thrown when an error occurs in a SPICE toolkit subroutine.
 * @author Michael Brady
 */
public class SpiceException extends Exception {

   private String m_name;
   private String m_description;
   private String m_longMessage;

   private String[] m_stackTrace;

   /**
    * Creates new <code>SpiceException</code> without detail message.
    */
   public SpiceException() 
   {
   }


   /**
    * Constructs an <code>SpiceException</code> with the specified detail message.
    * @param msg the detail message.
    */
   public SpiceException(String name, 
                         String description, 
                         String longMessage,
                         String[] stackTrace) 
   {
      super(name + ": " + description + ": " + longMessage);
      m_name = name;
      m_description = description;
      m_longMessage = longMessage;
      m_stackTrace = stackTrace;
   }

   public String getMessage()
   {
      StringBuffer msg =  new StringBuffer();
      msg.append(super.getMessage());
      msg.append("\n");
      msg.append("SPICE Toolkit stack trace:\n");

      for (int i=0; i < m_stackTrace.length; ++i)
      {
         msg.append("    ");
         msg.append(m_stackTrace[i]);
         msg.append("\n");
      }
      
      return msg.toString();
   }

   public String getName()
   {
      return m_name;
   }

   public String getDescription()
   {
      return m_description;
   }

   public String getLongMessage()
   {
      return m_longMessage;
   }

   public String[] getSpiceStackTrace()
   {
      return m_stackTrace;
   }
}

