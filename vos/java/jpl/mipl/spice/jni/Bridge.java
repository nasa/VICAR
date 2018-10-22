/*
 * Bridge.java
 *
 * Created on August 9, 2000, 3:30 PM
 */

package jpl.mipl.spice.jni;

import java.util.Enumeration;

/**
 *
 * @author  mpb
 * @version 
 */
public class Bridge extends Object {

    /** No construction allowed.  Static methods only. */
    private Bridge() {
    }

    static int[] booleanToInt(boolean[] b)
    {
      int[] ret = new int[b.length];
      for (int i=0; i < b.length; ++i)
      {
        ret[i] = (b[i] == true) ? 1 : 0;
      }
      return ret;
    }
    
    static void columnsToNamesDescs(Column[] columns, 
                                        String[] names, 
                                        String[] descs)
    {
      for (int i=0; i < columns.length; ++i )
      {
        names[i] = columns[i].getName();
        descs[i] = columns[i].getDescriptor();
      }      
    }
}