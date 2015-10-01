/*
 * KernelPool.java
 *
 * Created on August 18, 2000, 4:05 PM
 */

package jpl.mipl.spice.jni;

import java.util.ArrayList;

/** A kernel pool allows the contents of kernels to be searched.
 * <P>
 * E kernels should be loaded with
 * the {@link #load(EKernel) load} method, then queried via a Statement
 * obtained from {@link #createStatement()}, then unloaded via
 * {@link #unload(EKernel) unload} or {@link @unloadAll() unloadAll}
 * @author Michael Brady
 */
public class KernelPool extends Object {

    private ArrayList m_loadedEk = new ArrayList();
    private ArrayList m_loadedFurnish = new ArrayList();
    
    /** Creates new KernelPool */
    public KernelPool() 
    {
    }

    /** Creates a new KernelPool with the kernels listed in the
     *  specified 'meta-kernal' loaded.
     */
    public KernelPool(java.io.File metaFile)
        throws SpiceException
    {
        furnish(metaFile);
    }
    
    /**
     *  Loads the specified text kernel into this pool.
     *  @param file The kernel to load.
     *  @throws SpiceException if an error was detected in the SPICE toolkit
     */
    public void load(TextKernel file)
      throws SpiceException
    {
      file.load();
    }
    
    /**
     *  Clears all text kernel data which has been loaded.
     *  @throws SpiceException if an error was detected in the SPICE toolkit
     */
    public void clear()
      throws SpiceException
    {
      SpiceLib.clpool();
    }    
    
    /**
     *  Loads the kernels listed in the specified meta-kernel file.
     *  This works like the Spice Toolkit's "furnsh" routine.
     */
    public void furnish(java.io.File metaFile)
        throws SpiceException
    {
        SpiceLib.furnsh(metaFile.getAbsolutePath());
        m_loadedFurnish.add(metaFile);
    }
    
    /** Loads the specified kernel into this pool.
     * @param ek The kernel to load.
     * @throws SpiceException if an error was detected in the SPICE toolkit
     */
    public void load(EKernel ek)
      throws SpiceException
    {
      ek.load();
      m_loadedEk.add(ek);
    }
    
    /** Unloads the specified kernel from the kernel pool.
     * @param ek The kernel to be unloaded.
     * @throws SpiceException if an error was detected in the SPICE toolkit
     */
    public void unload(EKernel ek)
      throws SpiceException
    {
      ek.unload();
    }
    
    /** Unloads all kernels which have been loaded into this kernel pool.
     * @throws SpiceException if an error was detected in the SPICE toolkit
     */
    public void unloadAll()
      throws SpiceException
    {
      while (!m_loadedEk.isEmpty())
      {
        unload((EKernel)m_loadedEk.remove(0));
      }
      
      while (!m_loadedFurnish.isEmpty())
      {
        SpiceLib.unload(((java.io.File)
            m_loadedFurnish.remove(0)).getAbsolutePath());
      }      
    }
    
    /** Creates a statement object which can be used to query for data in
     * the E kernels which have been loaded into this pool.
     * @return a statment object
     */
    public jpl.mipl.spice.jni.JdbcStatement createStatement()
    {
      return new jpl.mipl.spice.jni.JdbcStatement(this);
    }
    
    /**
     *  Returns information about the specified table,
     *  if there is a table by that name in the pool.
     */
    public Table getTable(String tableName)
      throws SpiceException
    {
      for (int i=0; i < m_loadedEk.size(); ++i)
      {
        Table table = ((EKernel)m_loadedEk.get(i)).getTable(tableName);
        if (table != null)
        {
          return table;
        }
      }
      return null;
    }
}