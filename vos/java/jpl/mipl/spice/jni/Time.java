/*
 * Time.java
 *
 * Created on January 19, 2001, 4:27 PM
 */

package jpl.mipl.spice.jni;

/**
 * A class which converts between the various SPICE-supported time formats.
 * @author  Michael Brady
 */
public class Time 
  extends Object 
{

  /**
   *  Ephemeris time, or Barrycentric Dynamical Time (TBD).
   */
  private double m_ephemerisTime;
    
  /** Creates new Time 
   *  @param ephemeris time, is TBD measured in seconds past J2000.
   */
  public Time(double ephemerisTime)
  {
    set(ephemerisTime);
  }

  /** Creates new Time 
   *  @param ephemeris time, is TBD measured in seconds past J2000.
   */
  public Time(int spacecraftId, double encodedSclk)
    throws SpiceException
  {
    set(spacecraftId, encodedSclk);
  }
  
    /** Creates new Time 
   *  @param ephemeris time, is TBD measured in seconds past J2000.
   */
  public Time(int spacecraftId, String sclk)
    throws SpiceException
  {
    set(spacecraftId, sclk);
  }
  
  /**
   * Creates a new Time object, set to the specified epoch.
   * @param time Will be interpreted as specified in the SPICE toolkit's
   *             str2ek_c.c
   */  
  public Time(String epoch)
    throws SpiceException
  {
      set(epoch);
  }
  
  /**
   *  Sets this to the specified epoch.
   *  @param ephemeris time, is TBD measured in seconds past J2000.
   **/
  public void set(double ephemerisTime)
  {
      m_ephemerisTime = ephemerisTime;
  }
  
  /**
   *  Sets this to the specified epoch.
   *  @param spacecraftId as specified in the SPICE toolkit's naif_ids.req.
   */
  public void set(int spacecraftId, double encodedSclk)
    throws SpiceException
  {
      m_ephemerisTime = toEphemeris(spacecraftId, encodedSclk);
  }

  /**
   *  Sets this to the specified epoch.
   *  @param spacecraftId as specified in the SPICE toolkit's naif_ids.req.
   */
  public void set(int spacecraftId, String sclk)
    throws SpiceException
  {
      m_ephemerisTime = toEphemeris(spacecraftId, sclk);
  }
  
  /**
   * Sets this to the specified epoch.
   * @param time Will be interpreted as specified in the SPICE toolkit's
   *             str2ek_c.c
   */
  public void set(String time)
    throws SpiceException
  {
      m_ephemerisTime = toEphemeris(time);
  }
   
  /**
   *  Returns this time as ephemeris time (TDB seconds past J200).
   */
  public double asDouble()
  {
    return m_ephemerisTime;
  }

  /**
   *  Returns this ephemeris time as a string.
   *  @param format a string describing the output format.
   *          See NAIF's timout_c source code comments for details.
   */
  public String asString(String format)
    throws SpiceException
  {
    return SpiceLib.timout(m_ephemerisTime, format);
  }

  /**
   *  Returns this ephemeris time as a string with a default format.
   */
  public String asString()
    throws SpiceException
  {
    return asString("MON DD,YYYY  HR:MN:SC.#### (UTC) ::UTC");
  }
  
  /**
   *  Returns the equivalent ephemeris time (in double form) for
   *  the specfied mission and sclk time.
   */
  static public double toEphemeris(int spacecraftId, double sclk)
    throws SpiceException
  {
    return SpiceLib.sct2e(spacecraftId, sclk);
  }
    
  /**
   *  Returns the equivalent ephemeris time (in double form) for
   *  the specfied mission and sclk time.
   */
  static public double toEphemeris(int spacecraftId, String sclk)
    throws SpiceException
  {
    return SpiceLib.scs2e(spacecraftId, sclk);
  }    
  
  /**
   *  Returns the equivalent ephemeris time (in double form) for 
   *  the specified string, interpreted as described in the SPICE
   *  Toolkit's str2et_c.c.
   */
  static public double toEphemeris(String time)
    throws SpiceException
  {
      return SpiceLib.str2et(time);
  }
}