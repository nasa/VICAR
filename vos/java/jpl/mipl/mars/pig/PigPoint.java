package jpl.mipl.mars.pig;

/**
 * Equivalent to <code>PigVector</code>.  It has a different name to better
 * differentiate between 3-vectors used as a location in space (Point)
 * and ones used as a direction (Vector).
 * <p>
 * Because of stupidities in Java, we have to duplicate all the constructors
 * here.
 * @see PigVector
 *
 * @author Bob Deen, JPL
 */

public class PigPoint extends PigVector
{

    ////////////////////////////////////////////////////////////////////////
    // CONSTRUCTORS

    /** Construct a 0 vector */
    public PigPoint()
    {
        super();
    }

    /** Construct a vector with the given coordinates */
    public PigPoint(double x, double y, double z)
    {
        super(x, y, z);
    }

    /** Construct a vector from a double array */
    public PigPoint(double v[])
    {
        super(v);
    }

    /** Construct a vector from a float array */
    public PigPoint(float v[]) 
    { 
        super(v); 
    }

    /** Copy constructor.  Unlike in PigVector, this IS needed in order to
     *  convert PigVector's to PigPoint's. */
    public PigPoint(PigVector v) 
    { 
        super(v); 
    }

    // SPHERICAL CONSTRUCTORS

    /** Construct a vector using spherical coordinates.  In order to make this
     *  constructor different from the rectangular case, there is an extra
     *  int argument.  It can be anything (0 is recommended). */
    public PigPoint(double az, double el, double range, int sph)
    { 
        super(az, el, range, sph); 
    }

}


