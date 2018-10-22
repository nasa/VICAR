package jpl.mipl.mars.pig;

import static java.lang.Math.PI;
import static java.lang.Math.abs;
import static java.lang.Math.atan2;
import static java.lang.Math.cos;
import static java.lang.Math.sin;
import static java.lang.Math.sqrt;

/**
 * Class for a 3-D vector, stored in rectangular format.  Mathematically,
 * it is a column vector.
 * <p>
 * Spherical coordinates are labeled "Az", "El", and "Range", for lack
 * of a better term.  "Az" is equivalent to Longitude or RA, and is measured
 * counterclockwise from the +X axis.  "El" is like Latitude or Declination,
 * and is measured as the angle above the X-Y plane (meaning 0 is equator).
 * "Range" is the length of the vector.
 * <p>
 * All coordinate systems are considered right-handed, and all angles are
 * in radians.
 * <p>
 * Note that "elevation" in spherical coordinates is defined as the angle
 * from the X-Y plane towards the +Z axis.  If elevation is defined as a
 * rotation around the Y-axis, as in Pathfinder's case, this DOES NOT MATCH!
 * The right-hand rule says that +elevation goes toward the -Z axis.  If
 * your project defines "elevation" in this manner, the elevation angle must
 * be inverted for use with these routines.
 * <p>
 * <code>PigPoint</code> is equivalent to <code>PigVector</code>, it just
 * has a different name to
 * better differentiate between 3-vectors used as a location in space (Point)
 * and ones used as a direction (Vector).
 * <p>
 * This is an almost direct translation of the C++ classes.  There is one
 * fundamental difference, however: the Java classes are <em>immutable</em>.
 * The set functions actually return a new instance of the vector, and there
 * are no += style operations.
 * <p>
 * Of course the other primary difference is that Java does not have operator
 * overloading.  That means the operators are converted to standard methods.
 *
 * @author Bob Deen, JPL
 */

// Note, we do not need a clone() function because Object's is sufficient.

public class PigVector implements Cloneable
{

    /** X component */
    protected final double _x;
    /** Y component */
    protected final double _y;
    /** Z component */
    protected final double _z;

    ////////////////////////////////////////////////////////////////////////
    // STATIC FUNCTIONS

    /** Convert an angle from degrees to radians */
    public static double PigDeg2Rad(double x)
    {
        return x * (PI / 180.0);
    }

    /** Convert an angle from radians to degrees */
    public static double PigRad2Deg(double x)
    {
        return x * (180.0 / PI);
    }

    ////////////////////////////////////////////////////////////////////////
    // CONSTRUCTORS

    /** Construct a 0 vector */
    public PigVector()
    { _x = 0; _y = 0; _z = 0; }

    /** Construct a vector with the given coordinates */
    public PigVector(double x, double y, double z)
    { _x = x; _y = y; _z = z; }

    /** Construct a vector from a double array */
    public PigVector(double v[])
    { _x = v[0]; _y = v[1]; _z = v[2]; }

    /** Construct a vector from a float array */
    public PigVector(float v[])
    { _x = v[0]; _y = v[1]; _z = v[2]; }

    /** Copy constructor.  This is NOT NEEDED because the class is immutable -
     *  simply copy the pointer instead.  Included simply for completeness. */
    public PigVector(PigVector v)
    { _x = v._x; _y = v._y; _z = v._z; }

    // SPHERICAL CONSTRUCTORS

    /** Construct a vector using spherical coordinates.  In order to make this
     *  constructor different from the rectangular case, there is an extra
     *  int argument.  It can be anything (0 is recommended). */
    public PigVector(double az, double el, double range, int sph)
    { double rc = range * cos(el);
    _x = rc * cos(az);
    _y = rc * sin(az);
    _z = range * sin(el);
    }

    ////////////////////////////////////////////////////////////////////////
    // Rectangular access

    /** Return the X component */
    public double getX() { return _x; }

    /** Return the Y component */
    public double getY() { return _y; }

    /** Return the Z component */
    public double getZ() { return _z; }

    // Java does not have pass-by-reference; this is thus not implementable
    // without resorting to Double objects, which seems silly.
    //    public void getXYZ(double &x, double &y, double &z) const
    //			{ x = _x; y = _y; z = _z; }

    /** Get all components */
    public void getXYZ(double v[])
    { v[0] = _x; v[1] = _y; v[2] = _z; }

    /** Set just the X component (into a new object) */
    public PigVector setX(double x) { return new PigVector(x, _y, _z); }

    /** Set just the Y component (into a new object) */
    public PigVector setY(double y) { return new PigVector(_x, y, _z); }

    /** Set just the Z component (into a new object) */
    public PigVector setZ(double z) { return new PigVector(_x, _y, z); }

    // These method are not implemented to due the class' immutability.
    // They would be equivalent to a constructor.
    ///** Set all components via separate variables */
    //    public void setXYZ(double x, double y, double z)
    //	{ _x = x; _y = y; _z = z; }
    //
    ///** Set all components via a double array */
    //    public void setXYZ(const double v[3])
    //	{ _x = v[0]; _y = v[1]; _z = v[2]; }
    //
    ///** Set all components via a float array */
    //    public void setXYZ(const float v[3])
    //	{ _x = v[0]; _y = v[1]; _z = v[2]; }

    ////////////////////////////////////////////////////////////////////////
    // Spherical access

    /** Get the azimuth */
    public double getAz() { return atan2(_y, _x); }

    /** Get the elevation */
    public double getEl() { return atan2(_z, sqrt(_x*_x + _y*_y)); }

    /** Get the range (synonym for magnitude()) */
    public double getRange() { return magnitude(); }

    /** Set the azimuth without affecting elevation or range.  Returns a
     *  new object. */
    public PigVector setAz(double az)
    { double rc = getRange() * cos(getEl());
    return new PigVector(rc * cos(az), rc * sin(az), _z);
    }

    /** Set the elevation without affecting azimuth or range.  Returns a
     *  new object. */
    public PigVector setEl(double el)
    { return new PigVector(getAz(), el, getRange(), 0); }

    /** Set the range without affecting azimuth or elevation.  Returns a
     *  new object. */
    public PigVector setRange(double range)
    { return new PigVector(getAz(), getEl(), range, 0); }

    ////////////////////////////////////////////////////////////////////////
    // Operators.  Same functions as c++ but renamed due to lack of operator
    // overloading.
    // Note that vector * scalar is implemented but scalar * vector is not.
    // Note also that the += operators are not implemented, since this is an
    // immutable class in Java.

    /** Vector addition.<p>
     *  <code>c = a + b; -> c = a.add(b);</code>
     */
    public PigVector add(PigVector v2)
    { return new PigVector(_x+v2._x, _y+v2._y, _z+v2._z); }

    // Not implemented
    //    PigVector& operator+=(const PigVector &v2)          // v += v2
    //                        { _x += v2._x; _y += v2._y; _z += v2._z;
    //                          return *this;
    //			  }

    /** Vector Subtraction.<p>
     *  <code>c = a - b; -> c = a.subtract(b);</code>
     */
    public PigVector subtract(PigVector v2)
    { return new PigVector(_x-v2._x, _y-v2._y, _z-v2._z); }

    // Not implemented
    //    PigVector& operator-=(const PigVector &v2)		// v -= v2
    //			{ _x -= v2._x; _y -= v2._y; _z -= v2._z;
    //			  return *this;
    //			}

    /** Vector cross product.  Note the order; this operation is not commutative.<p>
     *  <code>c = a * b; -> c = a.cross(b);</code>
     */
    public PigVector cross(PigVector v2)
    { return new PigVector( _y * v2._z - _z * v2._y,
            _z * v2._x - _x * v2._z,
            _x * v2._y - _y * v2._x);
    }

    /** Vector dot product.<p>
     *  <code>c = a % b; -> c = a.dot(b);</code>
     */
    public double dot(PigVector v2)
    { return (_x*v2._x + _y*v2._y + _z*v2._z); }

    /** Scalar Multiply (vector times a scalar).<p>
     *  <code>c = a * b; -> c = a.multiply(b);</code>
     */
    public PigVector multiply(double s)
    { return new PigVector(_x*s, _y*s, _z*s); }

    // Not implemented
    //    PigVector& operator*=(const double s)		// v *= s (scalar)
    //			{ _x *= s; _y *= s; _z *= s;
    //			  return *this;
    //			}

    /** Scalar Divide (vector divided by a scalar).  Caller should check for
     *  divide by 0 first.<p>
     *  <code>c = a / b; -> c = a.divide(b);</code>
     */
    public PigVector divide(double s)
    { double ss = ((s == 0.0) ? 1e-12 : s);
    return new PigVector(_x/ss, _y/ss, _z/ss);
    }

    // Not implemented
    //    PigVector& operator/=(const double s)		// v /= s (scalar)
    //			{ double ss = ((s == 0.0) ? 1e-12 : s);
    //			  _x /= ss; _y /= ss; _z /= ss;
    //			  return *this;
    //			}

    /** Equality method.  This is much more complicated than it should be; see
     *  <code>http://www.artima.com/lejava/articles/equality.html</code>.  This
     *  implementation is taken from there. */
    public boolean equals(Object other)
    {
        boolean result = false;
        if (other instanceof PigVector) {
            PigVector that = (PigVector) other;
            result = (that.canEqual(this) && this._x == that._x &&
                    this._y == that._y && this._z == that._z);
        }
        return result;
    }

    /** Adjunct to <code>equals</code>.  See the referenced article. */
    public boolean canEqual(Object other) {
        return (other instanceof PigVector);
    }

    /** Hash code, must be overridden whenever <code>equals()</code> is. */
    public int hashCode()
    { return (int)(_x * 1000 + _y * 10000 + _z * 100000); }

    ////////////////////////////////////////////////////////////////////////
    // Other functions

    /** Returns the magnitude of the vector. */
    public double magnitude()	{ return sqrt(_x*_x + _y*_y + _z*_z); }

    /** Returns the square of the magnitude of the vector (avoids the sqrt) */
    public double magnitude_sq() { return (_x*_x + _y*_y + _z*_z); }

    /** Normalizes the vector so it becomes a unit vector.  Returns a new object. */
    public PigVector normalize()
    { double mag = magnitude();
    if (mag == 0.0) return this;	// don't need new obj because immutable
    return this.divide(mag);
    }

    /** Returns true if the components are all equal within the supplied epsilon. */

    public boolean equalEpsilon(PigVector v2, double epsilon)
    { return ((abs(_x - v2._x) <= epsilon) &&
            (abs(_y - v2._y) <= epsilon) &&
            (abs(_z - v2._z) <= epsilon));
    }
};

