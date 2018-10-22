package jpl.mipl.mars.pig;

import static java.lang.Math.abs;
import static java.lang.Math.acos;
import static java.lang.Math.atan2;
import static java.lang.Math.cos;
import static java.lang.Math.sin;
import static java.lang.Math.sqrt;

/**
 * Class for handling quaternions.
 * <p>
 * Be careful with the constructors!  (u,theta) constructs a rotation
 * theta about the axis u (a Vector).  (s, v) constructs a quaternion with
 * the explicit values (s, v[0], v[1], v[2]) (v is an array[3]).
 * <p>
 * All coordinate systems are considered right-handed, and all angles are
 * in radians.
 * <p>
 * The Euler angles are roll, pitch, and yaw (heading), as used by MER
 * based on the flight code.
 * <p>
 * This is an almost direct translation of the C++ classes.  There is one
 * fundamental difference, however: the Java classes are <em>immutable</em>.
 * The set functions actually return a new instance of the quaternion.
 * <p>
 * Of course the other primary difference is that Java does not have operator
 * overloading.  That means the operators are converted to standard methods.
 *
 * @see PigVector
 *
 * @author Bob Deen, JPL
 */

// Note, we do not need a clone() function because Object's is sufficient.
// Even though that will create a reference to the same _v, the PigVector
// class is immutable so there's no need to deep-copy it.

// Note, we don't implement equals() because it is almost certainly not
// necessary.

public class PigQuaternion implements Cloneable
{

    /** Scalar component */
    protected final double _s;
    /** Vector component */
    protected final PigVector _v;

    ////////////////////////////////////////////////////////////////////////
    // CONSTRUCTORS

    /** Construct an identity quaternion */
    public PigQuaternion()
    {
        _s = 1.0;
        _v = new PigVector();		// 0 vector
    }

    /** Construct a quaternion using axis, angle */
    public PigQuaternion(PigVector u, double theta)
    {
        _s = cos(theta/2.0);
        PigVector vu = u.normalize();
        _v = vu.multiply(sin(theta/2.0));
    }

    /** Construct a quaternion using scalar, vector components as an array */
    public PigQuaternion(double s, double v[])
    {
        _s = s;
        _v = new PigVector(v);
    }

    /** Construct a quaternion using scalar, vector components. */
    public PigQuaternion(double s, PigVector v)
    {
        _s = s;
        _v = v;		// don't need to copy since it's immutable
    }

    /** Create a quaternion from four numbers.  Note that this assumes the
     *  GROUND ordering convention, with the scalar first. */
    public PigQuaternion(double v0, double v1, double v2, double v3)
    {
        _s = v0;
        _v = new PigVector(v1, v2, v3);
    }

    /** Create a quaternion from a 4-element float array.  Note that this assumes
     *  the GROUND ordering convention, with the scalar first. */
    public PigQuaternion(float v[])
    {
        _s = v[0];
        _v = new PigVector(v[1], v[2], v[3]);
    }

    // This one is oddly missing from the C++...
    /** Create a quaternion from a 4-element double array.  Note that this assumes
     *  the GROUND ordering convention, with the scalar first. */
    public PigQuaternion(double v[])
    {
        _s = v[0];
        _v = new PigVector(v[1], v[2], v[3]);
    }

    /** Create a quaternion from Euler angles.
     *  @see PigQuaternion.getEulerAngles()
     */
    // Not the most efficient implementation...
    public PigQuaternion(double roll, double pitch, double yaw)
    {
        PigQuaternion roll_q = new PigQuaternion(new PigVector(1, 0, 0), roll);
        PigQuaternion pitch_q = new PigQuaternion(new PigVector(0, 1, 0),pitch);
        PigQuaternion yaw_q = new PigQuaternion(new PigVector(0, 0, 1), yaw);

        // *this = yaw_q * pitch_q * roll_q;
        PigQuaternion combo = (yaw_q.multiply(pitch_q)).multiply(roll_q);
        _s = combo.getS();
        _v = combo.getV();
    }

    ////////////////////////////////////////////////////////////////////////
    // Member access

    /** Get the components into an array.  This returns the components in the GROUND
     *  ordering convention, with the scalar first. */
    public void getComponents(double v[])
    {
        v[0] = _s;
        v[1] = _v.getX();
        v[2] = _v.getY();
        v[3] = _v.getZ();
    }

    // These are equivalent to constructors so are not implemented
    //    void setComponents(const double v[4]);
    //    void setComponents(const float v[4]);

    /** Return the scalar component of the quaternion */
    public double getS() { return _s; }

    /** Return the vector component of the quaternion */
    public PigVector getV() { return _v; }

    /** Return the amount of rotation as an angle (in radians) */
    public double getTheta()
    {
        return acos(_s) * 2.0;
    }

    /** Return the axis of rotation as a vector */
    public PigVector getU()
    {
        double sinth2 = sin(acos(_s));
        if (sinth2 == 0.0)
            return new PigVector(1.0, 0.0, 0.0);    // arbitrary axis if no rot
        return _v.divide(sinth2);
    }

    /** Set just the U component (rotation axis) without affecting Theta
     *  (into a new object) */
    public PigQuaternion setU(PigVector u)
    {
        double sinth2 = sin(acos(_s));
        PigVector vu = u.normalize();
        return new PigQuaternion(_s, vu.multiply(sinth2));
    }

    /** Set just the Theta component (amount of rotation) without affecting U
     *  (into a new object) */
    public PigQuaternion setTheta(double theta)
    {
        return new PigQuaternion(getU(), theta);
    }

    // Not implemented: same as constructor
    //    void setRotation(const PigVector &u, const double theta);


    ////////////////////////////////////////////////////////////////////////
    // Euler access functions

    /** Return the three Euler angles in an array of three elements.  The first
     *  element is roll, the second is pitch, and the third is yaw (heading).
     *  These are based on the MER flight code conventions.  Much of the code is
     *  cribbed from jpl.mipl.mars.rmc.util.Quaternion.java.  All angles are in
     *  radians.
     */

    public double[] getEulerAngles()
    {
        double r[][];
        double r00, r10, r20, r21, r22;

        r = getRotationMatrix();

        r00 = r[0][0];
        r10 = r[1][0];
        r20 = r[2][0];
        r21 = r[2][1];
        r22 = r[2][2];

        double roll  = atan2(r21, r22);
        double pitch = atan2((-1.0 * r20), sqrt(r00*r00 + r10*r10));
        double yaw   = atan2(r10, r00);

        double result[] = new double[3];
        result[0] = roll;
        result[1] = pitch;
        result[2] = yaw;

        return result;
    }

    /** Returns a rotation matrix converted from this quaternion.
     *  Based on Todd Litwin's rotq() function from Mat.c
     *  Taken directly from jpl.mipl.mars.rmc.util.Quaternion.java.
     *  @return 3x3 rotation matrix corresponding to this quaternion.
     */
    public double[][] getRotationMatrix()
    {
        double q0, q1, q2, q3;
        double q0q0, q0q1, q0q2, q0q3;
        double q1q1, q1q2, q1q3;
        double q2q2, q2q3, q3q3;

        double[][] r = new double[3][3];

        q0 = _s;
        q1 = _v.getX();
        q2 = _v.getY();
        q3 = _v.getZ();
        q0q0 = q0 * q0;
        q0q1 = q0 * q1;
        q0q2 = q0 * q2;
        q0q3 = q0 * q3;
        q1q1 = q1 * q1;
        q1q2 = q1 * q2;
        q1q3 = q1 * q3;
        q2q2 = q2 * q2;
        q2q3 = q2 * q3;
        q3q3 = q3 * q3;

        r[0][0] = q0q0 + q1q1 - q2q2 - q3q3;
        r[0][1] = 2.0 * (q1q2 - q0q3);
        r[0][2] = 2.0 * (q1q3 + q0q2);

        r[1][0] = 2.0 * (q1q2 + q0q3);
        r[1][1] = q0q0 + q2q2 - q1q1 - q3q3;
        r[1][2] = 2.0 * (q2q3 - q0q1);

        r[2][0] = 2.0 * (q1q3 - q0q2);
        r[2][1] = 2.0 * (q2q3 + q0q1);
        r[2][2] = q0q0 + q3q3 - q1q1 - q2q2;

        return r;
    }

    ////////////////////////////////////////////////////////////////////////
    // Operators
    // There is no addition, unless someone finds it necessary.

    /** Multiply two quaternions.  Note that q2 * q1 (or q2.multiply(q1)) means
     *  rotate by q1 first, then q2.<p>
     *  <code>c = a * b; -> c = a.multiply(b);</code>
     */
    public PigQuaternion multiply(PigQuaternion q2)
    {
        //    return PigQuaternion(_s * q2._s - _v % q2._v,
        //                (q2._v * _s) + (_v * q2._s) + (_v * q2._v));
        return new PigQuaternion(_s * q2._s - _v.dot(q2._v),
                (q2._v.multiply(_s).add(_v.multiply(q2._s)).add
                        (_v.cross(q2._v))));
    }

    /** Rotate a vector.  This is really q * (0,v) * q' but you can write
     *  it as simply q.rotate(v).  This assumes that q is a unit quaternion.
     */
    public PigVector rotate(PigVector v)
    {
        //    return (*this * PigQuaternion(0.0,v) * (~(*this))).getV();
        return ((this.multiply(new PigQuaternion(0.0,v))).multiply
                (this.inverse())).getV();
    }

    /** Invert a quaternion, which reverses the rotation for unit quaternions.
     *  Returns a new quaternion.
     */
    public PigQuaternion inverse()
    {
        return new PigQuaternion(_s, _v.multiply(-1.0));
    }

    ////////////////////////////////////////////////////////////////////////
    // Other functions

    /** Returns the magnitude of the quaternion. */
    public double magnitude()
    {
        return sqrt(_s*_s +
                _v.getX()*_v.getX() + _v.getY()*_v.getY() + _v.getZ()*_v.getZ());
    }

    /** Normalize the quaternion so it becomes a unit quaternion.  Returns a
     *  new object.
     */
    public PigQuaternion normalize()
    {
        double mag = magnitude();
        if (mag == 0.0)
            return this;	// don't need new obj because immutable
        return new PigQuaternion(_s/mag, _v.divide(mag));
    }

    // Returns true if the components are all equal within the supplied epsilon. */

    public boolean  equalEpsilon(PigQuaternion q2, double epsilon)
    {
        return ((abs(_s - q2._s) <= epsilon) &&
                _v.equalEpsilon(q2._v, epsilon));
    }
}
