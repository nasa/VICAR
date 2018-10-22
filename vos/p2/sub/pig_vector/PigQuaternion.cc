////////////////////////////////////////////////////////////////////////
// PigQuaternion
//
// Class for handling quaternions.
//
// Be careful with the constructors!  (u,theta) constructs a rotation
// theta about the axis u (a Vector).  (s, v) constructs a quaternion with
// the explicit values (s, v[0], v[1], v[2]) (v is a Vector or array[3].
//
// All coordinate systems are considered right-handed.
////////////////////////////////////////////////////////////////////////

#include "PigQuaternion.h"

PigQuaternion::PigQuaternion()
{
    // The default is an identity rotation (theta=0)
    _s = 1.0;
    _v.setXYZ(0.0, 0.0, 0.0);
}

PigQuaternion::PigQuaternion(const PigVector &u, const double theta)
{
    setRotation(u, theta);
}

PigQuaternion::PigQuaternion(const double s, const double v[3])
{
    _s = s;
    _v.setXYZ(v);
}

PigQuaternion::PigQuaternion(const double s, const PigVector v)
{
    _s = s;
    _v = v;
}

PigQuaternion::PigQuaternion(const double v0, const double v1, const double v2,
			const double v3)
{
    _s = v0;
    _v.setXYZ(v1, v2, v3);
}

PigQuaternion::PigQuaternion(const float v[4])
{
    _s = v[0];
    _v.setXYZ(&v[1]);
}

void PigQuaternion::getComponents(double v[4]) const
{
    v[0] = _s;
    v[1] = _v.getX();
    v[2] = _v.getY();
    v[3] = _v.getZ();
}

void PigQuaternion::setComponents(const double v[4])
{
    _s = v[0];
    _v.setXYZ(&v[1]);
}

void PigQuaternion::setComponents(const float v[4])
{
    _s = v[0];
    _v.setXYZ(&v[1]);
}

double PigQuaternion::getTheta() const
{
    return acos(_s) * 2.0;
}

PigVector PigQuaternion::getU() const
{
    double sinth2 = sin(acos(_s));
    if (sinth2 == 0.0)
	return PigVector(1.0, 0.0, 0.0);	// arbitrary axis if no rot
    return _v / sinth2;
}

void PigQuaternion::setU(const PigVector u)
{
    double sinth2 = sin(acos(_s));
    PigVector vu = u;
    vu.normalize();
    _v = (vu * sinth2);
}

void PigQuaternion::setTheta(const double theta)
{
    setRotation(getU(), theta);
}

void PigQuaternion::setRotation(const PigVector &u, const double theta)
{
    _s = cos(theta/2.0);

    PigVector vu = u;
    vu.normalize();
    _v = vu * sin(theta/2.0);
}

#if 0	/*!!!!*/
PigRotationMatrix PigQuaternion::getRotationMatrix() const
{ /*!!!!????*/ }

void PigQuaternion::setRotationMatrix(const PigRotationMatrix m)
{ /*!!!!????*/ }
#endif	/*!!!!*/

// Multiply two quaternions
// Note that q2 * q1 means rotate by q1 first, then q2
PigQuaternion PigQuaternion::operator*(const PigQuaternion &q2) const
{
    return PigQuaternion(_s * q2._s - _v % q2._v,
		(q2._v * _s) + (_v * q2._s) + (_v * q2._v));
}

// Rotate a vector.  This is really q * (0,v) * q' but you can write
// it as simply q * v.  This assumes that q is a unit quaternion.
PigVector PigQuaternion::operator*(const PigVector &v) const
{
    return (*this * PigQuaternion(0.0,v) * (~(*this))).getV();
}

// Unary ~ operator == q' (inverse for unit quaternions)
PigQuaternion PigQuaternion::operator~() const
{
    return PigQuaternion(_s, _v * -1.0);
}

// Other functions

double PigQuaternion::magnitude() const
{
    return sqrt(_s*_s +
	 _v.getX()*_v.getX() + _v.getY()*_v.getY() + _v.getZ()*_v.getZ());
}

void PigQuaternion::normalize()
{
    double mag = magnitude();
    if (mag == 0.0)
	return;
    _s /= mag;
    _v /= mag;
}

// Euler angle functions.  All are in radians.  These are based on the
// MER flight code conventions for roll, pitch, and yaw (heading).
// Much of the code cribbed from jpl.mipl.mars.rmc.util.Quaternion.java.

void PigQuaternion::setEulerAngles(double roll, double pitch, double yaw)
{
    PigQuaternion roll_q(PigVector(1, 0, 0), roll);
    PigQuaternion pitch_q(PigVector(0, 1, 0), pitch);
    PigQuaternion yaw_q(PigVector(0, 0, 1), yaw);

    *this = yaw_q * pitch_q * roll_q;
}

void PigQuaternion::getEulerAngles(double &roll, double &pitch, double &yaw)
{
    double r[3][3];
    double r00, r10, r20, r21, r22;

    getRotationMatrix(r);

    r00 = r[0][0];
    r10 = r[1][0];
    r20 = r[2][0];
    r21 = r[2][1];
    r22 = r[2][2];

    roll  = atan2(r21, r22);
    pitch = atan2((-1.0 * r20), sqrt(r00*r00 + r10*r10));
    yaw   = atan2(r10, r00);
}

// Used for getEulerAngles()...

void PigQuaternion::getRotationMatrix(double matrix[3][3]) const
{
    double q0, q1, q2, q3;
    double q0q0, q0q1, q0q2, q0q3;
    double q1q1, q1q2, q1q3;
    double q2q2, q2q3, q3q3;

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

    matrix[0][0] = q0q0 + q1q1 - q2q2 - q3q3;
    matrix[0][1] = 2.0 * (q1q2 - q0q3);
    matrix[0][2] = 2.0 * (q1q3 + q0q2);

    matrix[1][0] = 2.0 * (q1q2 + q0q3);
    matrix[1][1] = q0q0 + q2q2 - q1q1 - q3q3;
    matrix[1][2] = 2.0 * (q2q3 - q0q1);

    matrix[2][0] = 2.0 * (q1q3 - q0q2);
    matrix[2][1] = 2.0 * (q2q3 + q0q1);
    matrix[2][2] = q0q0 + q3q3 - q1q1 - q2q2;

}

