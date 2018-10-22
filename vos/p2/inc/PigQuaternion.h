////////////////////////////////////////////////////////////////////////
// PigQuaternion
//
// Class for handling quaternions.
//
// Be careful with the constructors!  (u,theta) constructs a rotation
// theta about the axis u (a Vector).  (s, v) constructs a quaternion with
// the explicit values (s, v[0], v[1], v[2]) (v is an array[3]).
//
// All coordinate systems are considered right-handed, and all angles are
// in radians.
//
// The Euler angles are roll, pitch, and yaw (heading), as used by MER
// based on the flight code.
////////////////////////////////////////////////////////////////////////

#ifndef PIGQUATERNION_H
#define PIGQUATERNION_H

#include "PigVector.h"

class PigQuaternion {
  protected:
    double _s;
    PigVector _v;

  public:
    PigQuaternion();
    PigQuaternion(const PigVector &u, const double theta);	// axis, angle!
    PigQuaternion(const double s, const double v[3]);
    PigQuaternion(const double s, const PigVector v); // scalar,vect components!
    PigQuaternion(const double v0, const double v1,
		  const double v2, const double v3);
    PigQuaternion(const float v[4]);

// Member access

    void getComponents(double v[4]) const;
    void setComponents(const double v[4]);
    void setComponents(const float v[4]);

    double getS() const { return _s; }
    const PigVector getV() const { return _v; }

    double getTheta() const;
    PigVector getU() const;

    void getEulerAngles(double &roll, double &pitch, double &yaw); // radians

    void setU(const PigVector u);
    void setTheta(const double theta);
    void setRotation(const PigVector &u, const double theta);
    void setEulerAngles(double roll, double pitch, double yaw); // radians

    void getRotationMatrix(double matrix[3][3]) const;
#if 0	/*!!!!*/
    void setRotationMatrix(const PigRotationMatrix m);
#endif  /*!!!!*/

// Overloaded operators
// There is no addition, unless someone finds it necessary.

    // Multiply two quaternions
    PigQuaternion operator*(const PigQuaternion &q2) const;

    // Rotate a vector.  This is really q * (0,v) * q' but you can write
    // it as simply q * v.
    PigVector operator*(const PigVector &v) const;

    // Unary ~ operator == q' (inverse for unit quaternions)
    PigQuaternion operator~() const;

// Other functions

    double magnitude() const;
    void normalize();

// True if the components are all equal within the supplied epsilon...

    int equalEpsilon(PigQuaternion q2, double epsilon)
			{ return ((fabs(_s - q2._s) <= epsilon) &&
				  _v.equalEpsilon(q2._v, epsilon));
			}
};

#endif

