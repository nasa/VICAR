////////////////////////////////////////////////////////////////////////
// PigVector
//
// Class for a 3-D vector, stored in rectangular format.  Mathematically,
// it is a column vector.
//
// Spherical coordinates are labeled "Az", "El", and "Range", for lack
// of a better term.  "Az" is equivalent to Longitude or RA, and is measured
// counterclockwise from the +X axis.  "El" is like Latitude or Declination,
// and is measured as the angle above the X-Y plane (meaning 0 is equator).
// "Range" is the length of the vector.
//
// All coordinate systems are considered right-handed, and all angles are
// in radians.
//
// Note that "elevation" in spherical coordinates is defined as the angle
// from the X-Y plane towards the +Z axis.  If elevation is defined as a
// rotation around the Y-axis, as in Pathfinder's case, this DOES NOT MATCH!
// The right-hand rule says that +elevation goes toward the -Z axis.  If
// your project defines "elevation" in this manner, the elevation angle must
// be inverted for use with these routines.
//
// PigPoint is equivalent to PigVector, it just has a different name to
// better differentiate between 3-vectors used as a location in space (Point)
// and ones used as a direction (Vector).
////////////////////////////////////////////////////////////////////////

#ifndef PIGVECTOR_H
#define PIGVECTOR_H

#include <math.h>

#ifndef M_PI			/* should be in math.h */
#define M_PI 3.14159265358979323846
#endif

#define PigDeg2Rad(x) ((x) * (M_PI/180.0))
#define PigRad2Deg(x) ((x) * (180.0/M_PI))

class PigVector {
  protected:
    double _x;
    double _y;
    double _z;

  public:
    PigVector()
			{ _x = 0; _y = 0; _z = 0; }
    PigVector(const double x, const double y, const double z)
			{ _x = x; _y = y; _z = z; }
    PigVector(const double v[3])
			{ _x = v[0]; _y = v[1]; _z = v[2]; }
    PigVector(const float v[3])
			{ _x = v[0]; _y = v[1]; _z = v[2]; }
// Rectangular access

    double getX() const { return _x; }
    double getY() const { return _y; }
    double getZ() const { return _z; }
    void getXYZ(double &x, double &y, double &z) const
			{ x = _x; y = _y; z = _z; }
    void getXYZ(double v[3]) const
			{ v[0] = _x; v[1] = _y; v[2] = _z; }

    void setX(const double x) { _x = x; }
    void setY(const double y) { _y = y; }
    void setZ(const double z) { _z = z; }
    void setXYZ(const double x, const double y, const double z)
			{ _x = x; _y = y; _z = z; }
    void setXYZ(const double v[3])
			{ _x = v[0]; _y = v[1]; _z = v[2]; }
    void setXYZ(const float v[3])
			{ _x = v[0]; _y = v[1]; _z = v[2]; }

// Spherical access

    double getAz() const { return atan2(_y, _x); }
    double getEl() const { return atan2(_z, sqrt(_x*_x + _y*_y)); }
    double getRange() const { return magnitude(); }

    void setAz(const double az)			// z doesn't change so no recalc
			{ double rc = getRange() * cos(getEl());
			  _x = rc * cos(az);
			  _y = rc * sin(az);
			}
    void setEl(const double el)
			{ setSpherical(getAz(), el, getRange()); }
    void setRange(const double range)
			{ setSpherical(getAz(), getEl(), range); }
    void setSpherical(const double az, const double el, const double range)
			{ double rc = range * cos(el);
			  _x = rc * cos(az);
			  _y = rc * sin(az);
			  _z = range * sin(el);
			}

// Overloaded operators
// Note that vector * scalar is implemented but scalar * vector is not.

    // Vector Addition
    PigVector operator+(const PigVector &v2) const	// v = v1 + v2
			{ return PigVector(_x+v2._x, _y+v2._y, _z+v2._z); }
    PigVector& operator+=(const PigVector &v2)		// v += v2
			{ _x += v2._x; _y += v2._y; _z += v2._z;
			  return *this;
			}
    // Vector Subtraction
    PigVector operator-(const PigVector &v2) const	// v = v1 - v2
			{ return PigVector(_x-v2._x, _y-v2._y, _z-v2._z); }
    PigVector& operator-=(const PigVector &v2)		// v -= v2
			{ _x -= v2._x; _y -= v2._y; _z -= v2._z;
			  return *this;
			}
    // Cross Product
    PigVector operator*(const PigVector &v2) const  // v = v1xv2 (cross product)
			{ return PigVector(_y * v2._z - _z * v2._y,
					_z * v2._x - _x * v2._z,
					_x * v2._y - _y * v2._x);
			}
		// cross product not implemented as *= due to order ambiguity
    // Dot Product
    double operator%(const PigVector &v2) const	// s = v1 % v2 (dot product)
			{ return (_x*v2._x + _y*v2._y + _z*v2._z); }
    // Scalar Multiply
    PigVector operator*(const double s) const		// v = v1 * s (scalar)
			{ return PigVector(_x*s, _y*s, _z*s); }
    PigVector& operator*=(const double s)		// v *= s (scalar)
			{ _x *= s; _y *= s; _z *= s;
			  return *this;
			}
    // Scalar Divide.  Caller should check for divide by 0 first.
    PigVector operator/(const double s) const		// v = v1 / s (scalar)
			{ double ss = ((s == 0.0) ? 1e-12 : s);
			  return PigVector(_x/ss, _y/ss, _z/ss);
			}
    PigVector& operator/=(const double s)		// v /= s (scalar)
			{ double ss = ((s == 0.0) ? 1e-12 : s);
			  _x /= ss; _y /= ss; _z /= ss;
			  return *this;
			}

    int operator==(const PigVector &v2) const		// v == v
			{ return ((_x == v2._x) && (_y == v2._y) &&
							(_z == v2._z));
			}
// Other functions

    double magnitude() const	{ return sqrt(_x*_x + _y*_y + _z*_z); }
    double magnitude_sq() const	{ return (_x*_x + _y*_y + _z*_z); } //avoid sqrt

    void normalize()	{ double mag = magnitude();
			  if (mag == 0.0) return;
			  *this /= mag;
			}

// True if the components are all equal within the supplied epsilon...

    int equalEpsilon(PigVector v2, double epsilon)
			{ return ((fabs(_x - v2._x) <= epsilon) &&
				  (fabs(_y - v2._y) <= epsilon) &&
				  (fabs(_z - v2._z) <= epsilon));
			}
};

typedef PigVector PigPoint;

#endif

