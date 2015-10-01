$!****************************************************************************
$!
$! Build proc for MIPL module pig_vector_h
$! VPACK Version 1.9, Tuesday, August 14, 2007, 16:51:57
$!
$! Execute by entering:		$ @pig_vector_h
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   ALL         Build a private version, and unpack the PDF and DOC files.
$!   STD         Build a private version, and unpack the PDF file(s).
$!   SYStem      Build the system version with the CLEAN option, and
$!               unpack the PDF and DOC files.
$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options
$!   UNPACK      All files are created.
$!   REPACK      Only the repack file is created.
$!   SOURCE      Only the source files are created.
$!   SORC        Only the source files are created.
$!               (This parameter is left in for backward compatibility).
$!
$!   The default is to use the STD parameter if none is provided.
$!
$!****************************************************************************
$!
$! The secondary options modify how the primary option is performed.
$! Note that secondary options apply to particular primary options,
$! listed below.  If more than one secondary is desired, separate them by
$! commas so the entire list is in a single parameter.
$!
$! Secondary options are:
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module pig_vector_h ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$!
$ if (Create_Source .or. Create_Repack) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to pig_vector_h.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Build = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Build = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Build = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("pig_vector_h.bld") .eqs. "" then gosub Build_File
$   if (primary .eqs. " ")
$   then
$      @pig_vector_h.bld "STD"
$   else
$      @pig_vector_h.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create pig_vector_h.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack pig_vector_h.com -mixed -
	-s PigVector.h PigQuaternion.h
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create PigVector.h
$ DECK/DOLLARS="$ VOKAGLEVE"
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

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create PigQuaternion.h
$ DECK/DOLLARS="$ VOKAGLEVE"
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

$ VOKAGLEVE
$ Return
$!#############################################################################
