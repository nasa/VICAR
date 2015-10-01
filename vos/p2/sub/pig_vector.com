$!****************************************************************************
$!
$! Build proc for MIPL module pig_vector
$! VPACK Version 1.9, Saturday, April 03, 2004, 12:19:48
$!
$! Execute by entering:		$ @pig_vector
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   COMPile     Compile the program modules
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
$!   TEST        Only the test files are created.
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
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
$! COMPile,ALL:
$!   DEBug      Compile for debug               (/debug/noopt)
$!   PROfile    Compile for PCA                 (/debug)
$!   LISt       Generate a list file            (/list)
$!   LISTALL    Generate a full list            (/show=all)   (implies LIST)
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module pig_vector ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_Test = ""
$ Create_Imake = ""
$ Do_Make = ""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("COMP", primary) .eqs. 0) then gosub Set_Exe_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_Test .or. Create_Imake .or -
        Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to pig_vector.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
$ Return
$!
$ Set_EXE_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("pig_vector.imake") .nes. ""
$   then
$      vimake pig_vector
$      purge pig_vector.bld
$   else
$      if F$SEARCH("pig_vector.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake pig_vector
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @pig_vector.bld "STD"
$   else
$      @pig_vector.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create pig_vector.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack pig_vector.com -mixed -
	-s PigQuaternion.cc -
	-i pig_vector.imake -
	-t TestPigVector.cc tpig_vector.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create PigQuaternion.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
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

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create pig_vector.imake
#define SUBROUTINE pig_vector

#define MODULE_LIST PigQuaternion.cc

#define P2_SUBLIB

#define USES_C_PLUS_PLUS

$ Return
$!#############################################################################
$Test_File:
$ create TestPigVector.cc
////////////////////////////////////////////////////////////////////////
// Test program for vectors and quaternions.
////////////////////////////////////////////////////////////////////////

#include "PigVector.h"
#include "PigQuaternion.h"

#include <stdio.h>

int main()
{
    PigVector v1;
    PigVector v2(3.2, 4.5, -2.7);
    double d3[3] = {6.3, -1.4, 3.8};
    PigVector *v3 = new PigVector(d3);

    printf("\nVECTOR TEST\n\n");

    printf("get/set\n");
    printf("v2: x=%f, y=%f, z=%f\n", v2.getX(), v2.getY(), v2.getZ());
    printf("s/b 3.2, 4.5, -2.7\n");

    double x,y,z;
    v3->getXYZ(x, y, z);
    printf("v3: x=%f, y=%f, z=%f\n", x, y, z);
    printf("s/b 6.3, -1.4, 3.8\n");

    double a[3];
    v3->getXYZ(a);
    printf("v3: a[0]=%f, a[1]=%f, a[2]=%f\n", a[0], a[1], a[2]);
    printf("s/b 6.3, -1.4, 3.8\n");

    v1.setX(10.1);
    v1.setY(-8.6);
    v1.setZ(0.4);
    printf("v1: x=%f, y=%f, z=%f\n", v1.getX(), v1.getY(), v1.getZ());
    printf("s/b 10.1, -8.6, 0.4\n");

    x=3.1; y=6.6; z=9.3;
    v1.setXYZ(x, y, z);
    printf("v1: x=%f, y=%f, z=%f\n", v1.getX(), v1.getY(), v1.getZ());
    printf("s/b 3.1, 6.6, 9.3\n");

    a[0]=3.3; a[1]=4.4; a[2]=5.5;
    v1.setXYZ(a);
    printf("v1: x=%f, y=%f, z=%f\n", v1.getX(), v1.getY(), v1.getZ());
    printf("s/b 3.3, 4.4, 5.5\n");

    printf("get/set spherical\n");
    printf("v2: x=%f, y=%f, z=%f\n", v2.getX(), v2.getY(), v2.getZ());
    printf("v2: az=%f, el=%f, range=%f\n", v2.getAz(),v2.getEl(),v2.getRange());
    printf("s/b .95265, -.454787, 6.14654\n");

    v2.setAz(.3312);
    printf("v2: x=%f, y=%f, z=%f\n", v2.getX(), v2.getY(), v2.getZ());
    printf("v2: az=%f, el=%f, range=%f\n", v2.getAz(),v2.getEl(),v2.getRange());
    printf("s/b .3312, -.454787, 6.14654\n");

    v2.setEl(-.0311);
    printf("v2: x=%f, y=%f, z=%f\n", v2.getX(), v2.getY(), v2.getZ());
    printf("v2: az=%f, el=%f, range=%f\n", v2.getAz(),v2.getEl(),v2.getRange());
    printf("s/b .3312, -.0311, 6.146544\n");

    v2.setRange(12.5);
    printf("v2: x=%f, y=%f, z=%f\n", v2.getX(), v2.getY(), v2.getZ());
    printf("v2: az=%f, el=%f, range=%f\n", v2.getAz(),v2.getEl(),v2.getRange());
    printf("s/b .3312, -.0311 12.5 (xyz=11,814944, 4.06276, -.388687\n");

    v2.setSpherical(3.1415926535/8, 3.1415926535/4, 10.0);
    printf("v2: x=%f, y=%f, z=%f\n", v2.getX(), v2.getY(), v2.getZ());
    printf("v2: az=%f, el=%f, range=%f\n", v2.getAz(),v2.getEl(),v2.getRange());
    printf("s/b xyz=6.532815, 2.70598, 7.071068\n");

    printf("addition\n");
    PigVector v4 = v1 + *v3;
    printf("v1: x=%f, y=%f, z=%f\n", v1.getX(), v1.getY(), v1.getZ());
    printf("v3: x=%f, y=%f, z=%f\n", v3->getX(), v3->getY(), v3->getZ());
    printf("+ : x=%f, y=%f, z=%f\n", v4.getX(), v4.getY(), v4.getZ());
    printf("s/b 9.6, 3.0, 9.3\n");

    v1 += *v3;
    printf("v1: x=%f, y=%f, z=%f\n", v1.getX(), v1.getY(), v1.getZ());
    printf("s/b 9.6, 3.0, 9.3\n");

    printf("subtraction\n");
    v4 = v1 - v2;
    printf("v1: x=%f, y=%f, z=%f\n", v1.getX(), v1.getY(), v1.getZ());
    printf("v2: x=%f, y=%f, z=%f\n", v2.getX(), v2.getY(), v2.getZ());
    printf("- : x=%f, y=%f, z=%f\n", v4.getX(), v4.getY(), v4.getZ());
    printf("s/b 3.067185, .294019, 2.228932\n");

    v1 -= v2;
    printf("v1: x=%f, y=%f, z=%f\n", v1.getX(), v1.getY(), v1.getZ());
    printf("s/b 3.067185, .294019, 2.228932\n");

    printf("cross product\n");
    v4 = v2 * *v3;
    printf("v2: x=%f, y=%f, z=%f\n", v2.getX(), v2.getY(), v2.getZ());
    printf("v3: x=%f, y=%f, z=%f\n", v3->getX(), v3->getY(), v3->getZ());
    printf("x : x=%f, y=%f, z=%f\n", v4.getX(), v4.getY(), v4.getZ());
    printf("s/b 20.18222, 19.723031, -26.1936\n");

    printf("dot product\n");
    double dot = v2 % *v3;
    printf("v2: x=%f, y=%f, z=%f\n", v2.getX(), v2.getY(), v2.getZ());
    printf("v3: x=%f, y=%f, z=%f\n", v3->getX(), v3->getY(), v3->getZ());
    printf(". : dot=%f\n", dot);
    printf("s/b 64.2384\n");

    printf("scalar multiply\n");
    v4 = v1 * 5.0;
    printf("v1: x=%f, y=%f, z=%f\n", v1.getX(), v1.getY(), v1.getZ());
    printf("v4: x=%f, y=%f, z=%f\n", v4.getX(), v4.getY(), v4.getZ());
    printf("s/b 15.3359, 1.47009, 11.14466\n");

    v4 *= 10.0;
    printf("v4: x=%f, y=%f, z=%f\n", v4.getX(), v4.getY(), v4.getZ());
    printf("s/b 153.359, 14.7009, 111.4466\n");

    printf("scalar divide\n");
    v4 = v1 / 3.0;
    printf("v1: x=%f, y=%f, z=%f\n", v1.getX(), v1.getY(), v1.getZ());
    printf("v4: x=%f, y=%f, z=%f\n", v4.getX(), v4.getY(), v4.getZ());
    printf("s/b 1.022395, .098006, .742977\n");

    v4 /= 2.0;
    printf("v4: x=%f, y=%f, z=%f\n", v4.getX(), v4.getY(), v4.getZ());
    printf("s/b .511198, .049003, .371488\n");

    printf("magnitude\n");
    printf("v1.mag=%f, v3.mag=%f, v4.mag=%f\n", v1.magnitude(), v3->magnitude(),
						v4.magnitude());
    printf("s/b 3.802921, 7,489326, .633820\n");

    printf("normalize\n");
    v4.normalize();
    printf("v4 norm: x=%f, y=%f, z=%f\n", v4.getX(), v4.getY(), v4.getZ());
    printf("v4 norm: az=%f, el=%f, range=%f\n", v4.getAz(),v4.getEl(),v4.getRange());
    printf("s/b .806534, .07731, .586111\n");

    v3->normalize();
    printf("v3 norm: x=%f, y=%f, z=%f\n", v3->getX(), v3->getY(), v3->getZ());
    printf("v3 norm: az=%f, el=%f, range=%f\n", v3->getAz(),v3->getEl(),v3->getRange());
    printf("s/b .841197, -.18693, .507389\n");

    delete v3;

    PigVector v6(34.1341345, 55.344112, -42.589971);
    PigVector v7(34.1341401, 55.344105, -42.589980);
// diff:           .0000056    .000007     .000009
    printf("v6: x=%f, y=%f, z=%f\n", v6.getX(), v6.getY(), v6.getZ());
    printf("v7: x=%f, y=%f, z=%f\n", v7.getX(), v7.getY(), v7.getZ());
    printf("Epsilon comparison v6,v7 with 0.000005 (s/b 0): %d\n",
					v6.equalEpsilon(v7,0.000005));
    printf("Epsilon comparison v6,v7 with 0.00001 (s/b 1): %d\n",
					v7.equalEpsilon(v7,0.00001));

////////////////////////////////////////////////////////////////////////

    printf("\nQUATERNION TEST\n\n");

    PigQuaternion q1;
    PigQuaternion q2(.2314, v4);

    printf("get/set\n");
    printf("q2: s=%f, vx=%f, vy=%f, vz=%f\n", q2.getS(), q2.getV().getX(), q2.getV().getY(), q2.getV().getZ());
    printf("s/b .2314, .806534, .07731, .586111\n");

    PigVector v5(0.5, 0.2, 1.0);
    PigQuaternion q3(v5, 3.1415926535/4);
    printf("q3: s=%f, vx=%f, vy=%f, vz=%f\n", q3.getS(), q3.getV().getX(), q3.getV().getY(), q3.getV().getZ());
    printf("s/b .92388, .168467, .067387, .336934\n");

    double a2[3];
    a2[0]=3.2; a2[1]=5.5; a2[2]=1.9;
    PigQuaternion *q4 = new PigQuaternion(2.2, a2);
    printf("q4: s=%f, vx=%f, vy=%f, vz=%f\n", q4->getS(), q4->getV().getX(), q4->getV().getY(), q4->getV().getZ());
    printf("s/b 2.2. 3.2, 5.5, 1.9\n");

    PigQuaternion q5(1.1, 2.2, 3.3, 4.4);
    printf("q5: s=%f, vx=%f, vy=%f, vz=%f\n", q5.getS(), q5.getV().getX(), q5.getV().getY(), q5.getV().getZ());
    printf("s/b 1.1, 2.2, 3.3, 4.4\n");

    double a3[4];
    q2.getComponents(a3);
    printf("q2: a0=%f, a1=%f, a2=%f, a3=%f\n", a3[0], a3[1], a3[2], a3[3]);
    printf("s/b .2314, .806534, .077314, .586111\n");

    PigQuaternion q6;
    a3[0] = .5432; a3[1] = .2211; a3[2] = .7766; a3[3] = .6789;
    q6.setComponents(a3);
    printf("q6: s=%f, vx=%f, vy=%f, vz=%f\n", q6.getS(), q6.getV().getX(), q6.getV().getY(), q6.getV().getZ());
    printf("s/b .5432, .2211, .7766, .6789\n");

    v5 = q2.getU();
    printf("q2: theta=%f, u0=%f, u1=%f, u2=%f\n", q2.getTheta(), v5.getX(), v5.getY(), v5.getZ());
    printf("s/b 2.67456, .829035, .079471, .602462\n");

    q1.setTheta(.431);
    printf("q1: s=%f, vx=%f, vy=%f, vz=%f\n", q1.getS(), q1.getV().getX(), q1.getV().getY(), q1.getV().getZ());
    printf("s/b .97687, .213836, 0, 0\n");

    v5.setXYZ(2.3, -4.1, 1.1);
    q1.setU(v5);
    printf("q1: s=%f, vx=%f, vy=%f, vz=%f\n", q1.getS(), q1.getV().getX(), q1.getV().getY(), q1.getV().getZ());
    printf("s/b .97687, .101868, -.18159, .048719\n");

    v5.setAz(.2213);
    q2.setRotation(v5, .0321);
    printf("v5: x=%f, y=%f, z=%f\n", v5.getX(), v5.getY(), v5.getZ());
    printf("q2: s=%f, vx=%f, vy=%f, vz=%f\n", q2.getS(), q2.getV().getX(), q2.getV().getY(), q2.getV().getZ());
    printf("s/b .999871, .015246, .003430, .003657\n");

    printf("multiply\n");
    q6 = q1 * q2;
    printf("q1: s=%f, vx=%f, vy=%f, vz=%f\n", q1.getS(), q1.getV().getX(), q1.getV().getY(), q1.getV().getZ());
    printf("q2: s=%f, vx=%f, vy=%f, vz=%f\n", q2.getS(), q2.getV().getX(), q2.getV().getY(), q2.getV().getZ());
    printf("q6: s=%f, vx=%f, vy=%f, vz=%f\n", q6.getS(), q6.getV().getX(), q6.getV().getY(), q6.getV().getZ());
    printf("s/b .975635, .115917, .177846, .055403\n");

    printf("magnitude\n");
    printf("q4: s=%f, vx=%f, vy=%f, vz=%f\n", q4->getS(), q4->getV().getX(), q4->getV().getY(), q4->getV().getZ());
    printf("q5: s=%f, vx=%f, vy=%f, vz=%f\n", q5.getS(), q5.getV().getX(), q5.getV().getY(), q5.getV().getZ());
    printf("q4.mag=%f, q5.mag=%f\n", q4->magnitude(), q5.magnitude());
    printf("s/b 6.995713, 6.024948\n");

    printf("normalize\n");
    q4->normalize();
    printf("q4n:s=%f, vx=%f, vy=%f, vz=%f\n", q4->getS(), q4->getV().getX(), q4->getV().getY(), q4->getV().getZ());
    printf("s/b .314478, .457423, .786196, .271595\n");

    q5.normalize();
    printf("q5n:s=%f, vx=%f, vy=%f, vz=%f\n", q5.getS(), q5.getV().getX(), q5.getV().getY(), q5.getV().getZ());
    printf("s/b .182574, .365148, .547723, .730297\n");

    printf("rotate vector\n");
    v5 = q1 * v1;
    printf("v1: x=%f, y=%f, z=%f\n", v1.getX(), v1.getY(), v1.getZ());
    printf("q1: s=%f, vx=%f, vy=%f, vz=%f\n", q1.getS(), q1.getV().getX(), q1.getV().getY(), q1.getV().getZ());
    printf("v5: x=%f, y=%f, z=%f\n", v5.getX(), v5.getY(), v5.getZ());
    printf("s/b 2.042821, -.0180548, 3.2076145\n");

    v5 = q2 * v2;
    printf("v2: x=%f, y=%f, z=%f\n", v2.getX(), v2.getY(), v2.getZ());
    printf("q2: s=%f, vx=%f, vy=%f, vz=%f\n", q2.getS(), q2.getV().getX(), q2.getV().getY(), q2.getV().getZ());
    printf("v5: x=%f, y=%f, z=%f\n", v5.getX(), v5.getY(), v5.getZ());
    printf("s/b 6.5622677, 2.5377018, 7.1060987\n");

    printf("prime\n");
    q2 = ~q1;
    printf("q1: s=%f, vx=%f, vy=%f, vz=%f\n", q1.getS(), q1.getV().getX(), q1.getV().getY(), q1.getV().getZ());
    printf("q2: s=%f, vx=%f, vy=%f, vz=%f\n", q2.getS(), q2.getV().getX(), q2.getV().getY(), q2.getV().getZ());
    printf("s/b .976870, -.101868, .181591, -.048719\n");

    PigQuaternion q7(.976865, -.101870, .181590, -.048721);
// diff:             .000005   .000002  .000001, .000002
    printf("q7: s=%f, vx=%f, vy=%f, vz=%f\n", q7.getS(), q7.getV().getX(), q7.getV().getY(), q7.getV().getZ());
    printf("Epsilon comparison q2,q7 with 0.000004 (s/b 0): %d\n",
					q2.equalEpsilon(q7,0.000004));
    printf("Epsilon comparison q2,q7 with 0.000006 (s/b 1): %d\n",
					q2.equalEpsilon(q7,0.000006));

    // Euler test 1
    printf("euler angles 1\n");

    PigQuaternion q8(0.896434, -0.0962536, 0.0873797, 0.423682);
    double roll, pitch, yaw;
    q8.getEulerAngles(roll, pitch, yaw);
    printf("roll=%f, pitch=%f, yaw=%f\n", PigRad2Deg(roll), PigRad2Deg(pitch),
					PigRad2Deg(yaw));
    printf("s/b r=-5.822569, p=13.7816267, y=49.889430\n");
    PigQuaternion q9;
    q9.setEulerAngles(roll, pitch, yaw);
    printf("q9: s=%f, vx=%f, vy=%f, vz=%f\n", q9.getS(), q9.getV().getX(), q9.getV().getY(), q9.getV().getZ());
    printf("s/b 0.896434, -0.0962536, 0.0873797, 0.423682\n");
    printf("Epsilon comparison q8,q9 with 0.000001 (s/b 1): %d\n",
					q8.equalEpsilon(q9,0.000001));

    // Euler test 2
    printf("euler angles 2\n");

    q8 = PigQuaternion(0.929976, 0.0369363, -0.0078811, 0.365675);
    q8.getEulerAngles(roll, pitch, yaw);
    printf("roll=%f, pitch=%f, yaw=%f\n", PigRad2Deg(roll), PigRad2Deg(pitch),
					PigRad2Deg(yaw));
    printf("s/b r=3.611488, p=-2.388312, y=42.855098\n");
    q9.setEulerAngles(roll, pitch, yaw);
    printf("q9: s=%f, vx=%f, vy=%f, vz=%f\n", q9.getS(), q9.getV().getX(), q9.getV().getY(), q9.getV().getZ());
    printf("s/b 0.929976, 0.0369363, -0.0078811, 0.365675\n");
    printf("Epsilon comparison q8,q9 with 0.000001 (s/b 1): %d\n",
					q8.equalEpsilon(q9,0.000001));
}

$!-----------------------------------------------------------------------------
$ create tpig_vector.imake
#define PROGRAM tpig_vector

#define MODULE_LIST TestPigVector.cc

#define MAIN_LANG_C_PLUS_PLUS
#define TEST

#define USES_C_PLUS_PLUS

#define LIB_P2SUB

$ Return
$!#############################################################################
