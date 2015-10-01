$!****************************************************************************
$!
$! Build proc for MIPL module gruen_h
$! VPACK Version 1.9, Monday, March 03, 2014, 17:21:04
$!
$! Execute by entering:		$ @gruen_h
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
$ write sys$output "*** module gruen_h ***"
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
$ write sys$output "Invalid argument given to gruen_h.com file -- ", primary
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
$   if F$SEARCH("gruen_h.bld") .eqs. "" then gosub Build_File
$   if (primary .eqs. " ")
$   then
$      @gruen_h.bld "STD"
$   else
$      @gruen_h.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create gruen_h.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack gruen_h.com -mixed -
	-s GruenTransform.h GruenTransformAmoeba2.h GruenTransformAmoeba4.h -
	   GruenTransformAmoeba5.h GruenTransformAmoeba6.h -
	   GruenTransformAmoeba8.h gruen.h
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create GruenTransform.h
$ DECK/DOLLARS="$ VOKAGLEVE"
#ifndef GRUENTRANSFORM_H
#define GRUENTRANSFORM_H

////////////////////////////////////////////////////////////////////////
// Define a transform for use by the Gruen correlator.
//
// The general form of this is:
//   x' = ax + by + c + gxy
//   y' = dx + ey + f + hxy
// which implements a generic perspective transform (affine transform plus
// xy terms to map a square to any quadrilateral).
//
// Coefficients are supplied in an array.  The number and order of terms in
// this array varies based on the number of degrees of freedom desired and
// should be considered opaque.  For example, amoeba2 uses just a and d,
// while amoeba(6) uses a-f and amoeba8 uses all 8.  Applications should
// not look directly at e.g. coefs[1] and expect it to mean any particular
// thing.  These modes are implemented by subclasses for efficiency.
//
// The array is continually passed in rather than being kept in the class
// because it allows us to use a single class instantiation with all of
// the myriad calculations needed by the amoeba objective function.
//
// The purpose of this class is speed, not safety, so bounds are not checked...
////////////////////////////////////////////////////////////////////////

typedef enum {
    AmoebaNone = 0,		// not using amoeba
    Amoeba2,
    Amoeba4,
    Amoeba5,
    Amoeba6,
    Amoeba8
} TransformType;

////////////////////////////////////////////////////////////////////////
// Base class
////////////////////////////////////////////////////////////////////////

class GruenTransform
{
    
public:

////////////////////////////////////////////////////////////////////////
// Factory function using strings.  Note that "amoeba" with no number is
// an alias for amoeba6.
////////////////////////////////////////////////////////////////////////

    static GruenTransform *create(const char *type);

////////////////////////////////////////////////////////////////////////
// Factory function using the enum type
////////////////////////////////////////////////////////////////////////

    static GruenTransform *create(TransformType type);

////////////////////////////////////////////////////////////////////////
// Constructor, really does nothing.
////////////////////////////////////////////////////////////////////////

    GruenTransform() { };

////////////////////////////////////////////////////////////////////////
// Compute the X and Y values.  The given array contains the coefficients
// needed for this particular transform.
////////////////////////////////////////////////////////////////////////

    virtual double computeX(double *coefs, double x, double y) = 0;
    virtual double computeY(double *coefs, double x, double y) = 0;

////////////////////////////////////////////////////////////////////////
// This code is used in the innermost loop of the correlator, so performance
// is critical.  Therefore the computeX/Y functions, as well as a combo
// of both, are implemented as *macros* here so the compiler can inline
// them, avoiding a virtual function call.
//
// Usage: if you want to say:
//    newx = xform->computeX(coefs, x, y);
// instead say:
//    GruenTransformComputeX(newx, type, coefs, x, y);
// where type is the GruenTransform type as returned from getType().
////////////////////////////////////////////////////////////////////////

#define GruenTransformComputeX(type, coefs, result, x, y)		\
    { switch (type) {							\
	case Amoeba2:							\
	    result = GruenTransformComputeXAmoeba2(coefs, x, y); 	\
	    break;							\
	case Amoeba4:							\
	    result = GruenTransformComputeXAmoeba4(coefs, x, y);	\
	    break;							\
	case Amoeba5:							\
	    result = GruenTransformComputeXAmoeba5(coefs, x, y); 	\
	    break;							\
	case Amoeba6:							\
	    result = GruenTransformComputeXAmoeba6(coefs, x, y); 	\
	    break;							\
	case Amoeba8:							\
	    result = GruenTransformComputeXAmoeba8(coefs, x, y); 	\
	    break;							\
    } }

#define GruenTransformComputeY(type, coefs, result, x, y)		\
    { switch (type) {							\
	case Amoeba2:							\
	    result = GruenTransformComputeYAmoeba2(coefs, x, y);	\
	    break;							\
	case Amoeba4:							\
	    result = GruenTransformComputeYAmoeba4(coefs, x, y);	\
	    break;							\
	case Amoeba5:							\
	    result = GruenTransformComputeYAmoeba5(coefs, x, y);	\
	    break;							\
	case Amoeba6:							\
	    result = GruenTransformComputeYAmoeba6(coefs, x, y);	\
	    break;							\
	case Amoeba8:							\
	    result = GruenTransformComputeYAmoeba8(coefs, x, y);	\
	    break;							\
    } }

#define GruenTransformComputeXandY(type, coefs, result_x, result_y, x, y) \
    { switch (type) {							\
	case Amoeba2:							\
	    result_x = GruenTransformComputeXAmoeba2(coefs, x, y); 	\
	    result_y = GruenTransformComputeYAmoeba2(coefs, x, y); 	\
	    break;							\
	case Amoeba4:							\
	    result_x = GruenTransformComputeXAmoeba4(coefs, x, y);	\
	    result_y = GruenTransformComputeYAmoeba4(coefs, x, y);	\
	    break;							\
	case Amoeba5:							\
	    result_x = GruenTransformComputeXAmoeba5(coefs, x, y); 	\
	    result_y = GruenTransformComputeYAmoeba5(coefs, x, y); 	\
	    break;							\
	case Amoeba6:							\
	    result_x = GruenTransformComputeXAmoeba6(coefs, x, y); 	\
	    result_y = GruenTransformComputeYAmoeba6(coefs, x, y); 	\
	    break;							\
	case Amoeba8:							\
	    result_x = GruenTransformComputeXAmoeba8(coefs, x, y); 	\
	    result_y = GruenTransformComputeYAmoeba8(coefs, x, y); 	\
	    break;							\
    } }

////////////////////////////////////////////////////////////////////////
// Functions to explicitly return a-h coefficients from the array.
// Those that do not apply to this specific transform are returned as 1 or 0,
// as appropriate for a no-op.
//   x' = ax + by + c + gxy
//   y' = dx + ey + f + hxy
////////////////////////////////////////////////////////////////////////

    virtual double getA(double *coefs) { return 1.0; };
    virtual double getB(double *coefs) { return 0.0; };
    virtual double getC(double *coefs) { return 0.0; };
    virtual double getD(double *coefs) { return 0.0; };
    virtual double getE(double *coefs) { return 1.0; };
    virtual double getF(double *coefs) { return 0.0; };
    virtual double getG(double *coefs) { return 0.0; };
    virtual double getH(double *coefs) { return 0.0; };

////////////////////////////////////////////////////////////////////////
// Get all the coefs at once
////////////////////////////////////////////////////////////////////////

    virtual void getCoefs(double *coefs,
		double *a, double *b, double *c, double *d,
		double *e, double *f, double *g, double *h) = 0;

////////////////////////////////////////////////////////////////////////
// Set the a-h coefficients into the coefs array.  Those that are not
// used by this particular transform are ignored.
////////////////////////////////////////////////////////////////////////

    virtual void setCoefs(double *coefs,
		double a, double b, double c, double d,
		double e, double f, double g, double h) = 0;

////////////////////////////////////////////////////////////////////////
// Returns the number of coefficients for this transform.
////////////////////////////////////////////////////////////////////////

    virtual int getNumCoefs() = 0;

////////////////////////////////////////////////////////////////////////
// Returns the type code for this instantiation.
////////////////////////////////////////////////////////////////////////

    virtual TransformType getType() = 0;
};

// Include here to get the macros.  After the class def so they have
// their base class defined.

#include "GruenTransformAmoeba2.h"
#include "GruenTransformAmoeba4.h"
#include "GruenTransformAmoeba5.h"
#include "GruenTransformAmoeba6.h"
#include "GruenTransformAmoeba8.h"

#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create GruenTransformAmoeba2.h
$ DECK/DOLLARS="$ VOKAGLEVE"
#ifndef GRUENTRANSFORMAMOEBA2_H
#define GRUENTRANSFORMAMOEBA2_H

#include "GruenTransform.h"

////////////////////////////////////////////////////////////////////////
// Defines the 2-dof Amoeba transform for use by the Gruen correlator.
//
// This uses coefficients c and f only.  This allows translation of the
// correlation window only.  It is the fastest mode, but generally the
// least accurate.
////////////////////////////////////////////////////////////////////////

class GruenTransformAmoeba2 : public GruenTransform
{
  public:

////////////////////////////////////////////////////////////////////////
// Constructor, really does nothing.
////////////////////////////////////////////////////////////////////////

    GruenTransformAmoeba2() : GruenTransform() { };

////////////////////////////////////////////////////////////////////////
// Compute the X and Y values.  The given array contains the coefficients
// needed for this particular transform.
// C = 0
// F = 1
////////////////////////////////////////////////////////////////////////

    virtual double computeX(double *coefs, double x, double y)
    {
	return x + coefs[0];
    };
    virtual double computeY(double *coefs, double x, double y)
    {
	return y + coefs[1];
    };

////////////////////////////////////////////////////////////////////////
// This code is used in the innermost loop of the correlator, so performance
// is critical.	 Therefore the computeX/Y functions, as well as splits of
// those which precompute for a constant Y, are implemented as *macros*
// here so the compiler can inline them, avoiding a virtual function call.
// Makes a big difference... better than 50% improvement in speed!
//
// Usage: if you want to say:
//    newx = xform->computeX(coefs, x, y);
// instead say:
//    GruenTransformComputeX(newx, type, coefs, x, y);
// where type is the GruenTransform type as returned from getType().
////////////////////////////////////////////////////////////////////////

#define GruenTransformComputeXAmoeba2(coefs, x, y)		\
	((x) + (coefs)[0])
#define GruenTransformComputeYAmoeba2(coefs, x, y)		\
	((y) + (coefs)[1])

////////////////////////////////////////////////////////////////////////
// Macros (as above), which precompute some terms for efficiency.  We assume
// Y is constant and X is varying here (as it is in the inner gruen loop).
// The precomputed values are fed back in to the second function.  They are
// additive, so the caller may add additional terms to them.
////////////////////////////////////////////////////////////////////////

#define GruenTransformPreComputeConstYAmoeba2(coefs, pre_x, pre_y, y)	\
	{ pre_x = (coefs)[0];						\
	  pre_y = (y) + (coefs)[1]; }

#define GruenTransformComputeFromConstYAmoeba2(coefs, res_x, res_y, pre_x, pre_y, x, y)	\
	{ res_x = (x) + (pre_x);					\
	  res_y = (pre_y); }

////////////////////////////////////////////////////////////////////////
// Functions to explicitly return a-h coefficients from the array.
// Those that do not apply to this specific transform are returned as 1 or 0,
// as appropriate for a no-op.
//   x' = ax + by + c + gxy
//   y' = dx + ey + f + hxy
////////////////////////////////////////////////////////////////////////

    virtual double getC(double *coefs) { return coefs[0]; };
    virtual double getF(double *coefs) { return coefs[1]; };

////////////////////////////////////////////////////////////////////////
// Get all the coefs at once
////////////////////////////////////////////////////////////////////////

    virtual void getCoefs(double *coefs,
                double *a, double *b, double *c, double *d,
                double *e, double *f, double *g, double *h)
    {
	*a = 1.0;
	*b = 0.0;
	*c = coefs[0];
	*d = 0.0;
	*e = 1.0;
	*f = coefs[1];
	*g = 0.0;
	*h = 0.0;
    }

////////////////////////////////////////////////////////////////////////
// Set the a-h coefficients into the coefs array.  Those that are not
// used by this particular transform are ignored.
////////////////////////////////////////////////////////////////////////

    virtual void setCoefs(double *coefs,
		double a, double b, double c, double d,
		double e, double f, double g, double h)
    {
	coefs[0] = c;
	coefs[1] = f;
    };

////////////////////////////////////////////////////////////////////////
// Returns the number of coefficients for this transform.
////////////////////////////////////////////////////////////////////////

    virtual int getNumCoefs() { return 2; };

////////////////////////////////////////////////////////////////////////
// Returns the type code for this instantiation.
////////////////////////////////////////////////////////////////////////
 
    virtual TransformType getType() { return Amoeba2; }

};

#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create GruenTransformAmoeba4.h
$ DECK/DOLLARS="$ VOKAGLEVE"
#ifndef GRUENTRANSFORMAMOEBA4_H
#define GRUENTRANSFORMAMOEBA4_H

#include "GruenTransform.h"

////////////////////////////////////////////////////////////////////////
// Defines the 4-dof Amoeba transform for use by the Gruen correlator.
//
// This uses coefficients b,c,f,g.  This allows for translation, shear,
// and trapezoid in x, but only translation in y.  This models a pair of
// epipolar-aligned stereo cameras looking out at a flat plane (such as
// from a rover).
////////////////////////////////////////////////////////////////////////

class GruenTransformAmoeba4 : public GruenTransform
{
  public:

////////////////////////////////////////////////////////////////////////
// Constructor, really does nothing.
////////////////////////////////////////////////////////////////////////

    GruenTransformAmoeba4() : GruenTransform() { };

////////////////////////////////////////////////////////////////////////
// Compute the X and Y values.  The given array contains the coefficients
// needed for this particular transform.
// B = 0
// C = 1
// F = 2
// G = 3
////////////////////////////////////////////////////////////////////////

    virtual double computeX(double *coefs, double x, double y)
    {
	return x + coefs[0] * y + coefs[1] + coefs[3] * x * y;
    };
    virtual double computeY(double *coefs, double x, double y)
    {
	return  y + coefs[2];
    };

////////////////////////////////////////////////////////////////////////
// This code is used in the innermost loop of the correlator, so performance
// is critical.	 Therefore the computeX/Y functions, as well as splits of
// those which precompute for a constant Y, are implemented as *macros*
// here so the compiler can inline them, avoiding a virtual function call.
// Makes a big difference... better than 50% improvement in speed!
//
// Usage: if you want to say:
//    newx = xform->computeX(coefs, x, y);
// instead say:
//    GruenTransformComputeX(newx, type, coefs, x, y);
// where type is the GruenTransform type as returned from getType().
////////////////////////////////////////////////////////////////////////

#define GruenTransformComputeXAmoeba4(coefs, x, y)		\
	((x) + (coefs)[0] * (y) + (coefs)[1] + (coefs)[3] * (x) * (y))
#define GruenTransformComputeYAmoeba4(coefs, x, y)		\
	((y) + (coefs)[2])

////////////////////////////////////////////////////////////////////////
// Macros (as above), which precompute some terms for efficiency.  We assume
// Y is constant and X is varying here (as it is in the inner gruen loop).
// The precomputed values are fed back in to the second function.  They are
// additive, so the caller may add additional terms to them.
////////////////////////////////////////////////////////////////////////

#define GruenTransformPreComputeConstYAmoeba4(coefs, pre_x, pre_y, y)	\
	{ pre_x = (coefs)[0] * (y) + (coefs)[1];			\
	  pre_y = (y) + (coefs)[2]; }

#define GruenTransformComputeFromConstYAmoeba4(coefs, res_x, res_y, pre_x, pre_y, x, y) \
        { res_x = (x) + (coefs)[3] * (x) * (y) + (pre_x);		\
	  res_y = (pre_y); }

////////////////////////////////////////////////////////////////////////
// Functions to explicitly return a-h coefficients from the array.
// Those that do not apply to this specific transform are returned as 1 or 0,
// as appropriate for a no-op.
//   x' = ax + by + c + gxy
//   y' = dx + ey + f + hxy
////////////////////////////////////////////////////////////////////////

    virtual double getB(double *coefs) { return coefs[0]; };
    virtual double getC(double *coefs) { return coefs[1]; };
    virtual double getF(double *coefs) { return coefs[2]; };
    virtual double getG(double *coefs) { return coefs[3]; };

////////////////////////////////////////////////////////////////////////
// Get all the coefs at once
////////////////////////////////////////////////////////////////////////

    virtual void getCoefs(double *coefs,
                double *a, double *b, double *c, double *d,
                double *e, double *f, double *g, double *h)
    {
        *a = 1.0;
        *b = coefs[0];
        *c = coefs[1];
        *d = 0.0;
        *e = 1.0;
        *f = coefs[2];
        *g = coefs[3];
        *h = 0.0;
    }

////////////////////////////////////////////////////////////////////////
// Set the a-h coefficients into the coefs array.  Those that are not
// used by this particular transform are ignored.
////////////////////////////////////////////////////////////////////////

    virtual void setCoefs(double *coefs,
		double a, double b, double c, double d,
		double e, double f, double g, double h)
    {
	coefs[0] = b;
	coefs[1] = c;
	coefs[2] = f;
	coefs[3] = g;
    };

////////////////////////////////////////////////////////////////////////
// Returns the number of coefficients for this transform.
////////////////////////////////////////////////////////////////////////

    virtual int getNumCoefs() { return 4; };

////////////////////////////////////////////////////////////////////////
// Returns the type code for this instantiation.
////////////////////////////////////////////////////////////////////////
 
    virtual TransformType getType() { return Amoeba4; }


};

#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create GruenTransformAmoeba5.h
$ DECK/DOLLARS="$ VOKAGLEVE"
#ifndef GRUENTRANSFORMAMOEBA5_H
#define GRUENTRANSFORMAMOEBA5_H

#include "GruenTransform.h"

////////////////////////////////////////////////////////////////////////
// Defines the 5-dof Amoeba transform for use by the Gruen correlator.
//
// This uses 5 coefficients, a,b,c,f,g.  This allows any transform in x
// (translation, shear, trapezoid, scale) but only translation in y.  THis
// models a pair of epipolar-aligned stereo cameras looking at a more general
// scene (not constrained to a flat plane.  This is generally the best mode
// for epipolar-aligned cameras.
////////////////////////////////////////////////////////////////////////

class GruenTransformAmoeba5 : public GruenTransform
{
  public:

////////////////////////////////////////////////////////////////////////
// Constructor, really does nothing.
////////////////////////////////////////////////////////////////////////

    GruenTransformAmoeba5() : GruenTransform() { };

////////////////////////////////////////////////////////////////////////
// Compute the X and Y values.  The given array contains the coefficients
// needed for this particular transform.
// A = 0
// B = 1
// C = 2
// D = n/a
// E = n/a
// F = 3
// G = 4
// H = n/a
////////////////////////////////////////////////////////////////////////

    virtual double computeX(double *coefs, double x, double y)
    {
	return coefs[0] * x + coefs[1] * y + coefs[2] + coefs[4] * x * y;
    };
    virtual double computeY(double *coefs, double x, double y)
    {
	return y + coefs[3];
    };

////////////////////////////////////////////////////////////////////////
// This code is used in the innermost loop of the correlator, so performance
// is critical.	 Therefore the computeX/Y functions, as well as splits of
// those which precompute for a constant Y, are implemented as *macros*
// here so the compiler can inline them, avoiding a virtual function call.
// Makes a big difference... better than 50% improvement in speed!
//
// Usage: if you want to say:
//    newx = xform->computeX(coefs, x, y);
// instead say:
//    GruenTransformComputeX(newx, type, coefs, x, y);
// where type is the GruenTransform type as returned from getType().
////////////////////////////////////////////////////////////////////////

#define GruenTransformComputeXAmoeba5(coefs, x, y)		\
	((coefs)[0] * (x) + (coefs)[1] * (y) + (coefs)[2] + (coefs)[4]*(x)*(y))
#define GruenTransformComputeYAmoeba5(coefs, x, y)		\
	((y) + (coefs)[3])

////////////////////////////////////////////////////////////////////////
// Macros (as above), which precompute some terms for efficiency.  We assume
// Y is constant and X is varying here (as it is in the inner gruen loop).
// The precomputed values are fed back in to the second function.  They are
// additive, so the caller may add additional terms to them.
////////////////////////////////////////////////////////////////////////

#define GruenTransformPreComputeConstYAmoeba5(coefs, pre_x, pre_y, y)	\
	{ pre_x = (coefs)[1] * (y) + (coefs)[2];			\
	  pre_y = (y) + (coefs)[3]; }

#define GruenTransformComputeFromConstYAmoeba5(coefs, res_x, res_y, pre_x, pre_y, x, y) \
        { res_x = (coefs)[0] * (x) + (coefs)[4] * (x) * (y) + (pre_x);					\
	  res_y = (pre_y); }

////////////////////////////////////////////////////////////////////////
// Functions to explicitly return a-h coefficients from the array.
// Those that do not apply to this specific transform are returned as 1 or 0,
// as appropriate for a no-op.
//   x' = ax + by + c + gxy
//   y' = dx + ey + f + hxy
////////////////////////////////////////////////////////////////////////

    virtual double getA(double *coefs) { return coefs[0]; };
    virtual double getB(double *coefs) { return coefs[1]; };
    virtual double getC(double *coefs) { return coefs[2]; };
    virtual double getF(double *coefs) { return coefs[3]; };
    virtual double getG(double *coefs) { return coefs[4]; };

////////////////////////////////////////////////////////////////////////
// Get all the coefs at once
////////////////////////////////////////////////////////////////////////

    virtual void getCoefs(double *coefs,
                double *a, double *b, double *c, double *d,
                double *e, double *f, double *g, double *h)
    {
        *a = coefs[0];
        *b = coefs[1];
        *c = coefs[2];
        *d = 0.0;
        *e = 1.0;
        *f = coefs[3];
        *g = coefs[4];
        *h = 0.0;
    }

////////////////////////////////////////////////////////////////////////
// Set the a-h coefficients into the coefs array.  Those that are not
// used by this particular transform are ignored.
////////////////////////////////////////////////////////////////////////

    virtual void setCoefs(double *coefs,
		double a, double b, double c, double d,
		double e, double f, double g, double h)
    {
	coefs[0] = a;
	coefs[1] = b;
	coefs[2] = c;
	coefs[3] = f;
	coefs[4] = g;
    };

////////////////////////////////////////////////////////////////////////
// Returns the number of coefficients for this transform.
////////////////////////////////////////////////////////////////////////

    virtual int getNumCoefs() { return 5; };

////////////////////////////////////////////////////////////////////////
// Returns the type code for this instantiation.
////////////////////////////////////////////////////////////////////////
 
    virtual TransformType getType() { return Amoeba5; }

};

#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create GruenTransformAmoeba6.h
$ DECK/DOLLARS="$ VOKAGLEVE"
#ifndef GRUENTRANSFORMAMOEBA6_H
#define GRUENTRANSFORMAMOEBA6_H

#include "GruenTransform.h"

////////////////////////////////////////////////////////////////////////
// Defines the 6-dof Amoeba transform for use by the Gruen correlator.
//
// This uses 6 coefficients, a-f (all but the xy terms).  This implements
// a generic affine transform with no trapezoidal parameters.  Translation,
// scale, shear, and rotation are modeled.  This mode is historically the
// primary mode, but does not model in-situ cameras very well.
////////////////////////////////////////////////////////////////////////

class GruenTransformAmoeba6 : public GruenTransform
{
  public:

////////////////////////////////////////////////////////////////////////
// Constructor, really does nothing.
////////////////////////////////////////////////////////////////////////

    GruenTransformAmoeba6() : GruenTransform() { };

////////////////////////////////////////////////////////////////////////
// Compute the X and Y values.  The given array contains the coefficients
// needed for this particular transform.
// A = 0
// B = 1
// C = 2
// D = 3
// E = 4
// F = 5
// G = n/a
// H = n/a
////////////////////////////////////////////////////////////////////////

    virtual double computeX(double *coefs, double x, double y)
    {
	return coefs[0] * x + coefs[1] * y + coefs[2];
    };
    virtual double computeY(double *coefs, double x, double y)
    {
	return coefs[3] * x + coefs[4] * y + coefs[5];
    };

////////////////////////////////////////////////////////////////////////
// This code is used in the innermost loop of the correlator, so performance
// is critical.	 Therefore the computeX/Y functions, as well as splits of
// those which precompute for a constant Y, are implemented as *macros*
// here so the compiler can inline them, avoiding a virtual function call.
// Makes a big difference... better than 50% improvement in speed!
//
// Usage: if you want to say:
//    newx = xform->computeX(coefs, x, y);
// instead say:
//    GruenTransformComputeX(newx, type, coefs, x, y);
// where type is the GruenTransform type as returned from getType().
////////////////////////////////////////////////////////////////////////

#define GruenTransformComputeXAmoeba6(coefs, x, y)		\
	((coefs)[0] * (x) + (coefs)[1] * (y) + (coefs)[2])
#define GruenTransformComputeYAmoeba6(coefs, x, y)		\
	((coefs)[3] * (x) + (coefs)[4] * (y) + (coefs)[5])

////////////////////////////////////////////////////////////////////////
// Macros (as above), which precompute some terms for efficiency.  We assume
// Y is constant and X is varying here (as it is in the inner gruen loop).
// The precomputed values are fed back in to the second function.  They are
// additive, so the caller may add additional terms to them.
////////////////////////////////////////////////////////////////////////

#define GruenTransformPreComputeConstYAmoeba6(coefs, pre_x, pre_y, y)	\
	{ pre_x = (coefs)[1] * (y) + (coefs)[2];			\
	  pre_y = (coefs)[4] * (y) + (coefs)[5]; }

#define GruenTransformComputeFromConstYAmoeba6(coefs, res_x, res_y, pre_x, pre_y, x, y) \
        { res_x = (coefs)[0] * (x) + (pre_x);				\
	  res_y = (coefs)[3] * (x) + (pre_y); }

////////////////////////////////////////////////////////////////////////
// Functions to explicitly return a-h coefficients from the array.
// Those that do not apply to this specific transform are returned as 1 or 0,
// as appropriate for a no-op.
//   x' = ax + by + c + gxy
//   y' = dx + ey + f + hxy
////////////////////////////////////////////////////////////////////////

    virtual double getA(double *coefs) { return coefs[0]; };
    virtual double getB(double *coefs) { return coefs[1]; };
    virtual double getC(double *coefs) { return coefs[2]; };
    virtual double getD(double *coefs) { return coefs[3]; };
    virtual double getE(double *coefs) { return coefs[4]; };
    virtual double getF(double *coefs) { return coefs[5]; };

////////////////////////////////////////////////////////////////////////
// Get all the coefs at once
////////////////////////////////////////////////////////////////////////

    virtual void getCoefs(double *coefs,
                double *a, double *b, double *c, double *d,
                double *e, double *f, double *g, double *h)
    {
        *a = coefs[0];
        *b = coefs[1];
        *c = coefs[2];
        *d = coefs[3];
        *e = coefs[4];
        *f = coefs[5];
        *g = 0.0;
        *h = 0.0;
    }

////////////////////////////////////////////////////////////////////////
// Set the a-h coefficients into the coefs array.  Those that are not
// used by this particular transform are ignored.
////////////////////////////////////////////////////////////////////////

    virtual void setCoefs(double *coefs,
		double a, double b, double c, double d,
		double e, double f, double g, double h)
    {
	coefs[0] = a;
	coefs[1] = b;
	coefs[2] = c;
	coefs[3] = d;
	coefs[4] = e;
	coefs[5] = f;
    };

////////////////////////////////////////////////////////////////////////
// Returns the number of coefficients for this transform.
////////////////////////////////////////////////////////////////////////

    virtual int getNumCoefs() { return 6; };

////////////////////////////////////////////////////////////////////////
// Returns the type code for this instantiation.
////////////////////////////////////////////////////////////////////////
 
    virtual TransformType getType() { return Amoeba6; }


};

#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create GruenTransformAmoeba8.h
$ DECK/DOLLARS="$ VOKAGLEVE"
#ifndef GRUENTRANSFORMAMOEBA8_H
#define GRUENTRANSFORMAMOEBA8_H

#include "GruenTransform.h"

////////////////////////////////////////////////////////////////////////
// Defines the 8-dof Amoeba transform for use by the Gruen correlator.
//
// This uses all 8 coefficients.  This implements a generic perspective
// transform with translation, scale, shear, rotation, and trapezoid (or
// perspective) transforms.  This is the most general mode, but may be
// underconstrained for small window sizes.
////////////////////////////////////////////////////////////////////////

class GruenTransformAmoeba8 : public GruenTransform
{
  public:

////////////////////////////////////////////////////////////////////////
// Constructor, really does nothing.
////////////////////////////////////////////////////////////////////////

    GruenTransformAmoeba8() : GruenTransform() { };

////////////////////////////////////////////////////////////////////////
// Compute the X and Y values.  The given array contains the coefficients
// needed for this particular transform.
// A = 0
// B = 1
// C = 2
// D = 3
// E = 4
// F = 5
// G = 6
// H = 7
////////////////////////////////////////////////////////////////////////

    virtual double computeX(double *coefs, double x, double y)
    {
	return coefs[0] * x + coefs[1] * y + coefs[2] + coefs[6] * x * y;
    };
    virtual double computeY(double *coefs, double x, double y)
    {
	return coefs[3] * x + coefs[4] * y + coefs[5] + coefs[7] * x * y;
    };

////////////////////////////////////////////////////////////////////////
// This code is used in the innermost loop of the correlator, so performance
// is critical.	 Therefore the computeX/Y functions, as well as splits of
// those which precompute for a constant Y, are implemented as *macros*
// here so the compiler can inline them, avoiding a virtual function call.
// Makes a big difference... better than 50% improvement in speed!
//
// Usage: if you want to say:
//    newx = xform->computeX(coefs, x, y);
// instead say:
//    GruenTransformComputeX(newx, type, coefs, x, y);
// where type is the GruenTransform type as returned from getType().
////////////////////////////////////////////////////////////////////////

#define GruenTransformComputeXAmoeba8(coefs, x, y)		\
	((coefs)[0] * (x) + (coefs)[1] * (y) + (coefs)[2] + (coefs)[6]*(x)*(y))
#define GruenTransformComputeYAmoeba8(coefs, x, y)		\
	((coefs)[3] * (x) + (coefs)[4] * (y) + (coefs)[5] + (coefs)[7]*(x)*(y))

////////////////////////////////////////////////////////////////////////
// Macros (as above), which precompute some terms for efficiency.  We assume
// Y is constant and X is varying here (as it is in the inner gruen loop).
// The precomputed values are fed back in to the second function.  They are
// additive, so the caller may add additional terms to them.
////////////////////////////////////////////////////////////////////////

#define GruenTransformPreComputeConstYAmoeba8(coefs, pre_x, pre_y, y)	\
	{ pre_x = (coefs)[1] * (y) + (coefs)[2];			\
	  pre_y = (coefs)[4] * (y) + (coefs)[5]; }

#define GruenTransformComputeFromConstYAmoeba8(coefs, res_x, res_y, pre_x, pre_y, x, y) \
        { res_x = (coefs)[0] * (x) + (coefs)[6] * (x) * (y) + (pre_x);	\
	  res_y = (coefs)[3] * (x) + (coefs)[7] * (x) * (y) + (pre_y); }

////////////////////////////////////////////////////////////////////////
// Functions to explicitly return a-h coefficients from the array.
// Those that do not apply to this specific transform are returned as 1 or 0,
// as appropriate for a no-op.
//   x' = ax + by + c + gxy
//   y' = dx + ey + f + hxy
////////////////////////////////////////////////////////////////////////

    virtual double getA(double *coefs) { return coefs[0]; };
    virtual double getB(double *coefs) { return coefs[1]; };
    virtual double getC(double *coefs) { return coefs[2]; };
    virtual double getD(double *coefs) { return coefs[3]; };
    virtual double getE(double *coefs) { return coefs[4]; };
    virtual double getF(double *coefs) { return coefs[5]; };
    virtual double getG(double *coefs) { return coefs[6]; };
    virtual double getH(double *coefs) { return coefs[7]; };

////////////////////////////////////////////////////////////////////////
// Get all the coefs at once
////////////////////////////////////////////////////////////////////////

    virtual void getCoefs(double *coefs,
                double *a, double *b, double *c, double *d,
                double *e, double *f, double *g, double *h)
    {
        *a = coefs[0];
        *b = coefs[1];
        *c = coefs[2];
        *d = coefs[3];
        *e = coefs[4];
        *f = coefs[5];
        *g = coefs[6];
        *h = coefs[7];
    }

////////////////////////////////////////////////////////////////////////
// Set the a-h coefficients into the coefs array.  Those that are not
// used by this particular transform are ignored.
////////////////////////////////////////////////////////////////////////

    virtual void setCoefs(double *coefs,
		double a, double b, double c, double d,
		double e, double f, double g, double h)
    {
	coefs[0] = a;
	coefs[1] = b;
	coefs[2] = c;
	coefs[3] = d;
	coefs[4] = e;
	coefs[5] = f;
	coefs[6] = g;
	coefs[7] = h;
    };

////////////////////////////////////////////////////////////////////////
// Returns the number of coefficients for this transform.
////////////////////////////////////////////////////////////////////////

    virtual int getNumCoefs() { return 8; };

////////////////////////////////////////////////////////////////////////
// Returns the type code for this instantiation.
////////////////////////////////////////////////////////////////////////
 
    virtual TransformType getType() { return Amoeba8; }

};

#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create gruen.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/************************************************************************/
/* Gruen correlation algorithm.  See gruen.c file for meanings of	*/
/* all the arguments.							*/
/* gruen() is the original API; gruen2() has several new enhancements.  */
/************************************************************************/

#ifndef _GRUEN_H
#define _GRUEN_H

#include "xvmaininc.h"
#include "SimpleImage.h"

#ifdef __cplusplus
extern "C" {
#endif

/* Main gruen algorithm */

#ifdef _NO_PROTO
int gruen();
int gruen2();
int gruen3();
#else
int gruen(double *left, int nlw, int nsw, int max_left_area,
	   double *right, int nlw2, int nsw2, int max_right_area,
	   double *correl,
	   double *line_offset, double *samp_offset,
	   double line_coef[3], double samp_coef[3],
	   double line_coef_limits[3][2], double samp_coef_limits[3][2],
	   double line_temp[3], double samp_temp[3],
	   double percent, int limits, double *quality, int mode);

int gruen2(double *left, int nlw, int nsw, int max_left_area,
	   double *right, int nlw2, int nsw2, int max_right_area,
	   double *correl,
	   double *line_offset, double *samp_offset,
	   double line_coef[4], double samp_coef[4],
	   double line_coef_limits[3][2], double samp_coef_limits[3][2],
	   double line_temp[3], double samp_temp[3],
	   double percent, int limits, double *quality, int mode,
	   double ftol, int inv_flag);

int gruen3(SimpleImage<double> *left_img,
	   SimpleImage<double> *right_img,
	   SimpleImage<double> *&correl_img,
	   double *line_offset, double *samp_offset,
	   double line_coef[4], double samp_coef[4],
	   double line_coef_limits[3][2], double samp_coef_limits[3][2],
	   double line_temp[3], double samp_temp[3],
	   double percent, int limits, double *quality, int mode,
	   double ftol, int inv_flag);
#endif

#ifdef __cplusplus
}
#endif

#endif	/* _GRUEN_H */

$ VOKAGLEVE
$ Return
$!#############################################################################
