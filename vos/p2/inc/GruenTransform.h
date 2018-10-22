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

