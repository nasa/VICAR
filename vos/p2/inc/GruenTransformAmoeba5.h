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

