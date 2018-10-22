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

