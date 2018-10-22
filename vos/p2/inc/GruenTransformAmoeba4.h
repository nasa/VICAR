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

