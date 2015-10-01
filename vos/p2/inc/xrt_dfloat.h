/************************************************************************/
/* Wrappers to handle D-to-G floating point conversion for XRT/graph	*/
/* under Alpha/VMS.  This is required because VICAR code is compiled	*/
/* for d_float but the XRT library is compiled for g_float.  For XRT/3D,*/
/* this approach is not practical, so a separate version is required.	*/
/*									*/
/* WARNING:  XRT/Table uses doubles only in XrtTblDrawPS().  However,	*/
/* the arglist for that routine does not lend itself to this kind of	*/
/* wrapper.  If you pass in doubles, you *must* use the converters	*/
/* directly!								*/
/*									*/
/* The same Xrtd calls may be used for non-Alpha machines, or even for	*/
/* Alpha compiled for g-float.  In that case, the converter routines	*/
/* are no-ops.								*/
/*									*/
/* Floats are not a problem because they are the same between d and g	*/
/* float; only the double format is different.				*/
/*									*/
/* Note that XrtGraph.h must be included before this file.  Also,	*/
/* <stdio.h> may be needed as well.					*/
/************************************************************************/

#ifndef XRT_DFLOAT_H
#define XRT_DFLOAT_H

#if defined(__VMS) && defined(__ALPHA) && __D_FLOAT

#include <cvt$routines.h>
#include <cvtdef.h>

/* General-purpose converters */

static double XrtConvertDtoGfloat(double dfloat)
{
   double gfloat;
   cvt$convert_float((void *)&dfloat, CVT$K_VAX_D, (void *)&gfloat, CVT$K_VAX_G,
			CVT$M_ROUND_TO_NEAREST);   
   return gfloat;
}

static double XrtConvertGtoDfloat(double gfloat)
{
   double dfloat;
   cvt$convert_float((void *)&gfloat, CVT$K_VAX_G, (void *)&dfloat, CVT$K_VAX_D,
			CVT$M_ROUND_TO_NEAREST);   
   return dfloat;
}

#else	/* Not Alpha or not D */

/* No-op converters */

static double XrtConvertDtoGfloat(double dfloat)
{ return dfloat; }

static double XrtConvertGtoDfloat(double gfloat)
{ return gfloat; }

#endif	/* VMS && Alpha */

/************************************************************************/

/* XRT wrappers */

static void XrtdUnmap(Widget w1, int i2, double d3, double d4, XrtMapResult *mr)
{
   XrtUnmap(w1, i2, XrtConvertDtoGfloat(d3), XrtConvertDtoGfloat(d4), mr);
}

static int XrtdDrawPS(Widget w1, FILE *f2, char *c3, int i4, double d5,
	double d6, double d7, int i8, double d9, double d10, double d11,
	double d12, char *c13, int i14, char *c15, int i16, char *c17, int i18,
	char *c19, int i20, int i21, int i22, int i23)
{
   return XrtDrawPS(w1, f2, c3, i4, XrtConvertDtoGfloat(d5),
	XrtConvertDtoGfloat(d6), XrtConvertDtoGfloat(d7), i8,
	XrtConvertDtoGfloat(d9), XrtConvertDtoGfloat(d10),
	XrtConvertDtoGfloat(d11), XrtConvertDtoGfloat(d12), c13, i14, c15, i16,
	c17, i18, c19, i20, i21, i22, i23);
}

static int XrtdGenDataAppendPt(XrtData *xd1, int i2, double d3, double d4)
{
   return XrtGenDataAppendPt(xd1, i2, XrtConvertDtoGfloat(d3),
	XrtConvertDtoGfloat(d4));
}

static int XrtdArrDataAppendPts(XrtData *xd1, double d2, float *f3)
{
   return XrtArrDataAppendPts(xd1, XrtConvertDtoGfloat(d2), f3);
}

static XtArgVal XrtdFloatToArgVal(double d1)
{
   return XrtFloatToArgVal(XrtConvertDtoGfloat(d1));
}

#endif
