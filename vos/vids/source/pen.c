/************************************************************************/
/* Pen.c contains the set of subroutines used to perform pen drawing.
 */
#include "VIDSdefs.h"

/* Globally available variables describing the current pen state 	*/
Point		CurPos={1,1};	/* The current pen position		*/
GraphColor	CurColor=White;	/* The current pen color		*/
int		CurImp = -1;	/* The IMP in which to draw		*/
/************************************************************************/
/* MoveTo will set the current pen position to the horizontal and
 * vertical positions given by h and v respectively.
 */
int MoveTo(h, v)
  int h, v;			/* the new horizontal/vertical pen pos	*/
{
  CurPos.h = h;
  CurPos.v = v;
  return SUCCESS;
}
/************************************************************************/
/* LineTo will draw a line in the current color to the point specified
 * by (h,v) and reset the pen position to the new spot.
 */
int LineTo(env, h, v)
  VIDSEnvironment	*env;	/* Description of display device	*/
  int			h,v;	/* new endpoint for the line		*/
{
  Point newPos;			/* point struct for new endpoint	*/
  int status;			/* status holder			*/
  int imp;			/* plane in which to draw		*/
  
  newPos.h = h; newPos.v = v;
  imp = (CurImp < 0) ? env->grafIMP : CurImp;
  status = DrawLine(env->devUnit, imp, 255, GC(CurColor),
                  &CurPos, &newPos);
  if (status != SUCCESS) ABORT(FAIL, "Unable to draw line", "VIDS-VRDIERR");
  CurPos.h = h; CurPos.v = v;
  return SUCCESS;
}
/************************************************************************/
/* SetPenColor will set the current color of the pen to the given color
 */
int SetPenColor(color)
  GraphColor	color;		/* new color for pen			*/
{
  CurColor = color;
  return SUCCESS;
}
/************************************************************************/
/* GetPenColor will return the current pen color
 */
GraphColor GetPenColor()
{
  return CurColor;
}
/************************************************************************/
/* SetPenPlane will set the Image Memory Plane in which to draw for
 * succeeding pen commands.  If SetPenPlane is not called, the current
 * graphics plane is used.  The plane set with this routine remains in
 * effect until ClearPenPlane is called.
 */
int SetPenPlane(imp)
  int imp;		/* which image memory plane	*/
{
  CurImp = imp;
  return SUCCESS;
}
/************************************************************************/
/* ClearPenPlane will force all pen routines to use the current
 * graphics plane.  To use a different plane, use the SetPenPlane routine.
 */
int ClearPenPlane()
{
  CurImp = -1;
  return SUCCESS;
}
/************************************************************************/
/************************************************************************/
