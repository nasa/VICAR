#include "VIDSdefs.h"
/* CursorUtils.c contains various utilities for handling cursors in VIDS.
 */
/************************************************************************/
/* DisplayCursor makes the current cursor visible.  No other attributes
 * are changed.
 */
int DisplayCursor(env)
  VIDSEnvironment	*env;
{
  int status;
  
  status = zdcon(env->devUnit, env->cursor.number, env->cursor.form,
               env->cursor.blink);
  if (status != SUCCESS)
    ABORT(FAIL, "Sorry, cannot turn on the cursor", "VIDS-VRDIERR");
  return SUCCESS;
}
/************************************************************************/
/* HideCursor makes the current cursor invisible.
 */
int HideCursor(env)
  VIDSEnvironment	*env;
{
  int status;
  
  status = zdcoff(env->devUnit, env->cursor.number);
  if (status != SUCCESS)
    ABORT(FAIL, "Sorry, cannot turn off the cursor", "VIDS-VRDIERR");
  return SUCCESS;
}
/************************************************************************/
/* SetCursor sets attributes for the cursor and makes the new cursor
 * the "current" cursor.
 */
int SetCursor(env, number, form, blink)
  VIDSEnvironment	*env;
  int			number, form, blink;
{
  env->cursor.number = number;
  env->cursor.form = form;
  env->cursor.blink = blink;
  return DisplayCursor(env);
}
/************************************************************************/
/* Translates raw cursor coordinates to the image plane.  This routine does
 * NOT check the coordinates to make sure they are inside the IMP.
 */
int CursRaw2IMP(env, imp, samp, line, outsamp, outline)
  VIDSEnvironment *env;		/* Environment for nlMax, nsMax		*/
  int imp;			/* image memory plane in question	*/
  int samp, line;		/* cursor position relative to screen	*/
  int *outsamp, *outline;	/* cursor position rel to image plane	*/
{
  int unit;			/* VRDI unit number			*/
  int zoom;			/* HW zoom factor			*/
  int nlMax, nsMax;		/* max size of image memory planes	*/

  unit = env->devUnit;
  nlMax = env->nlMax;
  nsMax = env->nsMax;
  zoom = zdszoom(unit, imp);	/* Compensate for hardware zoom		*/
  if (zoom == 0)
      zoom = 1;

  if (zoom > 0)
  {
      *outsamp = (samp-1) / zoom + zdsdwsamp(unit, imp);
      *outline = (line-1) / zoom + zdsdwline(unit, imp);
  }
  else
  {
      *outsamp = (samp-1) * (-zoom) + zdsdwsamp(unit, imp);
      *outline = (line-1) * (-zoom) + zdsdwline(unit, imp);
  }

  if (*outsamp > nsMax) *outsamp -= nsMax;	/* Compensate for image	*/
  if (*outline > nlMax) *outline -= nlMax;	/* planes that wrap	*/
  return SUCCESS;
}

/************************************************************************/
/* Translates image plane coordinates to raw cursor (screen) coordinates.
 */
int CursIMP2Raw(env, imp, samp, line, outsamp, outline)
  VIDSEnvironment *env;		/* Environment for nlMax, nsMax		*/
  int imp;			/* image memory plane in question	*/
  int samp, line;		/* cursor position relative to imp	*/
  int *outsamp, *outline;	/* cursor position relative to screen	*/
{
  int unit;			/* VRDI unit number			*/
  int zoom;			/* HW zoom factor			*/
  int nlMax, nsMax;		/* max size of image memory planes	*/

  unit = env->devUnit;
  nlMax = env->nlMax;
  nsMax = env->nsMax;
  zoom = zdszoom(unit, imp);	/* Compensate for hardware zoom		*/
  if (zoom == 0)
      zoom = 1;

  if (zoom > 0)
  {
      *outsamp = (samp - zdsdwsamp(unit, imp)) * zoom + 1;
      *outline = (line - zdsdwline(unit, imp)) * zoom + 1;
  }
  else
  {
      *outsamp = (samp - zdsdwsamp(unit, imp)) / (-zoom) + 1;
      *outline = (line - zdsdwline(unit, imp)) / (-zoom) + 1;
  }

  while (*outsamp <= 0) *outsamp += nsMax;	/* Compensate for image */
  while (*outline <= 0) *outline += nlMax;	/* planes that wrap	*/
  while (*outsamp > nsMax) *outsamp -= nsMax;
  while (*outline > nlMax) *outline -= nlMax;
  return SUCCESS;
}

/******************************************************************************/
/* Translates image plane cursor coordinates to file coordinates.  This routine
 * does NOT check the coordinates to make sure they are inside the file.
 */
int CursIMP2File(plane, samp, line, outsamp, outline)
  PlaneInfo *plane;		/* Pointer to plane info struct		*/
  int samp, line;		/* cursor position rel to image plane	*/
  int *outsamp, *outline;	/* cursor position rel to file		*/
{
  int zoom;			/* Software zoom factor			*/

  zoom = plane->softZoom;
  if (zoom == 0)
      zoom = 1;

  *outsamp = samp - plane->accessWindow.ss;	/* rel. to access window */
  *outline = line - plane->accessWindow.sl;	/* zero-based now */

  if (zoom > 0)					/* Un-zoom (zero-based) */
  {
      *outsamp /= zoom;
      *outline /= zoom;
  }
  else
  {
      *outsamp *= (-zoom);
      *outline *= (-zoom);
  }

  *outsamp += plane->imageWindow.ss;		/* rel. to file */
  *outline += plane->imageWindow.sl;		/* one-based now */

  return SUCCESS;
}
/******************************************************************************/
/* Translates image file coordinates to image plane coordinates.  This routine
 * does NOT check the coordinates to make sure they are inside the file.
 */
int CursFile2IMP(env, imp, samp, line, outsamp, outline)
  VIDSEnvironment	*env;
  int imp;			/* image plane in question		*/
  int samp, line;		/* cursor position rel to image plane	*/
  int *outsamp, *outline;	/* cursor position rel to file		*/
{
  PlaneInfo *plane;		/* Pointer to plane info struct		*/
  int zoom;			/* Software zoom factor			*/

  plane = &env->planes[imp];
  zoom = plane->softZoom;
  if (zoom == 0)
      zoom = 1;

  *outsamp = samp - plane->imageWindow.ss;	/* rel. to access window */
  *outline = line - plane->imageWindow.sl;	/* zero-based now */

  if (zoom < 0)					/* Un-zoom (zero-based) */
  {
      *outsamp /= (-zoom);
      *outline /= (-zoom);
  }
  else
  {
      *outsamp *= zoom;
      *outline *= zoom;
  }

  *outsamp += plane->accessWindow.ss;		/* rel. to file */
  *outline += plane->accessWindow.sl;		/* one-based now */

  return SUCCESS;
}
/************************************************************************/
/* CursIMP2IMP will convert coordinates from one image plane to another,
 * so that the coords in the second plane align with the first on the 
 * screen.  The returned coordinates will always be within the plane.
 */
int CursIMP2IMP(env, imp1, imp2, x, y, outx, outy)
  VIDSEnvironment	*env;
  int			imp1,imp2;	/* in: in/out image planes	*/
  int			x,y;		/* in: point in imp1		*/
  int			*outx, *outy;	/* out: point in imp2		*/
{
  int unit;				/* VRDI unit number		*/
  int xraw, yraw;
  int xend, yend;

  unit = env->devUnit;

  xraw = x - zdsdwsamp(unit,imp1);
  if (xraw < 0)
    xraw += env->nsMax;
  xraw = Zoom(xraw, zdszoom(unit,imp1));
  if (xraw > VideoSamps(env))
    xraw -= Zoom(env->nsMax, zdszoom(unit,imp1));
  *outx = Zoom(xraw, -zdszoom(unit,imp2)) + zdsdwsamp(unit,imp2);
  while (*outx > env->nsMax)
    *outx -= env->nsMax;
  while (*outx <= 0)
    *outx += env->nsMax;

  yraw = y - zdsdwline(unit,imp1);
  if (yraw < 0)
    yraw += env->nlMax;
  yraw = Zoom(yraw, zdszoom(unit,imp1));
  if (yraw > VideoLines(env))
    yraw -= Zoom(env->nlMax, zdszoom(unit,imp1));
  *outy = Zoom(yraw, -zdszoom(unit,imp2)) + zdsdwline(unit,imp2);
  while (*outy > env->nlMax)
    *outy -= env->nlMax;
  while (*outy <= 0)
    *outy += env->nlMax;

  return SUCCESS;
}

/************************************************************************/
/* OffsetIMP2IMP will convert an offset from one image plane to another,
 * so that the offsets cover the same distance on the screen.  This differs
 * from CursIMP2IMP in that the pan is not taken into account, i.e. the
 * offsets are position-independent.
 */
int OffsetIMP2IMP(env, imp1, imp2, x, y, outx, outy)
  VIDSEnvironment	*env;
  int			imp1,imp2;	/* in: in/out image planes	*/
  int			x,y;		/* in: offset in imp1		*/
  int			*outx, *outy;	/* out: offset in imp2		*/
{
  int			unit;		/* VRDI unit number		*/

  unit = env->devUnit;

  *outx = Zoom(Zoom(x, zdszoom(unit,imp1)), -zdszoom(unit,imp2));
  *outy = Zoom(Zoom(y, zdszoom(unit,imp1)), -zdszoom(unit,imp2));

  return SUCCESS;
}

/************************************************************************/
/* Translates file coordinates directly to raw cursor (screen) coordinates.
 * This is used instead of the two-step File->IMP->Raw so we can do some
 * boundary checks to make sure the coordinate is on the display, and
 * so we can use a slightly different wrapping algorithm than IMP2Raw does.
 * Returns SUCCESS if the file coordinate is on the screen, or FAIL if not.
 */
int CursFile2Raw(env, imp, samp, line, outsamp, outline)
  VIDSEnvironment *env;		/* Environment for nlMax, nsMax		*/
  int imp;			/* image memory plane in question	*/
  int samp, line;		/* cursor position relative to imp	*/
  int *outsamp, *outline;	/* cursor position relative to screen	*/
{
  int unit;			/* VRDI unit number			*/
  int zoom;			/* HW zoom factor			*/
  int nlMax, nsMax;		/* max size of image memory planes	*/
  PlaneInfo *plane;

  unit = env->devUnit;
  nlMax = env->nlMax;
  nsMax = env->nsMax;

  plane = &env->planes[imp];
  if (samp < plane->imageWindow.ss) return FAIL;   /* Off displayed file area */
  if (line < plane->imageWindow.sl) return FAIL;
  if (samp >= plane->imageWindow.ss + plane->imageWindow.ns) return FAIL;
  if (line >= plane->imageWindow.sl + plane->imageWindow.nl) return FAIL;

  CursFile2IMP(env, imp, samp, line, &samp, &line);
  if (samp < 1) return FAIL;		/* Bad if off image plane */
  if (line < 1) return FAIL;
  if (samp > nsMax) return FAIL;
  if (line > nlMax) return FAIL;

  /* The following code duplicates CursIMP2Raw except for the image	*/
  /* plane wrapping at the bottom.  This method (adjusting by zoom *	*/
  /* nl or ns) allows us to reject coordinates that may be off-screen	*/
  /* do to a hardware zoom or pan.  Note: HW zooms are always positive!	*/

  zoom = zdszoom(unit, imp);	/* Compensate for hardware zoom		*/
  if (zoom == 0)
      zoom = 1;
  *outsamp = (samp - zdsdwsamp(unit, imp)) * zoom + 1;
  *outline = (line - zdsdwline(unit, imp)) * zoom + 1;

  if (*outsamp <= 0) *outsamp += zoom * nsMax;	/* Compensate for image */
  if (*outline <= 0) *outline += zoom * nlMax;	/* planes that wrap	*/
  if (*outsamp > nsMax) *outsamp -= zoom * nsMax;
  if (*outline > nlMax) *outline -= zoom * nlMax;

  if (*outsamp < 1) return FAIL;		/* Bad if off screen */
  if (*outline < 1) return FAIL;
  if (*outsamp > VideoSamps(env)) return FAIL;
  if (*outline > VideoLines(env)) return FAIL;

  return SUCCESS;
}
