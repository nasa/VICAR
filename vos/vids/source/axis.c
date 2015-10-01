#include "VIDSdefs.h"

#define AXISSIZE	256	/* range of the axis			*/

#define BIGTICSIZE	6	/* size of large tic mark		*/
#define SMALLTICSIZE	3	/* size of small tic mark		*/

#define MINXSIZE	32	/* smallest size to allow before	*/
#define MINYSIZE	32	/* removing labels			*/

/************************************************************************/
/* PlotAxis plots a pair of axes with tic marks, numeric labels, and an
 * optional label string.  On input, plotarea is the total size of the
 * display region.  On output, it is reduced to be the size of the data
 * area only (excluding the labeling).  Note that the axis lines themselves
 * are not drawn (it's up to the caller).  Horiz and vert are True if the
 * tic marks and numeric labels should be printed on each axis.  The range
 * of the axes is always (0,255).  The label for the axes is given in
 * xlabel and ylabel.  If the string is non-null, the label is printed,
 * regardless of whether horiz or vert are true.
 */

PlotAxis(env, plotarea, color, mask, horiz, vert, xlabel, ylabel)
   VIDSEnvironment	*env;		/* The VIDS environment		*/
  Rect			*plotarea;	/* area to plot the data in	*/
  GraphColor		color, mask;	/* color and mask to use	*/
  Boolean		horiz;		/* True if plot horiz. axis	*/
  Boolean		vert;		/* True if plot vert. axis	*/
  char			*xlabel;	/* text label for x axis	*/
  char			*ylabel;	/* text label for y axis	*/
{
  int		unit, imp;
  int		i, status;
  int		vidlines, vidsamps;
  Boolean	plotxtic, plotytic;
  int		minx, miny;
  int		left, bottom;
  int		len, slen;
  Point		start, end;
  Point		lblsize;		/* Size of a "0000" label	*/
  int		size, lblsiz;
  double	scale;
  int		numtics, ticspace;
  char		text[10];

  unit = env->devUnit;
  imp = env->grafIMP;

  /* Set up the text drawing routines */

  status = SystemText(env);
  if (status != SUCCESS)
    return status;
  status = zdtcolor(GC(color), 0);
  if (status != SUCCESS)
    ABORT(FAIL, "Unable to set text color", "VIDS-VRDIERR");
  status = zdtmask(GC(mask));
  if (status != SUCCESS)
    ABORT(FAIL, "Unable to set text mask", "VIDS-VRDIERR");

  lblsize.v = SystemTextHeight(env);	/* height in pixels to use for font */

  /* Now figure out how much room the labels will take */

  status = zdtlength(&lblsize.h, 4, "0000");	/* space that 4 digits take */
  if (status != SUCCESS)
    ABORT(FAIL, "Unable to get text length", "VIDS-VRDIERR");

  left = plotarea->left;
  bottom = plotarea->bottom;

  plotxtic = True;
  plotytic = True;

  if (vert)
    plotarea->left += lblsize.h + BIGTICSIZE + 2;
  if (horiz)
    plotarea->bottom -= lblsize.v + BIGTICSIZE + 2;
  if (xlabel != NULL && strlen(xlabel) != 0)
    plotarea->bottom -= lblsize.v + 1;
  if (ylabel != NULL && strlen(ylabel) != 0)
    plotarea->left += lblsize.v + 1;		/* y label is rotated 90 deg */

  minx = MINXSIZE;			/* minimum room needed for x numbers */
  if (horiz)
    minx = (3 * lblsize.h) / 2;
  miny = MINYSIZE;			/* min room needed for y-axis numbers */
  if (vert)
    miny = lblsize.v * 2;

  /* Check to see if X-axis tics and numbers fit */

  if (horiz == True && plotarea->bottom - miny <= plotarea->top)
  {
    plotxtic = False;
    NotifyUser(Inform, "", "Display area is too small for X-axis labels.");
    if (horiz)
      plotarea->bottom += lblsize.v + BIGTICSIZE + 2;
    if (xlabel != NULL && strlen(xlabel) != 0)
      plotarea->bottom += lblsize.v + 1;
  }

  /* Check to see if Y-axis tics and numbers fit */

  if (vert == True && plotarea->left + minx >= plotarea->right)
  {
    plotytic = False;
    NotifyUser(Inform, "", "Display area is too small for Y-axis labels.");
    if (vert)
      plotarea->left -= lblsize.h + BIGTICSIZE + 2;
    if (ylabel != NULL && strlen(ylabel) != 0)
      plotarea->left -= lblsize.v + 1;
  }

  /* Check to see if X-axis label fits, and plot it if it does */
  /* X-axis label can extend beyond Y-axis; Y label can't go beyond X-axis */

  if (xlabel != NULL && strlen(xlabel) != 0 && plotxtic == True)
  {				/* if no room for tics, no room for label */
    slen = strlen(xlabel);
    status = zdtlength(&len, slen, xlabel);
    if (status != SUCCESS)
      ABORT(FAIL, "Unable to get text length", "VIDS-VRDIERR");

    if (left + len >= plotarea->right)		/* doesn't fit */
    {
      plotarea->bottom += lblsize.v + 1;
      NotifyUser(Inform, "", "Display area is too small for X-axis labels.");
    }
    else
    {
      start.v = bottom;				/* center it */
      start.h = left + (plotarea->right - left - len) / 2;
      zdttext(unit, imp, start.h, start.v, 1, slen, xlabel);
    }
  }

  /* Check to see if Y-axis label fits, and plot it if it does */

  if (ylabel != NULL && strlen(ylabel) != 0 && plotytic == True)
  {				/* if no room for tics, no room for label */
    slen = strlen(ylabel);
    status = zdtlength(&len, slen, ylabel);
    if (status != SUCCESS)
      ABORT(FAIL, "Unable to get text length", "VIDS-VRDIERR");

    if (plotarea->bottom - len <= plotarea->top)	/* doesn't fit */
    {
      plotarea->left -= lblsize.v + 1;
      NotifyUser(Inform, "", "Display area is too small for Y-axis labels.");
    }
    else
    {
      status = RotateText(env, 90.0);
      if (status != SUCCESS)
        ABORT(FAIL, "Unable to set text rotation for Y-axis label", "VIDS-VRDIERR");
      start.h = left + lblsize.v;			/* center it */
      start.v = plotarea->bottom - (plotarea->bottom - plotarea->top - len) / 2;
      zdttext(unit, imp, start.h, start.v, 1, slen, ylabel);
      RotateText(env, 0.0);
    }
  }

  /* Now plot the X-axis tic marks and numeric labels */

  if (horiz==True && plotxtic==True && lblsize.h != 0)
  {
    size = plotarea->right - plotarea->left;
    lblsiz = (lblsize.h * 3) / 2;

    scale = (double) size / (double)(AXISSIZE-1);
    numtics = size / lblsiz;	/* divide by 0 prevented by 'if' above */

    if (numtics >= 128)		/* force tic marks to powers of 2 */
      ticspace = 2;
    else if (numtics >= 64)
      ticspace = 4;
    else if (numtics >= 32)
      ticspace = 8;
    else if (numtics >= 16)
      ticspace = 16;
    else if (numtics >= 8)
      ticspace = 32;
    else if (numtics >= 4)
      ticspace = 64;
    else if (numtics >= 2)
      ticspace = 128;
    else
      ticspace = 256;

    for (i=0; i<=AXISSIZE; i += ticspace)
    {
      start.h = plotarea->left + (int)(i * scale);		/* big tic */
      if (i == AXISSIZE)
        start.h = plotarea->left + (int)((i-1) * scale);
      end.h = start.h;
      start.v = plotarea->bottom + BIGTICSIZE;
      end.v = plotarea->bottom + 1;
      status = DrawLine(unit, imp, GC(mask), GC(color), &start, &end);
      if (status != SUCCESS)
        ABORT(FAIL, "Unable to draw tic mark", "VIDS-VRDIERR");

      if (i != AXISSIZE)
      {								/* small tic */
        start.h = plotarea->left + (int)((i+(ticspace/2))*scale);
        end.h = start.h;
        start.v = plotarea->bottom + SMALLTICSIZE;
        status = DrawLine(unit, imp, GC(mask), GC(color), &start, &end);
        if (status != SUCCESS)
          ABORT(FAIL, "Unable to draw tic mark", "VIDS-VRDIERR");
      }

      start.h = plotarea->left + (int)(i * scale);		/* numbers */
      if (i == AXISSIZE)
        start.h = plotarea->left + (int)((i-1) * scale);
      start.v = plotarea->bottom + lblsize.v + BIGTICSIZE + 1;
      sprintf(text, "%d", i);
      if (i == AXISSIZE)		/* ends with 255, not 256! */
        sprintf(text, "%d", i-1);
      slen = strlen(text);
      status = zdtlength(&len, slen, text);
      if (status != SUCCESS)
        ABORT(FAIL, "Unable to get text length", "VIDS-VRDIERR");
      if (i == AXISSIZE)
        start.h -= len;
      else if (i != 0)
        start.h -= (len/2);
      zdttext(unit, imp, start.h, start.v, 1, slen, text);
    }
  }

  /* Now plot the Y-axis tic marks and numeric labels */

  if (vert==True && plotytic==True)
  {
    size = plotarea->bottom - plotarea->top;
    lblsiz = lblsize.v * 2;

    scale = (double) size / (double)(AXISSIZE-1);
    numtics = size / lblsiz;

    if (numtics >= 128)		/* force tic marks to powers of 2 */
      ticspace = 2;
    else if (numtics >= 64)
      ticspace = 4;
    else if (numtics >= 32)
      ticspace = 8;
    else if (numtics >= 16)
      ticspace = 16;
    else if (numtics >= 8)
      ticspace = 32;
    else if (numtics >= 4)
      ticspace = 64;
    else if (numtics >= 2)
      ticspace = 128;
    else
      ticspace = 256;

    for (i=0; i<=AXISSIZE; i += ticspace)
    {
      start.v = plotarea->bottom - (int)(i * scale);		/* big tic */
      if (i == AXISSIZE)
        start.v = plotarea->bottom - (int)((i-1) * scale);
      end.v = start.v;
      start.h = plotarea->left - BIGTICSIZE;
      end.h = plotarea->left - 1;
      status = DrawLine(unit, imp, GC(mask), GC(color), &start, &end);
      if (status != SUCCESS)
        ABORT(FAIL, "Unable to draw tic mark", "VIDS-VRDIERR");

      if (i != AXISSIZE)					/* small tic */
      {
        start.v = plotarea->bottom - (int)((i+(ticspace/2))*scale);
        end.v = start.v;
        start.h = plotarea->left - SMALLTICSIZE;
        status = DrawLine(unit, imp, GC(mask), GC(color), &start, &end);
        if (status != SUCCESS)
          ABORT(FAIL, "Unable to draw tic mark", "VIDS-VRDIERR");
      }

      start.v = plotarea->bottom - (int)(i * scale);		/* numbers */
      if (i == AXISSIZE)
        start.v = plotarea->bottom - (int)((i-1) * scale);
      start.h = plotarea->left - BIGTICSIZE - 1 - lblsize.h;
      if (i == AXISSIZE)
        start.v += lblsize.v;
      else if (i != 0)
        start.v += (lblsize.v/2);
      sprintf(text, "%3d", i);
      if (i == AXISSIZE)		/* ends with 255, not 256! */
        sprintf(text, "%3d", i-1);
      zdttext(unit, imp, start.h, start.v, 1, 3, text);
    }
  }

  return SUCCESS;
}
