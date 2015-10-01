#include "VIDSdefs.h"
#include <math.h>
#define PI 3.141592654

/* JPROFILE plots a profile, or cross-section, of the image along a
 * given path.
 */

#define SWAP(x,y,t)	(t = x, x = y, y = t)

#define NSLOTS		3
static GraphColor color_defaults[NSLOTS] = {Magenta, Yellow, Cyan};

#define MARKSTEP	20
#define ASPECT_RATIO	.5;	/* ratio of height to width of rotated prof */

typedef enum {Horiz, RTop, RBottom} Rotate;

int jprofile_do(env)

  VIDSEnvironment	*env;	/* The VIDS environment			*/
{
  int i,j,status;		/* temporary increment, status variables*/
  int planeList[MAXPLANES];	/* list of planes to use		*/
  int nPlanes;			/* number of image planes to use	*/
  Region *rgn;			/* region containing line segments	*/
  Region *disprgn;		/* region to display the planes in	*/
  GraphColor colors[MAXPLANES];	/* colors to display each plane with	*/
  Boolean stretch, scale;	/* match parameters of the same name	*/
  Rotate rotate;		/* value of Rotate parameter		*/
  int range[2];			/* min/max DN values; (-1,-1)=auto scale*/
  Point *points[MAXPLANES];	/* Point list for each plane		*/
  int npoints[MAXPLANES];	/* number of points for each plane	*/
  unsigned char *values[MAXPLANES]; /* pixel values for each plane	*/
  PlaneInfo *plane;
  int npts, n;
  Boolean plotted;
  int slot;
  int *lut;
  int dnmin, dnmax, xmax;	/* max across all planes		*/
  Rect plotarea;
  double angle, xform[2][2];
  int *CurrentLut();

  ShowGraphics(env);
  InvalHist(&env->planes[env->grafIMP]);

  status = GetJprofileParms(env, planeList, &nPlanes, &rgn, &disprgn,
		colors, &stretch, range, &scale, &rotate);
  if (status != SUCCESS)
    return status;

  NotifyUser(Inform, "", "Collecting profile data.");

  for (i=0; i<nPlanes; i++)
  {
    points[i] = NULL;
    values[i] = NULL;
  }

  dnmin = 255;
  dnmax = 0;
  xmax = 0;

  slot = 0;

  /* Collect the profile points and values */

  for (i = 0; i < nPlanes; i++)
  {
    lut = NULL;
    if (colors[i] == NoColor)
    {
      if (env->isColor)			/* check for red, green, or blue */
      {
        if (planeList[i] == env->redIMP)
          colors[i] = Red;
        else if (planeList[i] == env->greenIMP)
          colors[i] = Green;
        else if (planeList[i] == env->blueIMP)
          colors[i] = Blue;
      }
      else				/* bw mode */
      {
        if (planeList[i] == env->bwIMP)
          colors[i] = Red;
      }
    }

    if (colors[i] == NoColor)		/* still no color, so assign default */
    {
      colors[i] = color_defaults[slot];
      slot = (slot+1) % NSLOTS;
    }      

    if (stretch)
      lut = CurrentLut(env, planeList[i]);

    status = GetProfile(env, planeList[i], rgn, lut, stretch,
                      &points[i], &values[i], &npoints[i],
		      &dnmin, &dnmax, &xmax);
    if (status != SUCCESS && status != CLIPPED)
      goto cleanup;
  }

  /* Now plot the profile */

  if (xmax < 2)
    ABORT(FAIL, "Not enough points to plot a profile", "VIDS-TOOSMALL");

  if (scale != True)		/* don't scale the plot */
  {
    dnmin = env->profile.savednmin;
    dnmax = env->profile.savednmax;
    if (env->profile.savexmax != 0)	/* don't clobber if this is first one */
      xmax = env->profile.savexmax;
  }

  if (range[0] != -1)		/* user overrides auto range (and NOSCALE) */
    dnmin = range[0];
  if (range[1] != -1)
    dnmax = range[1];

  env->profile.savednmin = dnmin;
  env->profile.savednmax = dnmax;
  env->profile.savexmax = xmax;

  status = SetProfileXform(env, rgn, disprgn, &plotarea, rotate, &angle, xform);
  if (status != SUCCESS)
    goto cleanup;

  status = PlotProfileLabel(env, &plotarea, dnmin, dnmax, xmax,
			  rotate, angle, xform);
  if (status != SUCCESS)
    goto cleanup;

  for (i = 0; i < nPlanes; i++)
  {
    if (npoints[i] > 0)
    {
      if (rotate != Horiz)
        xmax = npoints[i];
      status = PlotProfileData(env, &plotarea, colors[i],
			     points[i], values[i], npoints[i],
			     dnmin, dnmax, xmax, planeList[i], xform);
      if (status != SUCCESS)
        goto cleanup;
    }
  }

  status = SUCCESS;

cleanup:
  for (i=0; i< nPlanes; i++)
  {
    if (points[i] != NULL)
      free(points[i]);
    if (values[i] != NULL)
      free(values[i]);
  }
  return status;
}


/************************************************************************/
/* GetProfile gets a profile for one image plane.  Memory is allocated to
 * hold the values, and pointers are returned.  Can return the status
 * CLIPPED, if the region is off the image plane.
 */

int GetProfile(env, imp, rgn, lut, stretch, points, values, npoints,
		dnmin, dnmax, xmax)
  VIDSEnvironment	*env;	/* The VIDS environment			*/
  int		imp;		/* plane number to get data from 	*/
  Region	*rgn;		/* polygon region defining profile path	*/
  int		*lut;		/* look-up table to use if stretch==True*/
  Boolean	stretch;	/* True iff lut should be used		*/
  Point		**points;	/* returns ptr to the points in this imp*/
  unsigned char	**values;	/* returns ptr to values for this imp	*/
  int		*npoints;	/* returns number of points in this imp	*/
  int		*dnmin;		/* in/out: minimum DN value		*/
  int		*dnmax;		/* in/out: maximum DN value		*/
  int		*xmax;		/* in/out: max x value (# of points)	*/
{
  int i, status;
  Region	*locRgn;
  int npts, n;
  Region *NextEmptyRegion();

  *npoints = 0;
  *points = NULL;
  *values = NULL;

  locRgn = NextEmptyRegion(env);
  if (locRgn == NULL)
    return FAIL;
  MarkRgnTemp(locRgn);

  status = RegionToLocal(env, imp, rgn, locRgn);
  if (status != SUCCESS)
    return status;

/* Take the list of line segments and get a list of all the points	*/
/* along the line.  The maximum number of points in a line segment is	*/
/* the horizontal span plus the vertical span of the segment, since any	*/
/* diagonal lines will 'cut the corner' and so have fewer points.	*/

  npts = 0;			/* max number of points along line */
  for (i=0; i<locRgn->nPoints-1; i++)
    npts += (abs(locRgn->pointList[i].h - locRgn->pointList[i+1].h) + 1) +
            (abs(locRgn->pointList[i].v - locRgn->pointList[i+1].v) + 1);

  *points = malloc(npts * sizeof(Point));
  if (*points == NULL)
    ABORT(FAIL, "Insufficient memory to gather profile points", "VIDS-INSUFMEM");

  npts = 0;
  for (i=0; i<locRgn->nPoints-1; i++)
  {
    GetPointsInLine(&locRgn->pointList[i], &locRgn->pointList[i+1],
		    &(*points)[npts], &n);
    npts += (n-1);		/* don't include vertex twice! */
  }
  npts++;			/* but include endpoint of entire poly */

  *values = malloc(npts * sizeof(**values));
  if (*values == NULL)
    ABORT(FAIL, "Insufficient memory to gather profile values", "VIDS-INSUFMEM");

  for (i=0; i<npts; i++)
  {
    status = zdipixelread(env->devUnit, imp,
			(*points)[i].h, (*points)[i].v, &(*values)[i]);
    if (status != SUCCESS)
      ABORT(FAIL, "Can't read pixel from screen", "VIDS-VRDIERR");
    if (stretch)
      (*values)[i] = lut[(*values)[i]];
    *dnmin = MIN(*dnmin, (int)(*values)[i]);
    *dnmax = MAX(*dnmax, (int)(*values)[i]);
  }

  *npoints = npts;
  *xmax = MAX(*xmax, npts);

  return SUCCESS;
}

/************************************************************************/
/* GetJprofileParms gathers all the parameters for JPROFILE and supplies
 * the defaults for disprgn and color.  If there are more planes given
 * than colors, then default colors are assigned to the extras.
 * This routine returns a temporary region that contains the line segment
 * endpoints.
 */

int GetJprofileParms(env, planeList, nPlanes, rgn, disprgn, colors,
		  stretch, range, scale, rotate)
  VIDSEnvironment	*env;	/* The VIDS environment			*/
  int		planeList[];	/* list of planes to use		*/
  int		*nPlanes;	/* number of image planes to use	*/
  Region	**rgn;		/* returns pointer to region w/line segs*/
  Region	**disprgn;	/* region to display the profile in	*/
  GraphColor	colors[];	/* colors used to display each plane	*/
  Boolean	*stretch;	/* True iff pipe profile through LUT	*/
  int		range[2];	/* DN range to use for min & max	*/
				/* (-1,-1) means use auto-scaling	*/
  Boolean	*scale;		/* True iff plot should be scaled to fit*/
  Rotate	*rotate;	/* Value of Rotate parameter		*/
{
  TAEVariable	*v;		/* temp VARIABLE pointer		*/
  int nRgns;			/* number of regions specified (1 or 0)	*/
  int nPoints;			/* number of points given		*/
  int i,j,status;		/* temporary increment, status variables*/
  int disprgncnt;		/* number of display regions given	*/
  int colorcnt;			/* count of colors given in parameter	*/
  GraphColor color;
  GraphColor StringToColor();
  Region *NameToRegion(), *PointsToRegion(), *GetRegion();
  TAEVariable *GetVariable();

  if (GetPlaneList(env, planeList, nPlanes, True) != SUCCESS)
    return FAIL;

  *rgn = NULL;

  v = GetVariable(env, "LAST");
  if (v == NULL) return FAIL;
  if (EQUAL(SVAL(*v, 0), "LAST"))
  {
    *rgn = NameToRegion(env, "PROFILE$TEMP", True);
    if (*rgn != NULL)
    {
      if ((*rgn)->type != Polygon)
        ABORT(FAIL, "Last path is bad: region must be a polygon", "VIDS-NOTPOLY");
      if (FrameLineSegs(env->devUnit, env->grafIMP, GC(System), GC(System),
			(*rgn)->pointList, (*rgn)->nPoints) != SUCCESS)
        ABORT(FAIL, "Unable to draw the profile path", "VIDS-VRDIERR");
    }
  }
  if (*rgn == NULL)		/* 'NEW or no last region */
  {
    v = GetVariable(env, "POINTS");
    if (v == NULL) return FAIL;
    nPoints = v->v_count;

    if (nPoints == 0)				/* if no points given,	*/
    {						/* use interactive curs	*/
      *rgn = GetRegion(env, env->grafIMP, "PROFILE$TEMP", Polygon);
      if (*rgn == NULL)
        return FAIL;
    }
    else
    {
      nPoints = nPoints / 2;
      if ((nPoints * 2) != v->v_count)
        ABORT(FAIL,"Points array must have an even number of integers",
              "VIDS-ODDPOINTS");
      if (nPoints < 2)
        ABORT(FAIL,"Not enough points to define a region; at least 2 required",
              "VIDS-INSUFPTS");

      *rgn = PointsToRegion(env, "PROFILE$TEMP", Polygon, v->v_cvp, nPoints);
      if (*rgn == NULL)
        return FAIL;

      if (FrameLineSegs(env->devUnit, env->grafIMP, GC(System), GC(System),
			(*rgn)->pointList, (*rgn)->nPoints) != SUCCESS)
        ABORT(FAIL, "Unable to draw the profile path", "VIDS-VRDIERR");
    }
  }

  if (GetRegionList(env, disprgn, &disprgncnt, "DISPRGN", 1) != SUCCESS)
    return FAIL;

  v = GetVariable(env, "COLOR");
  if (v == NULL) return FAIL;
  if (v->v_count <= 0)
    colorcnt = 0;
  else
    colorcnt = v->v_count;

  for (i=0; i<colorcnt; i++)		/* translate color strings to values */
    colors[i] = StringToColor(SVAL(*v, i));
  if (colorcnt == 0)
    color = NoColor;
  else
    color = colors[colorcnt-1];
  if (colorcnt < *nPlanes)
  {
    for (i=colorcnt; i<*nPlanes; i++)	/* fill in extra colors with	*/
      colors[i] = color;		/* last specified color		*/
  }

  v = GetVariable(env, "STRETCH");
  if (v == NULL) return FAIL;
  *stretch = False;
  if (EQUAL(SVAL(*v, 0), "STRETCHED"))
    *stretch = True;

  v = GetVariable(env, "RANGE");
  if (v == NULL) return FAIL;
  range[0] = -1;  range[1] = -1;
  if (v->v_count == 2)
  {
    range[0] = IVAL(*v, 0);
    range[1] = IVAL(*v, 1);
  }

  v = GetVariable(env, "SCALE");
  if (v == NULL) return FAIL;
  *scale = False;
  if (EQUAL(SVAL(*v, 0), "SCALE"))
    *scale = True;

  v = GetVariable(env, "ROTATE");
  if (v == NULL) return FAIL;
  *rotate = Horiz;
  if (EQUAL(SVAL(*v, 0), "RTOP"))
    *rotate = RTop;
  if (EQUAL(SVAL(*v, 0), "RBOTTOM"))
    *rotate = RBottom;
  if (*rotate != Horiz)
  {
    if ((*rgn)->nPoints != 2)
      ABORT(FAIL, "Can only rotate profiles of a single line", "VIDS-BADROT");
  }

  return SUCCESS;
}

/************************************************************************/
/* GetPointsInLine takes two endpoints for a line, and returns all the
 * points that make up the line, in effect rasterizing it.  The returned
 * points are put in 'points', and the number of points is returned in 'n'.
 * Both endpoints are included.  This routine uses a form of Bresenham's
 * line algorithm.
 */

int GetPointsInLine(startpt, endpt, points, n)
  Point *startpt;		/* starting endpoint of line		*/
  Point *endpt;			/* ending endpoint of line		*/
  Point *points;		/* where to put the returned points	*/
  int *n;			/* number of points returned		*/
{
  int x1, y1, x2, y2;
  int dx, dy, d, inc1, inc2;
  int x, y, start, end, inc;
  int negx, swapxy;
  int t, npix, i;

  x1 = startpt->h;
  y1 = startpt->v;
  x2 = endpt->h;
  y2 = endpt->v;

  dx = x2 - x1;
  dy = y2 - y1;
  negx = (dx > 0 && dy < 0) || (dx <= 0 && dy > 0);
  if (negx)
  {
    x1 = -x1;
    x2 = -x2;
  }

  dx = abs(dx);
  dy = abs(dy);
  swapxy = (dx < dy);
  if (swapxy)
  {
    SWAP(x1, y1, t);
    SWAP(x2, y2, t);
    SWAP(dx, dy, t);
  }

  d = 2*dy - dx;
  inc1 = 2*dy;
  inc2 = 2*(dy-dx);

  npix = abs(dx) + 1;
  if (x1 > x2)
  {
    x = x2;
    y = y2;
    start = npix-1;
    end = -1;		/* count down to 0 */
    inc = -1;
  }
  else
  {
    x = x1;
    y = y1;
    start = 0;
    end = npix;		/* count up to npix-1 */
    inc = 1;
  }

  for (i=start; i!=end; i+=inc)
  {
    points[i].h = x;
    points[i].v = y;
    if (swapxy)
      SWAP(points[i].h, points[i].v, t);
    if (negx)
      points[i].h = -points[i].h;
    x++;
    if (d < 0)
      d += inc1;
    else
    {
      y++;
      d += inc2;
    }
  }

  *n = npix;

  return SUCCESS;
}

/************************************************************************/
/* SetProfileXform sets the transformation matrix for profile rotations.
 * It also sets up plotarea.  If the profile is rotated, then plotarea
 * has the width and height of the plot area before rotation.  If the
 * profile is not rotated, then plotarea is derived from the dispregion.
 */
int SetProfileXform(env, rgn, disprgn, plotarea, rotate, angle, xform)
  VIDSEnvironment	*env;		/* The VIDS environment		*/
  Region		*rgn;		/* rgn profile is collected on	*/
  Region		*disprgn;	/* region to use for plotting	*/
  Rect			*plotarea;	/* area to plot in		*/
  Rotate		rotate;		/* whether or not to rotate plot*/
  double		*angle;		/* rotation angle		*/
  double		xform[2][2];	/* transformation matrix	*/
{
  int status;
  Point start, end;
  double rise, run;
  double xsize, dnsize;
  Rect		impRect;
  Region	*locRgn;
  Region *NameToRegion(), *NextEmptyRegion();

  if (rotate != Horiz)
  {				/* rotate, so first get angle of line */
    locRgn = NextEmptyRegion(env);
    if (locRgn == NULL)
      return FAIL;
    MarkRgnTemp(locRgn);

    status = RegionToLocal(env, env->grafIMP, rgn, locRgn);
    if (status != SUCCESS)
      return status;

    start.h = locRgn->pointList[0].h;		/* Must be polygon! */
    start.v = locRgn->pointList[0].v;
    end.h = locRgn->pointList[locRgn->nPoints-1].h;
    end.v = locRgn->pointList[locRgn->nPoints-1].v;

    DisposeRegion(env, locRgn);

    plotarea->left = start.h;
    plotarea->bottom = start.v;

    rise = end.v - start.v;
    run = end.h - start.h;

    *angle = - atan2(rise, run);		/* neg cuz y==0 is at top */

    xsize = sqrt((rise * rise) + (run * run));
    dnsize = xsize * ASPECT_RATIO;

    plotarea->right = plotarea->left + xsize;
    plotarea->top = plotarea->bottom - dnsize;

    xform[0][0] = cos(*angle);
    xform[0][1] = -sin(*angle);
    xform[1][0] = sin(*angle);
    xform[1][1] = cos(*angle);

    /* We need to flip the graph if they want it on the bottom.  We	*/
    /* also need to flip it if the points were specified right-to-left.	*/
    /* If both are true, we don't need to flip.				*/

    if ((rotate == RBottom && start.h <= end.h) ||
	(rotate	== RTop && start.h > end.h))
    {				/* to flip the graph, just negate orig y */
      xform[0][1] = -xform[0][1];
      xform[1][1] = -xform[1][1];
    }
  }
  else				/* not rotated */
  {
    if (disprgn == NULL)			/* last-ditch default */
      disprgn = NameToRegion(env, "FULLSCREEN", False);

    locRgn = NextEmptyRegion(env);
    if (locRgn == NULL)
      return FAIL;
    MarkRgnTemp(locRgn);

    status = RegionToLocal(env, env->grafIMP, disprgn, locRgn);
    if (status != SUCCESS)
      return status;

    SetRect(&impRect, 1, 1, env->nlMax, env->nsMax);

    BlockMove(&locRgn->bounds, plotarea, sizeof(Rect));
    DisposeRegion(env, locRgn);

    if (! RectInRect(plotarea, &impRect))
      ABORT(FAIL,
      "Display region crosses edge of graphics plane, so can't display profile",
      "VIDS-RGNXIMP");

    *angle = 0.0;
    xform[0][0] = 1.0;
    xform[0][1] = 0.0;
    xform[1][0] = 0.0;
    xform[1][1] = 1.0;
  }

  return SUCCESS;
}

/************************************************************************/
/* PlotProfileLabel plots the profile axis and label in the given region
 * on the graphics plane.  Plotrect on input contains the total plotting
 * rectangle; on output it is shrunk to be the plotting rect for the data
 * area only (if rotate==Horiz).  The VRDI status is returned.
 */

int PlotProfileLabel(env, plotarea, dnmin, dnmax, xmax, rotate, angle, xform)
  VIDSEnvironment	*env;		/* The VIDS environment		*/
  Rect			*plotarea;	/* bounding rect of data area	*/
  int			dnmin;		/* min DN value			*/
  int			dnmax;		/* max DN value			*/
  int			xmax;		/* max x value (# of points)	*/
  Rotate		rotate;		/* specifies rotation of plot	*/
  double		angle;		/* angle of rotation		*/
  double		xform[2][2];	/* rotation transformation	*/
{
  int		vidlines, vidsamps;
  int		unit, imp, status;
  int		h, v;
  int		xx, yy;
  int		tlen, len, temp, n;
  int		left, bottom;		/* of disprgn, not of plotarea	*/
  int		plotlbl;
  int		x[4], y[4];
  char		text[10];
  Point		lblsize;		/* size of a "0000" label	*/
  float		degangle;		/* angle in degrees (grrr)	*/
  Boolean	backwards;

  imp = env->grafIMP;
  unit = env->devUnit;

  status = SystemText(env);
  if (status != SUCCESS)
    return status;
  status = zdtcolor(GC(System), 0);
  if (status != SUCCESS)
    ABORT(FAIL, "Unable to set text color", "VIDS-VRDIERR");
  status = zdtmask(GC(System));
  if (status != SUCCESS)
    ABORT(FAIL, "Unable to set text mask", "VIDS-VRDIERR");
  degangle = angle * 180.0 / PI;	/* convert to degrees */
  while (degangle > 90.0)		/* force to (-90, +90) range */
    degangle -= 180.0;			/* so text isn't upside-down */
  while (degangle < -90.0)
    degangle += 180.0;
  status = RotateText(env, degangle);
  if (status != SUCCESS)
    ABORT(FAIL, "Unable to set text rotation", "VIDS-VRDIERR");

  lblsize.v = SystemTextHeight(env);	/* height in pixels to use for font */

  /* Now figure out how much room the labels will take */

  status = zdtlength(&lblsize.h, 4, "0000");	/* space that 4 digits take */
  if (status != SUCCESS)
    ABORT(FAIL, "Unable to get text length", "VIDS-VRDIERR");

  if (rotate == Horiz)
  {
    left = plotarea->left;
    bottom = plotarea->bottom;

    plotarea->left += lblsize.h + 1;	/* see if there's room for labels */
    plotarea->bottom -= lblsize.v + 1;

    if ((plotarea->left + 2*lblsize.h >= plotarea->right) ||
        (plotarea->bottom - 2*lblsize.v <= plotarea->top))
    {
      plotarea->left -= lblsize.h + 1;
      plotarea->bottom += lblsize.v + 1;
      NotifyUser(Inform, "",
                 "Profile display area is too small for the labels.");
    }
    else
    {
      /* First plot the DN axis values */

      v = plotarea->bottom;
      h = left;
      sprintf(text, "%3d", dnmin);
      len = strlen(text);
      zdttext(unit, imp, h, v, 1, len, text);

      v = plotarea->top + lblsize.v;
      sprintf(text, "%3d", dnmax);
      len = strlen(text);
      zdttext(unit, imp, h, v, 1, len, text);

      if (plotarea->bottom - 4*lblsize.v > plotarea->top)
      {
        v = (plotarea->bottom + plotarea->top) / 2 + (lblsize.v / 2);
	zdttext(unit, imp, h, v, 1, 2, "DN");
      }

      /* Now plot the linear (x) axis labels */

      v = bottom;
      h = plotarea->left;
      sprintf(text, "%d", 1);
      len = strlen(text);
      status = zdtlength(&temp, len, text);
      if (status != SUCCESS)
        ABORT(FAIL, "Unable to get text length", "VIDS-VRDIERR");
      tlen = temp;
      zdttext(unit, imp, h, v, 1, len, text);

      sprintf(text, "%d", xmax);
      len = strlen(text);
      status = zdtlength(&temp, len, text);
      if (status != SUCCESS)
        ABORT(FAIL, "Unable to get text length", "VIDS-VRDIERR");
      h = plotarea->right - temp;
      tlen += temp;
      zdttext(unit, imp, h, v, 1, len, text);

      strcpy(text, "SAMPLES");
      len = strlen(text);
      status = zdtlength(&temp, len, text);
      if (status != SUCCESS)
        ABORT(FAIL, "Unable to get text length", "VIDS-VRDIERR");
      if (plotarea->left + 2*tlen + temp < plotarea->right)
      {
        h = (plotarea->left + plotarea->right) / 2 - (temp / 2);
        zdttext(unit, imp, h, v, 1, len, text);
      }
    }

    /* Plot the axis lines */

    x[0] = plotarea->left;
    y[0] = plotarea->top;
    x[1] = plotarea->left;
    y[1] = plotarea->bottom;
    x[2] = plotarea->right;
    y[2] = plotarea->bottom;
    x[3] = plotarea->right;
    y[3] = plotarea->top;

    status = zdimpolyline(unit, imp, GC(System), GC(System), 4, x, y);
    if (status != SUCCESS)
      ABORT(FAIL, "Unable to draw axis lines", "VIDS-VRDIERR");
  }
  else					/* Rotated */
  {
    left = plotarea->left;
    bottom = plotarea->bottom;

    /* If the rotation pivot point is on the right, we still want the	*/
    /* labels to be on the left.  Since 'plotarea' coordinates are	*/
    /* defined *before* rotation and the pivot point is always on the	*/
    /* left in these coordinates, we must put the labels off the right	*/
    /* edge of the plot, where they will be rotated over to the left.	*/
    /* This all happens if the angle is not between -pi/2 and pi/2.	*/

    backwards = False;
    if (angle > (PI/2.0) || angle < (-PI/2.0))
      backwards = True;

    if (plotarea->bottom - 2*lblsize.v <= plotarea->top)
    {
      NotifyUser(Inform, "",
                 "Profile display area is too small for the labels.");
    }
    else
    {
      /* First plot the DN axis values */

      if (backwards)
        xx = (plotarea->right - plotarea->left) + lblsize.h;
      else
        xx = - lblsize.h;
      if (rotate == RTop)
        yy = 0;
      else
        yy = lblsize.v;
      h = left + (xform[0][0] * xx + xform[0][1] * yy);
      v = bottom - (xform[1][0] * xx + xform[1][1] * yy);
      sprintf(text, "%3d", dnmin);
      len = strlen(text);
      zdttext(unit, imp, h, v, 1, len, text);

      if (rotate == RTop)
        yy = (plotarea->bottom - plotarea->top) - lblsize.v;
      else
        yy = plotarea->bottom - plotarea->top;
      h = left + (xform[0][0] * xx + xform[0][1] * yy);
      v = bottom - (xform[1][0] * xx + xform[1][1] * yy);
      sprintf(text, "%3d", dnmax);
      len = strlen(text);
      zdttext(unit, imp, h, v, 1, len, text);
    }

    /* Now Plot the axis line */

    if (backwards)
      xx = (plotarea->right - plotarea->left);
    else
      xx = 0;
    yy = 0;
    x[0] = left + (xform[0][0] * xx + xform[0][1] * yy);
    y[0] = bottom - (xform[1][0] * xx + xform[1][1] * yy);
    yy = plotarea->bottom - plotarea->top;
    x[1] = left + (xform[0][0] * xx + xform[0][1] * yy);
    y[1] = bottom - (xform[1][0] * xx + xform[1][1] * yy);

    status = zdimpolyline(unit, imp, GC(System), GC(System), 2, x, y);
    if (status != SUCCESS)
      ABORT(FAIL, "Unable to draw axis lines", "VIDS-VRDIERR");
  }

  return SUCCESS;
}

/************************************************************************/
/* PlotProfileData plots the actual data for a profile in the given area
 * on the graphics plane, using the given pen.  The VRDI status is returned.
 */

int PlotProfileData(env, plotarea, color, points, values, npoints,
		    dnmin, dnmax, xmax, imageimp, xform)
  VIDSEnvironment	*env;		/* The VIDS environment		*/
  Rect			*plotarea;	/* area to plot in		*/
  GraphColor		color;		/* pen color to use		*/
  Point			*points;	/* points of the profile	*/
  unsigned char		*values;	/* value at each point		*/
  int			npoints;	/* number of points		*/
  int			dnmin;		/* min DN value to use		*/
  int			dnmax;		/* max DN value to use		*/
  int			xmax;		/* max x to use (# of points)	*/
  int			imageimp;	/* imp the image is on (for x's)*/
  double		xform[2][2];	/* transformation matrix	*/
{
  int unit;				/* device unit number		*/
  int imp;				/* image plane to use (graph)	*/
  int dnsize, xsize;		/* size of plot area in DN and x directions */
  double dnscale, xscale;
  int *x, *y;
  int dn, i, status;
  int left, bottom;
  Point pt;
  double xx, yy;

  unit = env->devUnit;
  imp = env->grafIMP;

  if (color == NoColor)		/* last-ditch default */
    color = White;

  left = plotarea->left;
  bottom = plotarea->bottom;

  dnsize = (plotarea->bottom - plotarea->top);
  xsize = (plotarea->right - plotarea->left);

  if (dnmin == dnmax)
    dnscale = 0.0;		/* shouldn't really happen */
  else
    dnscale = (double)dnsize / (double)(dnmax - dnmin);

  if (xmax == 0)
    xscale = 0.0;		/* shouldn't really happen */
  else
    xscale = (double)(xsize) / (double)(xmax-1);

  x = malloc(npoints * sizeof(*x));
  if (x == NULL)
    ABORT(FAIL, "Insufficient memory to plot profile", "VIDS-INSUFMEM");
  y = malloc(npoints * sizeof(*y));
  if (y == NULL)
  {
    free(x);
    ABORT(FAIL, "Insufficient memory to plot profile", "VIDS-INSUFMEM");
  }

  for (i=0; i<npoints; i++)		/* put the points in the array */
  {
    dn = values[i] - dnmin;
    if ((int)values[i] < dnmin)
      dn = 0;
    if ((int)values[i] > dnmax)
      dn = dnmax - dnmin;

    yy = dn * dnscale;
    xx = MIN(xmax, i) * xscale;

    x[i] = left + (xform[0][0] * xx + xform[0][1] * yy);
    y[i] = bottom - (xform[1][0] * xx + xform[1][1] * yy);
  }

  status = zdimpolyline(unit, imp, GC(color), GC(color), npoints, x, y);
  free(x);
  free(y);
  if (status != SUCCESS)
    ABORT(FAIL, "Unable to plot profile", "VIDS-VRDIERR");

  for (i = 0; i < npoints; i += MARKSTEP)
  {
    /* Mark point on the path */

    status = CursIMP2IMP(env, imageimp, imp, points[i].h, points[i].v,
			&pt.h, &pt.v);
    if (status != SUCCESS)
      return status;
    status = MarkPoint(unit, imp, GC(System), GC(System), &pt);
    if (status != SUCCESS)
      ABORT(FAIL, "Can't mark points on the plot", "VIDS-VRDIERR");

    /* Mark corresponding point on the plot */

    dn = values[i] - dnmin;
    if ((int)values[i] < dnmin)
      dn = 0;
    if ((int)values[i] > dnmax)
      dn = dnmax - dnmin;

    yy = dn * dnscale;
    xx = MIN(xmax, i) * xscale;

    pt.h = left + (xform[0][0] * xx + xform[0][1] * yy);
    pt.v = bottom - (xform[1][0] * xx + xform[1][1] * yy);

    status = MarkPoint(unit, imp, GC(System), GC(System), &pt);
    if (status != SUCCESS)
      ABORT(FAIL, "Can't mark points on the plot", "VIDS-VRDIERR");
  }

  return SUCCESS;
}
