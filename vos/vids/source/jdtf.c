#include "VIDSdefs.h"

/* JDTF displays the transfer function (look-up tables) on the graphics plane
 * and optionally edits them.  JDTF can also be accessed by JSTRETCH-SHOW, and
 * JDTF 'EDIT is also called JSTRETCH-EDIT.
 */

#define NSLOTS		3
static GraphColor color_defaults[NSLOTS] = {Magenta, Yellow, Cyan};

#define LUTSIZE		256

int jdtf_do(env)

  VIDSEnvironment	*env;	/* The VIDS environment			*/
{
  int i,j,status;		/* temporary increment, status variables*/
  int planeList[MAXPLANES];	/* list of planes to use		*/
  int nPlanes;			/* number of image planes to use	*/
  Region *disprgn;		/* region to display the planes in	*/
  GraphColor colors[MAXPLANES];	/* colors to display each plane with	*/
  Boolean label;		/* True iff plot axis labels		*/
  Boolean edit;			/* True iff interactively edit LUT	*/
  int slot;
  int *lut;
  Rect plotarea;
  double xscale, yscale;
  int savelut[256];
  Boolean done, firstpt;
  Point start, end, pt1, pt2;
  int *CurrentLut(), *NewLut();

  ShowGraphics(env);
  InvalHist(&env->planes[env->grafIMP]);

  status = GetJdtfParms(env, planeList, &nPlanes, &disprgn, colors,
		      &label, &edit);
  if (status != SUCCESS)
    return status;

  slot = 0;

  /* Display the DTF for each plane */

  status = PlotDTFLabel(env, disprgn, &plotarea, label);
  if (status != SUCCESS)
    return status;

  for (i = 0; i < nPlanes; i++)
  {
    if (colors[i] == NoColor)		/* Assign a default color if needed */
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

    lut = CurrentLut(env, planeList[i]);	/* get the LUT to display */

    status = PlotDTFData(env, &plotarea, lut, colors[i], colors[i], 0, LUTSIZE-1);
    if (status != SUCCESS)
      return status;
  }

  if (edit)
  {
    GetDTFScale(&plotarea, &xscale, &yscale);

    for (i = 0; i < nPlanes; i++)
    {
      status = FrameRect(env->devUnit, env->grafIMP,	/* color border to */
		       0xFF, GC(colors[i]), &plotarea);	/* show current color */
      if (status != SUCCESS)
        ABORT(FAIL, "Can't draw border", "VIDS-VRDIERR");

      lut = NewLut(env, planeList[i]);   /* get the LUT to edit */
      for (j=0; j<LUTSIZE; j++)
        savelut[j] = lut[j];

      done = False;
      firstpt = True;

      NotifyUser(Inform, "", "Now editing the look-up table for plane %d.",
		 planeList[i]);

      while (done != True)
      {

        if (firstpt)			/* first point of two for line seg */
        {
          firstpt = False;
          status = GetPointButton(env, env->grafIMP, &start);
          end.h = start.h;		/* make it look like a very thin line */
          end.v = start.v;
        }
        else				/* second point for line seg */
        {
          firstpt = True;
          status = RubberLine(env, env->grafIMP, Limit, &start, &end, &plotarea);
        }

        if (status == REJECT)		/* accept this LUT as is and move on */
        {
          NotifyUser(Inform, "", "Changes to LUT for plane %d accepted",
			planeList[i]);
          done = True;
        }
        else if (status != SUCCESS)	/* abort the edit of this LUT */
        {
          status = PlotDTFData(env, &plotarea, lut, colors[i], NoColor,
			     0, LUTSIZE-1);
          if (status != SUCCESS)			/* blank old LUT */
            return status;

          for (j=0; j<LUTSIZE; j++)		/* restore saved LUT */
            lut[j] = savelut[j];

          SendLuts(env);

          NotifyUser(Inform, "", "Changes to LUT for plane %d ignored",
			planeList[i]);

          status = PlotDTFData(env, &plotarea, lut, colors[i], colors[i],
			     0, LUTSIZE-1);
          if (status != SUCCESS)
            return status;

          done = True;			/* and go to next LUT */
        }
        else				/* Accept this point */
        {
          PointToLut(&plotarea, xscale, yscale, &start, &pt1);
          PointToLut(&plotarea, xscale, yscale, &end, &pt2);

          status = PlotDTFData(env, &plotarea, lut, colors[i], NoColor,
			     pt1.h, pt2.h);	/* blank old LUT section */
          if (status != SUCCESS)
            return status;

          LinearLutRange(lut, pt1.h, pt2.h, pt1.v, pt2.v);

          SendLuts(env);			/* draw new LUT section */
          status = PlotDTFData(env, &plotarea, lut, colors[i], colors[i],
			     pt1.h, pt2.h);
          if (status != SUCCESS)
            return status;
        }
      }
    }
    status = FrameRect(env->devUnit, env->grafIMP,
		     0xFF, GC(System), &plotarea);	/* reset border color */
    if (status != SUCCESS)
      ABORT(FAIL, "Can't draw border", "VIDS-VRDIERR");
  }

  return SUCCESS;
}


/************************************************************************/
/* GetJdtfParms gathers all the parameters for JDTF and supplies
 * the defaults for disprgn and color.  If there are more planes given
 * than colors, then default colors are assigned to the extras.
 */

int GetJdtfParms(env, planeList, nPlanes, disprgn, colors, label, edit)
  VIDSEnvironment	*env;	/* The VIDS environment			*/
  int		planeList[];	/* list of planes to use		*/
  int		*nPlanes;	/* number of image planes to use	*/
  Region	**disprgn;	/* region to display the profile in	*/
  GraphColor	colors[];	/* colors used to display each plane	*/
  Boolean	*label;		/* True iff plot axis labels		*/
  Boolean	*edit;		/* True iff interactively edit the LUT	*/
{
  TAEVariable	*v;		/* temp VARIABLE pointer		*/
  int i,j,status;		/* temporary increment, status variables*/
  int disprgncnt;		/* number of display regions given	*/
  int colorcnt;			/* count of colors given in parameter	*/
  GraphColor color;
  GraphColor StringToColor();
  TAEVariable *GetVariable();

  if (GetPlaneList(env, planeList, nPlanes, True) != SUCCESS)
    return FAIL;

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

  v = GetVariable(env, "LABEL");
  if (v == NULL) return FAIL;
  *label = False;
  if (EQUAL(SVAL(*v, 0), "LABEL"))
    *label = True;

  *edit = False;
  v = GetVariable(env, "_PROC");
  if (v == NULL) return FAIL;
  if (EQUAL(SVAL(*v, 0), "JDTF"))	/* JDTF command, check for 'EDIT */
  {
    v = GetVariable(env, "EDIT");
    if (v == NULL) return FAIL;
    if (EQUAL(SVAL(*v, 0), "EDIT"))
      *edit = True;
  }
  else					/* JSTRETCH command, check for -EDIT */
  {
    v = GetVariable(env, "_SUBCMD");
    if (v == NULL) return FAIL;
    if (EQUAL(SVAL(*v, 0), "EDIT"))
      *edit = True;
  }

  return SUCCESS;
}

/************************************************************************/
/* PointToLut takes a point on the graphics plane, and the scaling factors,
 * and figures out where the point is in LUT space; i.e. it converts the
 * values to the 0..255 range.
 */

int PointToLut(plotarea, xscale, yscale, in, out)
  Rect		*plotarea;			/* the plotting area	*/
  double	xscale, yscale;			/* scaling factors	*/
  Point		*in;				/* the pt in graph coord*/
  Point		*out;				/* the pt in LUT coords	*/
{
  if (in->h < plotarea->left)
    in->h = plotarea->left;
  if (in->h > plotarea->right)
    in->h = plotarea->right;
  if (in->v < plotarea->top)
    in->v = plotarea->top;
  if (in->v > plotarea->bottom)
    in->v = plotarea->bottom;

  out->h = (double)(in->h - plotarea->left) / xscale + 0.5;
  out->v = (double)(plotarea->bottom - in->v) / yscale + 0.5;

  return SUCCESS;
}

/************************************************************************/
/* PlotDTFLabel plots the axis and label for the DTF plot in the given
 * region on the graphics plane.  Plotrect is set to the plotting area for
 * the data only; i.e. not including the label area.  The VRDI status is
 * returned.
 */

int PlotDTFLabel(env, disprgn, plotarea, label)
  VIDSEnvironment	*env;		/* The VIDS environment		*/
  Region		*disprgn;	/* display region		*/
  Rect			*plotarea;	/* out: bounding rect of data	*/
  Boolean		label;		/* True iff plot label		*/
{
  int		i, status;
  int		outsize, insize;
  double	outscale, inscale;
  int		numtics, ticspace;
  GraphColor	color;
  Point		start, end;
  Rect		impRect;
  Region	*locRgn;
  Region *NameToRegion(), *NextEmptyRegion();

  color = System;

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
    "Display region crosses edge of graphics plane, so can't display LUT",
    "VIDS-RGNXIMP");

  zddbatch(env->devUnit, True);

  if (label)
  {
    status = PlotAxis(env, plotarea, color, color, True, True,
		    "DN (IN)", "DN (OUT)");
    if (status != SUCCESS) {
      zddbatch(env->devUnit, False);
      return status;
    }
  }

  /* Now draw the axis */

  status = FrameRect(env->devUnit, env->grafIMP, GC(color), GC(color), plotarea);
  zddbatch(env->devUnit, False);
  if (status != SUCCESS)
    ABORT(FAIL, "Can't draw border", "VIDS-VRDIERR");

  return SUCCESS;
}

/************************************************************************/
/* GetDTFScale calculates the scaling factors for this plot.
 */

int GetDTFScale(plotarea, xscale, yscale)
  Rect			*plotarea;	/* area to plot in		*/
  double		*xscale;	/* out: x scaling factor	*/
  double		*yscale;	/* out: y scaling factor	*/
{
  int ysize, xsize;

  ysize = (plotarea->bottom - plotarea->top);
  xsize = (plotarea->right - plotarea->left);

  *yscale = (double)ysize / (double)(LUTSIZE-1);
  *xscale = (double)xsize / (double)(LUTSIZE-1);

  return SUCCESS;
}

/************************************************************************/
/* PlotDTFData plots the actual data for a dtf in the given area
 * on the graphics plane, using the given pen.  Only the data in the
 * range (start-1, stop+1) is plotted.  The VRDI status is returned.
 */

int PlotDTFData(env, plotarea, lut, mask, color, start, stop)
  VIDSEnvironment	*env;		/* The VIDS environment		*/
  Rect			*plotarea;	/* area to plot in		*/
  int			*lut;		/* the LUT to plot		*/
  GraphColor		mask;		/* mask to use for pen		*/
  GraphColor		color;		/* pen color to use		*/
{
  int unit;				/* device unit number		*/
  int imp;				/* image plane to use (graph)	*/
  int size, temp;
  double inscale, outscale;
  int x[LUTSIZE], y[LUTSIZE];
  int i, status;

  if (start > stop)		/* make sure start < stop */
  {
    temp = start;
    start = stop;
    stop = temp;
  }

  start--;		/* plot points on either side too so lines connect */
  if (start < 0)
    start = 0;
  stop++;
  if (stop > LUTSIZE-1)
    stop = LUTSIZE-1;
  size = (stop - start) + 1;

  unit = env->devUnit;
  imp = env->grafIMP;

  GetDTFScale(plotarea, &inscale, &outscale);

  for (i=start; i<=stop; i++)		/* plot the points */
  {
    x[i] = plotarea->left + (i * inscale);
    y[i] = plotarea->bottom - (lut[i] * outscale);
  }

  status=zdimpolyline(unit,imp,GC(mask),GC(color),size,&x[start],&y[start]);
  if (status != SUCCESS)
    ABORT(FAIL, "Unable to plot transfer function", "VIDS-VRDIERR");

  return SUCCESS;
}
