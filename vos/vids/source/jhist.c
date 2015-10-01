#include "VIDSdefs.h"

/* JHIST will collect and display the histogram for a region.
 */

/* Defaults for disprgn and color of image planes (r,g,b,bw)	*/
/* for both unstretched and stretched.				*/
static char *drimage_defaults[2][4] = {{"HIST1","HIST2","HIST3","HIST1"},
				       {"HIST4","HIST5","HIST6","HIST4"}};
static char cimage_defaults[2][4] = {{Red, Green, Blue, Red},
				     {Red, Green, Blue, Red}};

/* Defaults for disprgn and color for non-image planes		*/
#define NSLOTS	3		/* # of display slots for non-image planes */
static char *drother_defaults[NSLOTS] = {"HIST4", "HIST5", "HIST6"};
static int cother_defaults[NSLOTS] = {Magenta, Yellow, Cyan};

int jhist_do(env)

  VIDSEnvironment	*env;	/* The VIDS environment			*/
{
  int i,status;			/* temporary increment, status variables*/
  int planeList[MAXPLANES];	/* list of planes to use		*/
  int nPlanes;			/* number of image planes to use	*/
  Region *rgn;			/* region to collect hist over		*/
  Region *disprgn[MAXPLANES];	/* regions to display each plane in	*/
  GraphColor colors[MAXPLANES];	/* colors to display each plane with	*/
  Boolean label, vertical, stretch;
  int spike;			/* number of spikes to ignore		*/
  Boolean plotted;
  int slot;
  int stridx;			/* index into defaults tbl: 0=nostretch,*/
				/*			    1=stretch	*/
  Region *dr;
  GraphColor c;
  Region *NameToRegion();
  int *CurrentLut();

  status = GetJhistParms(env, planeList, &nPlanes, &rgn, disprgn, colors,
		&label, &vertical, &spike, &stretch);
  if (status != SUCCESS)
    return status;

  ShowGraphics(env);
  InvalHist(&env->planes[env->grafIMP]);

  slot = 0;

  stridx = 0;
  if (stretch)
    stridx = 1;

  for (i = 0; i < nPlanes; i++)
  {
    plotted = False;

    dr = disprgn[i];
    c = colors[i];

    if (env->isColor)			/* check for red, green, or blue */
    {
      if (planeList[i] == env->redIMP)
      {
        if (disprgn[i] == NULL)
          dr = NameToRegion(env, drimage_defaults[stridx][0], False);
        if (colors[i] == NoColor)
          c = cimage_defaults[stridx][0];

        status = OneHist(env, planeList[i], rgn, CurrentLut(env, planeList[i]),
		       dr, c, label, vertical, spike, stretch);
        plotted = True;
        if (status != SUCCESS && status != CLIPPED)
	  return status;
      }

      if (planeList[i] == env->greenIMP)
      {
        if (disprgn[i] == NULL)
          dr = NameToRegion(env, drimage_defaults[stridx][1], False);
        if (colors[i] == NoColor)
          c = cimage_defaults[stridx][1];

        status = OneHist(env, planeList[i], rgn, CurrentLut(env, planeList[i]),
		       dr, c, label, vertical, spike, stretch);
        plotted = True;
        if (status != SUCCESS && status != CLIPPED)
	  return status;
      }

      if (planeList[i] == env->blueIMP)
      {
        if (disprgn[i] == NULL)
          dr = NameToRegion(env, drimage_defaults[stridx][2], False);
        if (colors[i] == NoColor)
          c = cimage_defaults[stridx][2];

        status = OneHist(env, planeList[i], rgn, CurrentLut(env, planeList[i]),
		       dr, c, label, vertical, spike, stretch);
        plotted = True;
        if (status != SUCCESS && status != CLIPPED)
	  return status;
      }
    }
    else			/* bw mode */
    {
      if (planeList[i] == env->bwIMP)
      {
        if (disprgn[i] == NULL)
          dr = NameToRegion(env, drimage_defaults[stridx][3], False);
        if (colors[i] == NoColor)
          c = cimage_defaults[stridx][3];

        status = OneHist(env, planeList[i], rgn, CurrentLut(env, planeList[i]),
		       dr, c, label, vertical, spike, stretch);
        plotted = True;
        if (status != SUCCESS && status != CLIPPED)
	  return status;
      }
    }

    if (plotted != True)     /* not R, G, or B, so figure out where to put it */
    {
      if (disprgn[i] == NULL)
        dr = NameToRegion(env, drother_defaults[slot], False);
      if (colors[i] == NoColor)
        c = cother_defaults[slot];
      slot = (slot+1) % NSLOTS;

      status = OneHist(env, planeList[i], rgn, NULL,
		     dr, c, label, vertical, spike, stretch);
      plotted = True;
      if (status != SUCCESS && status != CLIPPED);
        return status;
    }
  }

  InvalHist(&env->planes[env->grafIMP]);
  return SUCCESS;
}


/************************************************************************/
/* OneHist does a histogram for one image plane.  Can return the status
 * CLIPPED, if the region is off the image plane.
 */

int OneHist(env, planenum, rgn, LUT, disprgn, color, label, vertical,
	     spike, stretch)
  VIDSEnvironment	*env;	/* The VIDS environment			*/
  int		planenum;	/* plane number to get data from 	*/
  Region	*rgn;		/* region to collect hist over		*/
  int		LUT[256];	/* look-up table to use			*/
  Region	*disprgn;	/* region to display the plane in	*/
  GraphColor	color;		/* color to display the plane with	*/
  Boolean	label;		/* True iff the DN label is printed	*/
  Boolean	vertical;	/* True iff vertical orientation	*/
  int		spike;		/* number of spikes to ignore		*/
  Boolean	stretch;	/* True iff pipe hist through LUT	*/
{
  int i,status;			/* temporary increment, status variables*/
  PlaneInfo *plane;
  double mean, sigma;
  long hist[HSIZE];
  Region *NameToRegion();
  int seed;
  char msg[80];

  if (disprgn == NULL)
    disprgn = NameToRegion(env, "FULLSCREEN", False);
  if (color == NoColor)
    color = White;

  plane = &env->planes[planenum];

  if (rgn == NULL)
    seed = 0;
  else
    seed = rgn->seed;

  if (plane->hist.array != NULL && plane->hist.rgnseed != seed)
  {
    InvalHist(plane);
  }

  if (plane->hist.array == NULL)
  {					/* Must collect histogram */

    plane->hist.array = malloc(HSIZE * sizeof(long));
    if (plane->hist.array == NULL)
      ABORT(FAIL, "Insufficient memory to collect histogram", "VIDS-INSUFMEM");

    status = CollectHist(env, planenum, rgn, plane->hist.array);
    if (status != SUCCESS)
      return status;

    plane->hist.rgnseed = seed;
  }

  if (stretch == True && LUT != NULL)
  {
    for (i=0; i<HSIZE; i++)
      hist[i] = 0;
    for (i=0; i<HSIZE; i++)		/* pipe the hist through the LUT */
      hist[LUT[i]] += plane->hist.array[i];
  }
  else
  {					/* don't go through the LUT */
    for (i=0; i<HSIZE; i++)
      hist[i] = plane->hist.array[i];
  }

  GatherHistStats(hist, &mean, &sigma);
  sprintf(msg, "Plane %d Mean = %f\tSigma = %f", planenum, mean, sigma);
  NotifyUser(Inform, "", msg);		/* can't use doubles in NotifyUser! */

  status = PlotHist(env, hist, disprgn, color, label, vertical, spike,
		  mean, sigma);
  if (status != SUCCESS)
    return status;

  return SUCCESS;
}

/************************************************************************/
/* GetJhistParms gathers all the parameters for JHIST.  Any unspecified
 * disprgn's have a NULL value.  Any unspecified colors are filled in with
 * the last color given, or NoColor if none are given.  This allows the
 * user to say 'color=magenta' and have all the histograms appear in magenta.
 */

int GetJhistParms(env, planeList, nPlanes, rgn, disprgn, colors,
		  label, vertical, spike, stretch)
  VIDSEnvironment	*env;	/* The VIDS environment			*/
  int		planeList[];	/* list of planes to use		*/
  int		*nPlanes;	/* number of image planes to use	*/
  Region	**rgn;		/* regions to collect hist over		*/
  Region	*disprgn[];	/* region to display each plane in	*/
  GraphColor	colors[];	/* colors used to display each plane	*/
  Boolean	*label;		/* True iff label is to be printed	*/
  Boolean	*vertical;	/* True iff use vertical orientation	*/
  int		*spike;		/* number of histogram spikes to ignore	*/
  Boolean	*stretch;	/* True iff pipe hist through LUT	*/
{
  TAEVariable	*v;		/* temp VARIABLE pointer		*/
  int nRgns;			/* number of regions specified (1 or 0)	*/
  int i,j,status;		/* temporary increment, status variables*/
  int disprgncnt;		/* number of display regions given	*/
  int colorcnt;			/* count of colors given in parameter	*/
  GraphColor color;
  GraphColor StringToColor();
  TAEVariable	*GetVariable();

  for (i=0; i<MAXPLANES; i++)
  {
    disprgn[i] = NULL;
    colors[i] = NoColor;
  }

  if (GetPlaneList(env, planeList, nPlanes, True) != SUCCESS)
    return FAIL;
  if (GetRegionList(env, rgn, &nRgns, "REGION", 1) != SUCCESS)
    return FAIL;
  if (nRgns == 0)
    *rgn = NULL;		/* region is defaulted */
  if (GetRegionList(env, disprgn, &disprgncnt, "DISPRGN", MAXPLANES)
		!= SUCCESS)
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

  v = GetVariable(env, "ORIENT");
  if (v == NULL) return FAIL;
  *vertical = False;
  if (EQUAL(SVAL(*v, 0), "VERTICAL"))
    *vertical = True;

  v = GetVariable(env, "LABEL");
  if (v == NULL) return FAIL;
  *label = False;
  if (EQUAL(SVAL(*v, 0), "LABEL"))
    *label = True;

  v = GetVariable(env, "SPIKE");
  if (v == NULL) return FAIL;
  *spike = IVAL(*v, 0);

  v = GetVariable(env, "STRETCH");
  if (v == NULL) return FAIL;
  *stretch = False;
  if (EQUAL(SVAL(*v, 0), "STRETCHED"))
    *stretch = True;

  return SUCCESS;
}


/************************************************************************/
/* PlotHist plots a histogram in the given region on the graphics plane,
 * using the given pen.  If label is True, then a DN value label and the
 * statistics (mean and sigma) are plotted and the histogram is shrunk to
 * make room.  If vertical is True, the histogram is printed with the DN
 * axis vertical, else it is printed horizontal.  The VRDI status is
 * returned.
 */

int PlotHist(env, hist, rgn, color, label, vertical, spike, mean, sigma)
  VIDSEnvironment	*env;		/* The VIDS environment		*/
  long			*hist;		/* the histogram to plot	*/
  Region		*rgn;		/* region to use for plotting	*/
  GraphColor		color;		/* pen color to use		*/
  Boolean		label;		/* controls printing of text	*/
  Boolean		vertical;	/* orientation of plot		*/
  int			spike;		/* number of spikes to saturate	*/
  double		mean;		/* mean of the histogram data	*/
  double		sigma;		/* sigma of the histogram	*/
{
  Rect plotarea;
  Point lblsize;
  int maxval, oldmax;
  int i, j, status;
  Rect impRect, rgnRect;
  Region *locRgn;
  Region *NextEmptyRegion();
  char text[30];

  SetRect(&impRect, 1, 1, env->nlMax, env->nsMax);

  locRgn = NextEmptyRegion(env);
  if (locRgn == NULL)
    return FAIL;
  MarkRgnTemp(locRgn);

  status = RegionToLocal(env, env->grafIMP, rgn, locRgn);
  if (status != SUCCESS)
    return status;

  BlockMove(&locRgn->bounds, &rgnRect, sizeof(Rect));

  if (! RectInRect(&rgnRect, &impRect))
    ABORT(FAIL,
    "Display region crosses edge of graphics plane, so can't display histogram",
    "VIDS-RGNXIMP");

  BlockMove(&rgnRect, &plotarea, sizeof(Rect));

  zddbatch(env->devUnit, True);

  if (label)
  {
    sprintf(text, "U=%3.1f  S=%3.1f", mean, sigma);

    if (vertical)
      status = PlotAxis(env, &plotarea, color, color, False, True, text, "");
    else
      status = PlotAxis(env, &plotarea, color, color, True, False, text, "");
    if (status != SUCCESS) {
      zddbatch(env->devUnit, False);
      return status;
    }
  }

  oldmax = 0;
  for (i=0; i<spike; i++)		/* de-spike the histogram */
  {
    maxval = 0;
    for (j=0; j<HSIZE; j++)
    {
      if ((hist[j] > maxval) && (oldmax == 0 || hist[j] < oldmax))
        maxval = hist[j];
    }
    if (maxval > 0)
      oldmax = maxval;
  }

  status = PlotHistData(env, hist, &plotarea, color, vertical, maxval);
  zddbatch(env->devUnit, False);
  if (status != SUCCESS)
    return status;

  return SUCCESS;
}

/************************************************************************/
/* PlotHistData plots the actual data for a histogram in the given area
 * on the graphics plane, using the given pen.  If vertical is True, the
 * histogram is printed with the DN axis vertical, else it is printed
 * horizontal.  The VRDI status is returned.
 */

int PlotHistData(env, hist, plotarea, color, vertical, maxval)
  VIDSEnvironment	*env;		/* The VIDS environment		*/
  long			*hist;		/* the histogram to plot	*/
  Rect			*plotarea;	/* area to plot in		*/
  GraphColor		color;		/* pen color to use		*/
  Boolean		vertical;	/* orientation of plot		*/
  int			maxval;		/* maximum value to plot	*/
{
  int unit;				/* device unit number		*/
  int imp;				/* image plane to use (graph)	*/
  double dnscale, valscale;
  int i, status;
  Point start, end;
  int dnsize, valsize;		/* size of plot area in DN and val directions */
  int dn, val;
  Boolean repeat;

  unit = env->devUnit;
  imp = env->grafIMP;

  if (vertical)
  {
    dnsize = (plotarea->bottom - plotarea->top + 1);
    valsize = (plotarea->right - plotarea->left);
  }
  else
  {
    dnsize = (plotarea->right - plotarea->left + 1);
    valsize = (plotarea->bottom - plotarea->top);
  }
  dnscale = (double)dnsize / (double)HSIZE;
  if (maxval == 0)
    valscale = 1.0;		/* shouldn't really happen */
  else
    valscale = (double)valsize / (double)maxval;

  /* Plot the axis */

  if (vertical)
  {
    start.v = plotarea->bottom;
    start.h = plotarea->left;
    end.v = plotarea->top;
    end.h = start.h;
  }
  else
  {
    start.v = plotarea->bottom;
    start.h = plotarea->left;
    end.v = start.v;
    end.h = plotarea->right;
  }
  status = DrawLine(unit, imp, GC(color), GC(color), &start, &end);
  if (status != SUCCESS)
    ABORT(FAIL, "Unable to draw histogram axis", "VIDS-VRDIERR");

  /* Now actually plot the histogram */

  for (i=0; i<HSIZE; i++)
  {
    val = (int)(MIN(hist[i],maxval) * valscale);
    dn = (int) (i * dnscale);
    repeat = True;
    while (repeat)
    {
      if (val != 0)
      {
        if (vertical)
        {
          start.h = plotarea->left;
          start.v = plotarea->bottom - dn;
          end.h = start.h + val;
          end.v = start.v;
        }
        else
        {
          start.v = plotarea->bottom;
          start.h = plotarea->left + dn;
          end.v = start.v - val;
          end.h = start.h;
        }
        status = DrawLine(unit, imp, GC(color), GC(color), &start, &end);
        if (status != SUCCESS)
	  ABORT(FAIL, "Unable to draw histogram data line", "VIDS-VRDIERR");
      }

      if ((dn + 1) < ((int) ((i+1) * dnscale)))
        dn++;				/* more than one line for this DN */
      else
        repeat = False;
    }
  }

  return SUCCESS;
}
