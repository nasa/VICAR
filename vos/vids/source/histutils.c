/* HistUtils.c contains code needed to collect, check, maintain, etc.
 * histograms.
 */
#include "VIDSdefs.h"
/************************************************************************/
/* CurrentHist returns a pointer to a filled histogram for a given
 * plane.  If no histogram has been collected or if the region pointer
 * is not null, it collects one on the given region or current access
 * window (last area displayed).  Returns NULL on failure.
 */
long *CurrentHist(env, imp, rgn)
  VIDSEnvironment	*env;
  int			imp;		/* imp whose hist we want	*/
  Region		*rgn;		/* region on which to collect	*/
{
  PlaneInfo	*plane;
  int		seed;
  
  plane = &env->planes[imp];
  seed = (rgn == NULL) ? 0 : rgn->seed;

  if ((plane->hist.array != NULL) && (plane->hist.rgnseed == seed))
    return (plane->hist.array);		 /* If already has hist, return	*/

  if (plane->hist.array == NULL)
  {
    plane->hist.array = (long *) malloc(HSIZE * sizeof(long));
    if (plane->hist.array == NULL)
      ABORT(NULL, "Insufficient memory to collect histogram", "VIDS-INSUFMEM");
  }

  if (CollectHist(env, imp, rgn, plane->hist.array) != SUCCESS)
    return NULL;

  plane->hist.rgnseed = seed;
  return (plane->hist.array);
}
/************************************************************************/
/* InvalHist marks the histogram for this plane as invalid, by
 * freeing it's memory.  This routine should be called every time
 * something changes that would affect the histogram (redisplaying, for
 * example).
 */

InvalHist(plane)
  PlaneInfo *plane;
{

if (plane->hist.array != NULL)
  free(plane->hist.array);
plane->hist.array = NULL;
plane->hist.rgnseed = 0;
}

/************************************************************************/
/* InvalHistLoc also marks the histogram for this plane as invalid,
 * by freeing it's memory.  This routine should be called when the location
 * of the regions change, but not the actual data in the image plane.
 * Examples are hardware pan and zoom.  This routine works like
 * InvalHist() except it doesn't invalidate if the region seed is 0.
 * That's the case when the histogram was collected not over a region, but
 * over the access window.  If only the region definitions change (and not
 * the image data), the hist is still good if it's defined in terms of the
 * access window.
 */

InvalHistLoc(plane)
  PlaneInfo *plane;
{

if (plane->hist.rgnseed != 0)
  InvalHist(plane);

}

/************************************************************************/
/* CollectHist collects a histogram from the given plane in the given
 * region.  The region is translated to local imp coords in this routine.
 * If the region is NULL, the current access window is used.  The VRDI
 * status is returned.  May also return the status CLIPPED, if the region
 * is off the image plane.
 */

int CollectHist(env, imp, rgn, hist)
  VIDSEnvironment	*env;		/* The VIDS environment		*/
  int			imp;		/* image memory plane to use	*/
  Region		*rgn;		/* region to use for collection	*/
  long			*hist;		/* returned histogram		*/
{
  int unit;			/* device unit number			*/
  int size, i, status;
  int y, x1, x2;
  RegionState rgnstate;		/* saves state info for NextRegionLine() */
  Boolean NextRegionLine();
  Region *locRgn;		/* rgn in local image plane coords	*/
  Rect points;			/* used if we define our own region	*/
  Rect impRect;
  PlaneInfo *plane;
  Region *PointsToRegion(), *NextEmptyRegion();

  NotifyUser(Inform, "", "Collecting histogram for plane %d.", imp);

  SetRect(&impRect, 1, 1, env->nlMax, env->nsMax);

  plane = &env->planes[imp];

  if (rgn == NULL)		/* defaulted, assign 'last displayed' region */
  {
    SizeToRect(&plane->accessWindow, &points);
    if (plane->accessWindow.ns == 0)
      points.right = env->nsMax;
    if (plane->accessWindow.nl == 0)
      points.bottom = env->nlMax;

	/* NOTE: a Rectangle structure is being used as an array of */
	/* two points below.  This is defined to work correctly.    */
    locRgn = PointsToRegion(env, "HIST$TEMP", Rectangle, &points, 2);
    if (locRgn == NULL)
      return FAIL;
    MarkRgnTemp(locRgn);
  }
  else				/* not defaulted; convert to local imp coords */
  {
    locRgn = NextEmptyRegion(env);
    if (locRgn == NULL)
      return FAIL;
    MarkRgnTemp(locRgn);

    status = RegionToLocal(env, imp, rgn, locRgn);
    if (status != SUCCESS)
      return status;

    if (! RectInRect(&locRgn->bounds, &impRect))
    {
      NotifyUser(Inform,"",
"Region \"%s\" crosses the edge of plane %d, so cannot collect histogram",
	      locRgn->name, imp);
      return CLIPPED;
    }
  }

  unit = env->devUnit;

  for (i=0; i<HSIZE; i++)
    hist[i] = 0;

  switch (locRgn->type)
  {
    case Rectangle :
    case Square :
      status = zdiawset(unit, imp, locRgn->bounds.left,
				 locRgn->bounds.top,
				 locRgn->bounds.right,
				 locRgn->bounds.bottom);
      if (status != SUCCESS)
	ABORT(FAIL, "Error setting access window", "VIDS-VRDIERR");
      size = (locRgn->bounds.right - locRgn->bounds.left + 1) *
             (locRgn->bounds.bottom - locRgn->bounds.top + 1);
      status = zdiawread(unit, imp, size, env->buffer);
      zdiawset(unit, imp, 1, 1, env->nsMax, env->nlMax);
      if (status != SUCCESS)
	ABORT(FAIL, "Error reading histogram data from device", "VIDS-VRDIERR");

      for (i=0; i<size; i++)
        hist[*(env->buffer+i)]++;

      break;

    case Circle:
    case Oval:
    case Polygon:
      status = StartRegionLine(locRgn, &rgnstate);
      if (status != SUCCESS)
        ABORT(FAIL, "Insufficient memory to collect the histogram", "VIDS-INSUFMEM");
      while (NextRegionLine(locRgn, &rgnstate, &y, &x1, &x2))
      {
        size = x2 - x1 + 1;
        status = zdilineread(unit, imp, x1, y, size, env->buffer);
        if (status != SUCCESS)
	  ABORT(FAIL, "Error reading data from display device", "VIDS-VRDIERR");

        for (i=0; i<size; i++)
          hist[*(env->buffer+i)]++;
      }
      break;
  }

  return SUCCESS;
}

/************************************************************************/
/* GatherHistStats gathers statistics for a given histogram.
 */

GatherHistStats(hist, mean, sigma)
  long			*hist;		/* the histogram to use		*/
  double		*mean;		/* returned mean of data	*/
  double		*sigma;		/* returned sigma of data	*/
{
  int i;
  double rsum0, rsum1, rsum2, r;
  double variance;
  double sqrt();

  rsum0 = 0;
  rsum1 = 0;
  rsum2 = 0;

  for (i=0; i<HSIZE; i++)
  {
    r = (double) hist[i];
    rsum0 += r;
    rsum1 += r * i;
    rsum2 += r * i * i;
  }
  if (rsum0 == 0.0)
  {
    *mean = 0.0;
    *sigma = 0.0;
  }
  else
  {
    *mean = rsum1 / rsum0;
    variance = (rsum2 / rsum0) - (*mean * *mean);
    if (variance < 0.0)
      variance = 0.0;
    *sigma = sqrt(variance);
  }
}

