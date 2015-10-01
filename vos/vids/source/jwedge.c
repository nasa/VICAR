#include "VIDSdefs.h"

/* JWEDGE will draw a wedge in the image planes.
 */

int jwedge_do(env)

  VIDSEnvironment	*env;	/* The VIDS environment			*/
{
  int planeList[MAXPLANES];	/* list of planes to use		*/
  int nPlanes;			/* number of image planes to use	*/
  Region *rgn;			/* regions to display wedge in		*/
  int nRgns;			/* number of regions specified		*/
  int mindn, maxdn;		/* min and max values for wedge		*/
  int nsteps;			/* number of steps in the wedge		*/
  int i, status;		/* temporary increment, status variables*/
  TAEVariable *v;		/* temp VARIABLE pointer		*/
  TAEVariable *GetVariable();
  Rect impRect;
  Region *locRgn;
  Region *NextEmptyRegion();

  if (GetPlaneList(env, planeList, &nPlanes, False) != SUCCESS)
    return FAIL;
  if (GetRegionList(env, &rgn, &nRgns, "REGION", 1) != SUCCESS)
    return FAIL;

  v = GetVariable(env, "MIN");
  if (v == NULL) return FAIL;
  mindn = IVAL(*v, 0);

  v = GetVariable(env, "MAX");
  if (v == NULL) return FAIL;
  maxdn = IVAL(*v, 0);

  v = GetVariable(env, "NSTEPS");
  if (v == NULL) return FAIL;
  nsteps = IVAL(*v, 0);

  locRgn = NextEmptyRegion(env);
  if (locRgn == NULL)
    return FAIL;
  MarkRgnTemp(locRgn);

  SetRect(&impRect, 1, 1, env->nlMax, env->nsMax);

  zddbatch(env->devUnit, True);
  for (i = 0; i < nPlanes; i++)
  {
    status = RegionToLocal(env, planeList[i], rgn, locRgn);
    if (status != SUCCESS) {
      zddbatch(env->devUnit, False);
      return status;
    }

    if (! RectInRect(&locRgn->bounds, &impRect)) {
      zddbatch(env->devUnit, False);
      ABORT(FAIL,
      "Display region crosses edge of graphics plane, so can't display wedge",
      "VIDS-RGNXIMP");
    }

    status = DrawWedge(env, planeList[i], &locRgn->bounds, mindn, maxdn, nsteps);
    if (status != SUCCESS) {
      zddbatch(env->devUnit, False);
      return status;
    }
  }
  zddbatch(env->devUnit, False);
  return SUCCESS;
}


/************************************************************************/
/* DrawWedge draws a wedge in the given image plane and rectangle, using
 * the given range of DN's.
 */
int DrawWedge(env, imp, bounds, mindn, maxdn, nsteps)
  VIDSEnvironment	*env;	/* The VIDS environment			*/
  int		imp;		/* Image plane to use			*/
  Rect		*bounds;	/* Bounding rectangle of wedge		*/
  int		mindn;		/* Minimum DN value			*/
  int		maxdn;		/* Maximum DN value			*/
  int		nsteps;		/* Number of steps to use		*/
{
  int i, status;
  int size;
  int pix;
  double curstep, stepsize;
  double curdnstep, dnstepsize;
  unsigned char *buf;

  size = bounds->right - bounds->left + 1;
  if (size <= 0)
    ABORT(FAIL, "Size of wedge is too small", "VIDS-TOOSMALL");

  if (mindn > maxdn)
    ABORT(FAIL, "Sorry, MIN must be smaller than MAX.", "VIDS-BADPARAM");

  buf = malloc(size);
  if (buf == NULL)
    ABORT(FAIL, "Insufficient memory to draw wedge", "VIDS-INSUFMEM");

  if (nsteps <= 1)
  {
    stepsize = (maxdn-mindn);
    dnstepsize = (maxdn-mindn);
  }
  else
  {
    stepsize = (maxdn-mindn) / (double)nsteps;
    dnstepsize = (maxdn-mindn) / (double)(nsteps-1);
  }
  if (stepsize < 1.0)
    stepsize = 1.0;
  if (dnstepsize < 1.0)
    dnstepsize = 1.0;

  if (size == 1)
    buf[0] = 0;			/* avoid divide by 0 below */
  else
    curstep = 0.0;		/* location of current step */
    curdnstep = 0.0;		/* dn value to use at current step */
    for (i=0; i<size; i++)
    {
      pix = (int)((double)i * (double)(maxdn-mindn) / (double)(size-1));
      while (pix >= (int)(curstep+stepsize))
      {
        curstep += stepsize;
        curdnstep += dnstepsize;
      }
      buf[i] = MIN(maxdn, mindn + (int)curdnstep);
    }

  for (i=bounds->top; i<=bounds->bottom; i++)
  {
    status = zdilinewrite(env->devUnit, imp, bounds->left, i, size, buf);
    if (status != SUCCESS)
    {
      free(buf);
      ABORT(FAIL, "Unable to write wedge to display device", "VIDS-VRDIERR");
    }
  }

  free(buf);

  return SUCCESS;
}
