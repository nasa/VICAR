#include "VIDSdefs.h"
#include "verrdefs.h"		/* vrdi error definitions		*/

enum method {Hardware, Software, Best};
enum ref {Home, Center};
/************************************************************************/
/* jzoom_do -- code for jzoom command.  Jzoom will apply the given 
 * zoom factor to the image file.  If possible, it will apply a hardware
 * zoom, otherwise it will set a software zoom and redisplay.
 */
int jzoom_do(env)
  VIDSEnvironment	*env;	/* The VIDS environment			*/
{
  Point		loc;		/* Top left of new display window	*/
  Point		tmpLoc;		/* temp copy of loc			*/
  int		zoom;		/* zoom factor to be applied		*/
  int		impZoom;	/* individually calculated zoom for imp	*/
  Region	*rgn;		/* region to fit			*/
  Region	*locRgn;	/* region in image plane coordinates	*/
  enum method	theMethod;
  enum ref	reference;
  int		imps[MAXPLANES], nimps, imp;
  int		i,status;
  int		unit;		/* the device unit number		*/
  Boolean	independent;	/* True if planes can be zoomed independently */
  PlaneInfo	*p;		/* temp ptr to a plane			*/
  Boolean	raw;		/* True if coords are raw (cursor) loc	*/
  int		maxZoom;	/* Maximum supported hardware zoom	*/
  Point		orig;		/* Point representing origin (1,1)	*/
  Rect		impRect;
  Region *NextEmptyRegion();

  orig.h = orig.v = 1;

  GetZoomParms(env, &zoom, &loc, &theMethod, imps, &nimps, &reference, &raw, &rgn);
  unit = env->devUnit;
  
  zddinfo(unit, 38, 1, &maxZoom);	/* Get max zoom supported in hw	*/

  status = zddinfo(unit, 27, 1, &independent);
  if (! independent)
  {
    NotifyUser(Inform,"","**** All planes must be zoomed together.");
    zdeaction(1, 2, 2);
  }

  SetRect(&impRect, 1, 1, env->nlMax, env->nsMax);

  zddbatch(unit, True);

  for (i = 0; i < nimps; i++)
  {					/* apply zoom to each plane	*/
    imp = imps[i];
    p = &env->planes[imp];
    impZoom = zoom;

    locRgn = NULL;
    if (rgn != NULL)			/* convert region to local IMP coords */
    {
      locRgn = NextEmptyRegion(env);
      if (locRgn == NULL) {
        zddbatch(unit, False);
        return FAIL;
      }
      MarkRgnTemp(locRgn);

      status = RegionToLocal(env, imp, rgn, locRgn);
      if (status != SUCCESS) {
        zddbatch(unit, False);
        return status;
      }

      if (! RectInRect(&locRgn->bounds, &impRect)) {
        zddbatch(unit, False);
        ABORT(FAIL,
             "Zoom region crosses edge of the image plane, so can't zoom to it",
             "VIDS-RGNXIMP");
      }
    }

    if (theMethod == Hardware)		/* Specifically requested hw	*/
    {					/* zoom, so just do it.		*/
      if (locRgn != NULL)
      {
        HZoomRgn(env, locRgn, &impZoom, &loc, reference);
        if (zoom != 0) impZoom = zoom;
      }
      HardwareZoom(env, imp, impZoom, &loc, raw, reference);
    }
    else if (theMethod == Software)	/* Specifically requested sw	*/
    {					/* zoom, so just do it.		*/
      if (locRgn != NULL)
      {
        SZoomRgn(env, imp, locRgn, &impZoom, &loc, reference);
        if (zoom != 0) impZoom = zoom;
      }
      zddbatch(unit, False);
      zddbatch(unit, True);
      SoftwareZoom(env, imp, impZoom, &loc, raw, reference);
    }
    else /* theMethod == Best */	/* Do fastest zoom with best	*/
    {					/* resolution.			*/
      if (locRgn != NULL)	/* If zooming to fit a region,		*/
      {				/* calculate a new zoom for each plane	*/
        SZoomRgn(env, imp, locRgn, &impZoom, &loc, reference);
        if (zoom != 0) impZoom = zoom;
      }
      if (impZoom < 1)		/* zoom < 1 must be software zoom	*/
      {
        if (zdszoom(unit,imp) != 1)
          HardwareZoom(env, imp, 1, &orig, False, Home);
        if (p->softZoom != impZoom) {
          zddbatch(unit, False);
          zddbatch(unit, True);
          SoftwareZoom(env, imp, impZoom, &loc, raw, reference);
        }
      }
      else			/* 1 <= zoom <= maxZoom is hw zoom	*/
      {
        if (impZoom > maxZoom)		/* Do software zoom if greater	*/
	{				/* than max allowed.		*/
          HardwareZoom(env, imp, 1, &orig, False, Home);
	  if (p->softZoom != impZoom) {
            zddbatch(unit, False);
            zddbatch(unit, True);
            SoftwareZoom(env, imp, impZoom, &loc, raw, reference);
          }
	}
        else
        {
          if (locRgn != NULL) /* If zooming to a region, loc is in file coords */
          {
            CursFile2IMP(env, imp, loc.h, loc.v, &tmpLoc.h, &tmpLoc.v);
            HardwareZoom(env, imp, impZoom, &tmpLoc, False, reference);
	    if (p->softZoom != 1) {
              zddbatch(unit, False);
              zddbatch(unit, True);
              SoftwareZoom(env, imp, 1, &loc, False, reference);
            }
          }
          else if (raw)
          {
            CursRaw2IMP(env, imp, loc.h, loc.v, &tmpLoc.h, &tmpLoc.v);
            CursIMP2File(p, tmpLoc.h, tmpLoc.v, &tmpLoc.h, &tmpLoc.v);
            HardwareZoom(env, imp, impZoom, &loc, True, reference);
	    if (p->softZoom != 1) {
              zddbatch(unit, False);
              zddbatch(unit, True);
              SoftwareZoom(env, imp, 1, &tmpLoc, False, reference);
            }
          }
          else
          {
            HardwareZoom(env, imp, impZoom, &loc, raw, reference);
	    if (p->softZoom != 1) {
              zddbatch(unit, False);
              zddbatch(unit, True);
              SoftwareZoom(env, imp, 1, &loc, raw, reference);
            }
          }
        }
      }
    }
  }
  zddbatch(unit, False);

  return SUCCESS;
}
/************************************************************************/
/* GetZoomParms parses the parameters for the jzoom command.
 */
int GetZoomParms(env, zoom, loc, theMethod, imps, nimps, reference, raw, rgn)
  VIDSEnvironment	*env;		/* The VIDS environment		*/
  int			*zoom;
  Point			*loc;
  enum method		*theMethod;
  int			imps[], *nimps;
  enum ref		*reference;
  Boolean		*raw;		/* True if loc is raw coord	*/
  Region		**rgn;
{
  TAEVariable	*v;		/* temp VARIABLE pointer		*/
  int		n;
  TAEVariable *GetVariable();

  *raw = True;
  *zoom = 0;
  n = zdclocation(env->devUnit, env->cursor.number,
		  &loc->h, &loc->v);
  if (n != SUCCESS)
    loc->h = loc->v = 1;

  v = GetVariable(env, "REFERENC");
  if (v == NULL) return FAIL;
  switch (*SVAL(*v, 0))
  {
    case 'H' : *reference = Home; break;
    default  : *reference = Center; break;
  }

  *rgn = NULL;
  if (GetRegionList(env, rgn, &n, "REGION", 1) != SUCCESS) return FAIL;
  if (n == 1)
  {
    *raw = False;
  }

  v = GetVariable(env, "FACTOR");	/* if FACTOR given, let it	*/
  if (v == NULL) return FAIL;		/* override the ZoomRgn factor	*/
  if (v->v_count == 1) *zoom = IVAL(*v, 0);

  if (*rgn == NULL && v->v_count == 0)	/* if no factor given, default to 1 */
      *zoom = 1;

  if (GetPlaneList(env, imps, nimps, False) != SUCCESS) return FAIL;

  v = GetVariable(env, "SOURCE");
  if (v == NULL) return FAIL;
  switch (*SVAL(*v, 0))
  {
    case 'D' : *theMethod = Hardware; break;
    case 'F' : *theMethod = Software; break;
    default  : *theMethod = Best; break;
  }

  v = GetVariable(env, "LOCATION");
  if (v == NULL) return FAIL;
  if (v->v_count == 2)
  {
    *raw = False;
    loc->h = IVAL(*v, 1);
    loc->v = IVAL(*v, 0);
  }

  return SUCCESS;
}
/************************************************************************/
/* HZoomRgn calculates a zoom for a plane based on the specified region
 * filling the screen.
 */
HZoomRgn(env, rgn, zoom, loc, reference)
  VIDSEnvironment	*env;
  Region		*rgn;
  int			*zoom;
  Point			*loc;
  enum ref		reference;
{
  float	x,y;
  Rect	*b;

  b = &rgn->bounds;
  if (reference == Home)
  {
    loc->h = b->left;
    loc->v = b->top;
  }
  else /* reference == Center */
  {
    loc->h = (b->right + b->left + 1) / 2;
    loc->v = (b->bottom + b->top + 1) / 2;
  }
  y = (float) env->nlMax / (float) (b->bottom - b->top + 1);
  x = (float) env->nsMax / (float) (b->right - b->left + 1);
  
  x = MIN(x,y);
  
  *zoom = (int) ((x < 1.0) ? -(1.0/x + 0.999) : x);
  return;
}
/************************************************************************/
/* SZoomRgn calculates a zoom for a plane based on the contents of that
 * plane within the specified region filling the screen.
 */
SZoomRgn(env, imp, rgn, zoom, loc, reference)
  VIDSEnvironment	*env;
  int			imp;
  Region		*rgn;
  int			*zoom;
  Point			*loc;
  enum ref		reference;
{
  float	x,y;
  Rect	b;

  BlockMove(&rgn->bounds, &b, sizeof(Rect));
  CursIMP2File(&env->planes[imp], b.left, b.top, &b.left, &b.top);
  CursIMP2File(&env->planes[imp], b.right, b.bottom, &b.right, &b.bottom);
  if (reference == Home)
  {
    loc->h = b.left;
    loc->v = b.top;
  }
  else /* reference == Center */
  {
    loc->h = (b.right + b.left + 1) / 2;
    loc->v = (b.bottom + b.top + 1) / 2;
  }
  y = (float) env->nlMax / (float) (b.bottom - b.top + 1);
  x = (float) env->nsMax / (float) (b.right - b.left + 1);
  
  x = MIN(x,y);
  
  *zoom = (int) ((x < 1.0) ? -(1.0/x + 0.999) : x);
  return;
}
/************************************************************************/
/* HardwareZoom performs a hardware zoom on a single image plane.
 */
int HardwareZoom(env, imp, zoom, loc, raw, reference)
  VIDSEnvironment	*env;
  int			imp;		/* IMP to zoom			*/
  int			zoom;		/* zoom factor			*/
  Point			*loc;		/* Location to which to pan	*/
  Boolean		raw;		/* True if loc needs to be conv.*/
  enum ref		reference;	/* Whether loc is Center or Home*/
{
  int x,y,unit,status,nlVideo,nsVideo;
  PlaneInfo *p;

  p = &env->planes[imp];
  unit = env->devUnit;

  nsVideo = VideoSamps(env);		/* fill in video size constants	*/
  nlVideo = VideoLines(env);		/* for use below.		*/

  x = loc->h; y = loc->v;
  if (raw)
    CursRaw2IMP(env, imp, loc->h, loc->v, &x, &y);
  NotifyUser(Inform,"","Applying hardware zoom of %d to plane %d",
             zoom, imp);
  status = zdizoom(unit, imp, zoom);
  if ((status != SUCCESS) && (status != MUSTZOOM))
    ABORT(FAIL, "Sorry, cannot zoom by that factor.", "VIDS-VRDIERR");
  InvalHistLoc(p);	/* If histogram was tied to region, now invalid	*/
  if (reference == Center)
  {
    x = x - (Zoom(nsVideo, -zoom) / 2);
    y = y - (Zoom(nlVideo, -zoom) / 2);
  }
  x = MAX(1, x);	y = MAX(1, y);			/* Limit the pan*/
  x = MIN(x, env->nsMax - Zoom(nsVideo, -zoom) + 1);/* to prevent	*/
  y = MIN(y, env->nlMax - Zoom(nlVideo, -zoom) + 1);/* wrapping.	*/
  status = zdidwset(unit, imp, x, y);
  if ((status != SUCCESS) && (status != MUSTSETDW))
    NotifyUser(Inform, "", "Sorry, Unable to pan to the location (%d,%d).",
               y, x);
  return SUCCESS;
}
/************************************************************************/
/* SoftwareZoom redisplays an image plane at the specified zoom factor.
 */
int SoftwareZoom(env, imp, zoom, loc, raw, reference)
  VIDSEnvironment	*env;
  int			imp;		/* IMP to zoom			*/
  int			zoom;		/* zoom factor			*/
  Point			*loc;		/* Location to which to pan	*/
  Boolean		raw;		/* True if loc needs to be conv.*/
  enum ref		reference;	/* Whether loc is Center or Home*/
{
  PlaneInfo	*p;
  int		x,y,unit,nlVideo,nsVideo;

  p = &env->planes[imp];
  if (p->file == NULL) return SUCCESS;

  unit = env->devUnit;
  nsVideo = VideoSamps(env);		/* fill in video size constants	*/
  nlVideo = VideoLines(env);		/* for use below.		*/
  nsVideo = Zoom(nsVideo, -zdszoom(unit,imp));
  nlVideo = Zoom(nlVideo, -zdszoom(unit,imp));

  x = loc->h; y = loc->v;
  if (raw)
  {
    CursRaw2IMP(env, imp, loc->h, loc->v, &x, &y);
    CursIMP2File(p, x, y, &x, &y);
  }
  if (reference == Center)
  {
    x = x - Zoom(nsVideo / 2 + zdsdwsamp(unit,imp) - 1, -zoom);
    y = y - Zoom(nlVideo / 2 + zdsdwline(unit,imp) - 1, -zoom);
  }
  p->softZoom = zoom;
  x = MIN(x, p->file->ns - Zoom(nsVideo, -zoom) + 1);  /* Limit the pan	*/
  y = MIN(y, p->file->nl - Zoom(nlVideo, -zoom) + 1);  /* to prevent	*/
  x = MAX(1, x);	y = MAX(1, y);		       /* wrapping.	*/
  p->accessWindow.sl = p->accessWindow.ss = 1;
  p->accessWindow.nl = p->accessWindow.ns = 0;
  p->imageWindow.sl = y;			/* area of file to be	*/
  p->imageWindow.ss = x;			/* redisplayed.		*/
  p->imageWindow.nl = 0;
  p->imageWindow.ns = 0;
  p->subPixel.left = p->subPixel.top = 0;
  NotifyUser(Inform,"","Applying software zoom of %d to plane %d",
             zoom, imp);
  LoadIMP(env, imp);
  return SUCCESS;
}
/************************************************************************/
