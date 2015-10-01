/* GraphUtils.c contains the simple graphics routines to handle
 * things such as logical operations on rectangles, regions, etc.
 */
#include "VIDSdefs.h"
#include <math.h>

#define PI 3.141592654

/************************************************************************/
/* PointsToRect returns the bounding rectangle for a set of points
 */
PointsToRect(points, nPoints, theRect)
  Point		points[];	/* in: array of points			*/
  int		nPoints;	/* in: number of points in points[]	*/
  Rect		*theRect;	/* out: bounding rectangle of points	*/
{
  register int i,l,r,t,b;
  
  l = t = 32767;
  r = b = 0;
  for (i = 0; i < nPoints; i++)
  {
    l = MIN(l, points[i].h);
    r = MAX(r, points[i].h);
    t = MIN(t, points[i].v);
    b = MAX(b, points[i].v);
  }
  SetRect(theRect, t, l, b, r);
  return;
}
/************************************************************************/
/* SizeToRect converts between a SizeField and a Rect.
 */
SizeToRect(s, r)
  SizeField	*s;	/* in: a size field (sl,ss,nl,ns)		*/
  Rect		*r;	/* out: equivalent Rect				*/
{
  r->left = s->ss;	r->right = s->ss + s->ns - 1;
  r->top = s->sl;	r->bottom = s->sl + s->nl - 1;
  return;
}
/************************************************************************/
/* RectToSize converts between a Rect to a SizeField.
 */
RectToSize(r, s)
  Rect		*r;	/* out: equivalent Rect				*/
  SizeField	*s;	/* in: a size field (sl,ss,nl,ns)		*/
{
  s->ss = r->left;	s->ns = r->right - r->left + 1;
  s->sl = r->top;	s->nl = r->bottom - r->top + 1;
  return;
}
/************************************************************************/
/* EqualRect returns True if the two given rectangles are identical;
 * False otherwise.
 */
Boolean EqualRect(r1, r2)
  Rect	*r1, *r2;			/* two rectangles to compare	*/
{
  if ((r1->left  == r2->left ) && (r1->top    == r2->top   ) &&
      (r1->right == r2->right) && (r1->bottom == r2->bottom))
        return True;
  return False;
}
/************************************************************************/
/* RectInRect returns True if the rect r1 is entirely contained within
 * rect r2.
 */
Boolean RectInRect(r1, r2)
  Rect	*r1, *r2;			/* two rectangles to compare	*/
{
  if ((r1->left  >= r2->left ) && (r1->top    >= r2->top   ) &&
      (r1->right <= r2->right) && (r1->bottom <= r2->bottom))
        return True;
  return False;
}
/************************************************************************/
/* EmptyRect returns True if the given rectangle is empty.
 */
Boolean EmptyRect(r)
  Rect *r;
{
  if ((r->left > r->right) || (r->top > r->bottom))
    return True;
  return False;
}
/************************************************************************/
/* OffsetRect will offset rect r1 by the h/v increments dh,dv and store
 * the resulting rect in r1.
 */
OffsetRect(r1, dv, dh)
  Rect	*r1;			/* input/output rectangle		*/
  int	dv,dh;			/* vertical/horizontal increments	*/
{
  r1->left += dh;
  r1->right += dh;
  r1->top += dv;
  r1->bottom += dv;
  return;
}
/************************************************************************/
/* HomeRect will take a rectangle r1 which has points beyond the bounds
 * (1,1,nl,ns) and shift each point individually by multiples of nl or
 * ns to make it fit within the bounds.  The resulting rect is stored
 * in r2.
 */
HomeRect(r1, r2, nl, ns)
  Rect	*r1,*r2;		/* input/output rectangles		*/
  int	nl,ns;			/* lines/samps in home area		*/
{
  int top,left;			/* local copies to allow r1==r2		*/

  top = r1->top;
  left = r1->left;
  r2->top    = IntToRange(top,  1, nl);
  r2->left   = IntToRange(left, 1, ns);
  r2->bottom = r2->top + r1->bottom - top;
  r2->right  = r2->left + r1->right - left;
  return;
}
/************************************************************************/
/* HomePoint will take a point p1 which lies beyond the bounds
 * (1,1,nl,ns) and shift it by multiples of nl or ns to make it
 * fit within the bounds.  The resulting point is stored in p2.
 */
HomePoint(p1, p2, nl, ns)
  Point	*p1,*p2;		/* input/output points			*/
  int	nl,ns;			/* lines/samps in home area		*/
{
  p2->v = IntToRange(p1->v, 1, nl);
  p2->h = IntToRange(p1->h, 1, ns);
  return;
}
/************************************************************************/
/* SectRect takes the intersection of r1 and r2 and stores it in r3.
 * Note that no check for an empty rect is necessary since if they do
 * not intersect they will automatically satisfy the conditions of
 * the routine EmptyRect.
 */
SectRect(r1, r2, r3)
  Rect		*r1,*r2;		/* input rectangles		*/
  Rect		*r3;			/* output rectangle		*/
{
  r3->left = MAX(r1->left, r2->left);
  r3->right = MIN(r1->right, r2->right);
  r3->top = MAX(r1->top, r2->top);
  r3->bottom = MIN(r1->bottom, r2->bottom);
  return;
}
/************************************************************************/
/* ZoomRect will apply the zoom factor zoom to the rectangle r.
 */
ZoomRect(r, zoom)
  Rect	*r;
  int	zoom;
{
  r->left = Zoom(r->left, zoom); r->right  = Zoom(r->right,  zoom);
  r->top  = Zoom(r->top,  zoom); r->bottom = Zoom(r->bottom, zoom);
  if (zoom > 1)
  {					/* Move left side of rect to	*/
    r->left = r->left - zoom + 1;	/* the left edge of pixel	*/
    r->top  = r->top  - zoom + 1;
  }
  return;
}
/************************************************************************/
/* IntToRange will take an integer and add/subtract the range size until
 * it falls in the range; for example, to force an angle to be between
 * 0 and 360 degrees.
 */
int IntToRange(n, low, high)
  int	n;			/* integer in question			*/
  int	low,high;		/* low and high ends of the range	*/
{
  int size;

  size = high - low + 1;
  while (n < low) n += size;
  while (n > high) n -= size;
  return n;
}
/************************************************************************/
/* EqualPoint returns True if the two given points are identical;
 * False otherwise.
 */
Boolean EqualPoint(p1, p2)
  Point	*p1, *p2;			/* two Points to compare	*/
{
  if ((p1->h  == p2->h) && (p1->v == p2->v)) return True;
  return False;
}
/************************************************************************/
/* SquarePoints takes the four x,y locations of a rectangle, makes a
 * square out of them, keeping the first point in the same spot.
 */
SquarePoints(x0,y0,x1,y1)
  int x0,y0;			/* in: x,y of one corner of rectangle	*/
  int *x1,*y1;			/* in/out: x,y of opposite corner	*/
{
  int temp;
  
  temp = MIN(ABS(*y1 - y0), ABS(*x1 - x0));
  *x1 = (*x1 > x0) ? (x0 + temp) : (x0 - temp);
  *y1 = (*y1 > y0) ? (y0 + temp) : (y0 - temp);
  return;
}
/************************************************************************/
/* FrameRgn will draw a frame around a region in given plane.
 * Returns SUCCESS or FAIL.
 */
int FrameRgn(unit, imp, mask, color, rgn)
  int			unit,imp;	/* device unit no. and image plane */
  int			mask;		/* which bits to frame		*/
  int			color;		/* color with which to frame rgn*/
  Region		*rgn;		/* region to frame		*/
{
  int status;

  switch (rgn->type)
  {
    case Square :
    case Rectangle :
      status = FrameRect(unit, imp, mask, color, &rgn->bounds);
      break;
    case Circle:
    case Oval :
      status = FrameOval(unit, imp, mask, color, &rgn->bounds);
      break;
    case Polygon :
      status = FramePoly(unit, imp, mask, color, rgn->pointList, rgn->nPoints);
      break;
  }
  return status;
}
/************************************************************************/
/* FrameRect will draw a frame around a rectangle in given plane.
 * Returns SUCCESS or FAIL.
 */
int FrameRect(unit, imp, mask, color, theRect)
  int			unit,imp;	/* dev unit no. and image plane	*/
  int			mask;		/* which bits to frame		*/
  int			color;		/* color with which to frame	*/
  Rect			*theRect;	/* rectangle to frame		*/
{
  int	x[5],y[5];	/* x,y-coord. arrays	*/
  
  x[0] = x[3] = x[4] = theRect->left;
  x[1] = x[2] =        theRect->right;
  y[0] = y[1] = y[4] = theRect->top;
  y[2] = y[3] =        theRect->bottom;

  if (zdimpolyline(unit, imp, mask, color, 5, x, y) != SUCCESS)
    return FAIL;
  return SUCCESS;
}
/************************************************************************/
/* FramePoly will frame a polygon in the given plane.  Maintain in parallel
 * with FrameLineSegs().
 * Returns SUCCESS or FAIL.
 */
int FramePoly(unit, imp, mask, color, points, nPoints)
  int			unit,imp;	/* dev unit no. and image plane	*/
  int			mask;		/* which bits to frame		*/
  int			color;		/* color with which to frame	*/
  Point			points[];	/* array of points in polygon	*/
  int			nPoints;	/* number of points		*/
{
  int i,status;
  int *x, *y;
  
  x = malloc((nPoints + 1) * sizeof(int));
  if (x == NULL)
  {
    NotifyUser(Inform,"VIDS-INSUFMEM","Insufficient memory to frame polygon");
    return FAIL;
  }
  y = malloc((nPoints + 1) * sizeof(int));
  if (y == NULL)
  {
    NotifyUser(Inform,"VIDS-INSUFMEM","Insufficient memory to frame polygon");
    free(x);
    return FAIL;
  }

  for (i = 0; i < nPoints; i++)
  {
    x[i] = points[i].h;
    y[i] = points[i].v;
  }
  x[nPoints] = points[0].h;		/* close the  polygon	*/
  y[nPoints] = points[0].v;

  i = nPoints + 1;
  status = zdimpolyline(unit, imp, mask, color, i, x, y);
  free(x);
  free(y);
  return status;
}
/************************************************************************/
/* FrameLineSegs will frame a polygon in the given plane without closing
 * it (i.e., without connecting the end to the start).  Maintain in parallel
 * with FramePoly().
 * Returns SUCCESS or FAIL.
 */
int FrameLineSegs(unit, imp, mask, color, points, nPoints)
  int			unit,imp;	/* dev unit no. and image plane	*/
  int			mask;		/* which bits to frame		*/
  int			color;		/* color with which to frame	*/
  Point			points[];	/* array of points in polygon	*/
  int			nPoints;	/* number of points		*/
{
  int i,status;
  int *x, *y;
  
  x = malloc(nPoints * sizeof(int));
  if (x == NULL)
  {
    NotifyUser(Inform,"VIDS-INSUFMEM","Insufficient memory to frame polygon");
    return FAIL;
  }
  y = malloc(nPoints * sizeof(int));
  if (y == NULL)
  {
    NotifyUser(Inform,"VIDS-INSUFMEM","Insufficient memory to frame polygon");
    free(x);
    return FAIL;
  }

  for (i = 0; i < nPoints; i++)
  {
    x[i] = points[i].h;
    y[i] = points[i].v;
  }

  i = nPoints;
  status = zdimpolyline(unit, imp, mask, color, i, x, y);
  free(x);
  free(y);
  return status;
}
/************************************************************************/
/* FrameArrow will draw an arrow from point 1 to point 2 in the given plane.
 * The head is at point 2.  This is named FrameArrow for consistency with
 * the other Frame* routines in this module.
 * Returns SUCCESS or FAIL.
 */
int FrameArrow(unit, imp, mask, color, point1, point2)
  int			unit,imp;	/* dev unit no. and image plane	*/
  int			mask;		/* which bits to frame		*/
  int			color;		/* color with which to frame	*/
  Point			*point1;	/* point at tail of arrow	*/
  Point			*point2;	/* point at head of arrow	*/
{
  double angle;
  int x[6], y[6];
  int status;

  if ((point1->v == point2->v) && (point1->h == point2->h))
    angle = 0;			/* head == tail, so use angle=0 arbitrarily */
  else
    angle = atan2((double)point1->v-point2->v, (double)point1->h-point2->h);

  x[0] = point1->h;			/* tail of arrow */
  y[0] = point1->v;

  x[1] = point2->h;			/* head of arrow */
  y[1] = point2->v;

  x[2] = point2->h + 5.0*cos(angle+PI/6.0) + 0.5;	/* spread 30 degrees */
  y[2] = point2->v + 5.0*sin(angle+PI/6.0) + 0.5;	/* on each side */

  x[3] = point2->h + 2.0*cos(angle) + 0.5;	/* to centerline of arrow */
  y[3] = point2->v + 2.0*sin(angle) + 0.5;

  x[4] = point2->h + 5.0*cos(angle-PI/6.0) + 0.5;	/* other side */
  y[4] = point2->v + 5.0*sin(angle-PI/6.0) + 0.5;

  x[5] = point2->h;					/* back to tip */
  y[5] = point2->v;

  status = zdimpolyline(unit, imp, mask, color, 6, x, y);

  return status;
}
/************************************************************************/
/* FrameOval will frame an oval in the given plane.
 * Returns SUCCESS or FAIL.
 */
int FrameOval(unit, imp, mask, color, bounds)
  int			unit,imp;	/* dev unit no. and image plane	*/
  int			mask;		/* which bits to frame		*/
  int			color;		/* color with which to frame	*/
  Rect			*bounds;	/* bounding rectangle		*/
{
  int *xArr,*yArr,*xArr2,*yArr2,i,left,top,bottom,nPoints;
  int x,y,oldx;
  float a,b,b2;
  double sqrt();

  left = bounds->left; top = bounds->top; bottom = bounds->bottom;
  if ((left == bounds->right) || (top == bottom)) /* if no width, don't	*/
    return SUCCESS;				  /* draw it at all	*/
  nPoints = ((bottom - top) / 2) + 1;
  xArr = malloc( (nPoints + 1) * 2 * sizeof(int));
  yArr = malloc( (nPoints + 1) * 2 * sizeof(int));
  xArr2 = malloc( (nPoints + 1) * 2 * sizeof(int));
  yArr2 = malloc( (nPoints + 1) * 2 * sizeof(int));

  a = (float)(bounds->right - left) / 2;
  b = (float)(bounds->bottom - top) / 2;
  b2 = b * b;
  i = 0;
  for (y = 0; y < nPoints; y++)
  {
    oldx = x;
    x = (a * sqrt(1 - (y*y/b2)));

    /* When there is more than a one pixel gap between the endpoints of	*/
    /* successive lines, and try to connect them with a vector, the	*/
    /* 'jaggy' where the vector jumps up one pixel to the next line	*/
    /* can appear in a random place, and is dependant on the device.	*/
    /* Therefore, we make such a vector into a horizontal vector for	*/
    /* the horizontal span, followed by a 45 degree one pixel long	*/
    /* diagonal vector to the next scan line.  For example:		*/
    /*      ----X                          X				*/
    /* X----           becomes    X-------X				*/

    if (oldx - x > 1 && i != 0)		/* Add a horizontal vector */
    {
      yArr[i] = yArr[i-1];
      yArr2[i] = yArr2[i-1];
      xArr[i] = left + (int) a - (x+1);
      xArr2[i] = left + (int) (a + 0.5) + (x+1);
      i++;
    }
    yArr[i] = top + (int) (b + 0.5) + y;
    xArr[i] = left + (int) a - x;
    yArr2[i] = top + (int) b - y;
    xArr2[i] = left + (int) (a + 0.5) + x;
    i++;
  }

  yArr[i] = bottom; yArr2[i] = top;
  xArr[i] = xArr2[i] = left + (int) (a + 0.5);
  i++;
  zdimpolyline(unit, imp, mask, color, i, xArr, yArr);
  zdimpolyline(unit, imp, mask, color, i, xArr, yArr2);
  zdimpolyline(unit, imp, mask, color, i, xArr2, yArr);
  zdimpolyline(unit, imp, mask, color, i, xArr2, yArr2);

  free(xArr); free(yArr); free(yArr2); free(xArr2);
  return SUCCESS;
}
/************************************************************************/
/* MarkPoint will mark the indicated point on the given imp in the
 * desired color with an X.
 */
int MarkPoint(unit, imp, mask, color, pt)
  int			unit,imp;	/* dev unit no. and image plane	*/
  int			mask;		/* bits to modify		*/
  int			color;		/* color with which to frame	*/
  Point			*pt;		/* the point to mark		*/
{
  int status, nLines, nPix, size, x[2], y[2];

  nPix = zdsns(unit);
  nLines = zdsnl(unit);
  size = (zdsvnl(unit) > 512) ? 2 : 1;	/* if > 512 lines, make it bigger */

  x[0] = MAX(1, pt->h - size);
  x[1] = MIN(nPix, pt->h + size);
  y[0] = MAX(1, pt->v - size);
  y[1] = MIN(nLines, pt->v + size);
  status = zdimpolyline(unit, imp, mask, color, 2, x, y);
  if (status != SUCCESS) return status;

  x[0] = MIN(nPix, pt->h + size);
  x[1] = MAX(1, pt->h - size);
  y[0] = MAX(1, pt->v - size);
  y[1] = MIN(nLines, pt->v + size);
  status = zdimpolyline(unit, imp, mask, color, 2, x, y);
  return status;
}

/************************************************************************/
/* FillRegion will fill a region with the given value, applying the
 * given mask.  Returns the VRDI status.
 */
int FillRegion(env, imp, rgn, mask, color)
  VIDSEnvironment	*env;	/* the VIDS environment			*/
  int		imp;		/* image memory plane to fill		*/
  Region	*rgn;		/* ptr to region to fill		*/
  int		mask;		/* bit mask for values			*/
  int		color;		/* color/DN to fill region with		*/
{
  int status;		/* vrdi error status	*/
  int unit;		/* device unit number	*/
  int y, x1, x2;	/* coordinates of line	*/
  RegionState rgnstate;	/* Saves state info for NextRegionLine() */
  Boolean NextRegionLine();
  char *msg="Sorry, unable to erase that area";
  
  unit = env->devUnit;
  zddbatch(unit, True);
  switch (rgn->type)
  {
    case Rectangle :
    case Square :
      status = zdiawset(unit, imp, rgn->bounds.left,
				 rgn->bounds.top,
				 rgn->bounds.right,
				 rgn->bounds.bottom);
      if (status != SUCCESS) ABORT(status, msg, "VIDS-VRDIERR");
      status = zdimfill(unit, imp, mask, color);
      zdiawset(unit, imp, 1, 1, env->nsMax, env->nlMax);
      if (status != SUCCESS) ABORT(status, msg, "VIDS-VRDIERR");
      break;

    case Circle:
    case Oval:
    case Polygon:
      StartRegionLine(rgn, &rgnstate);
      while (NextRegionLine(rgn, &rgnstate, &y, &x1, &x2))
      {
        status = zdiawset(unit, imp, x1, y, x2, y);
        if (status != SUCCESS) ABORT(status, msg, "VIDS-VRDIERR");
        status = zdimfill(unit, imp, mask, color);
        if (status != SUCCESS) break;
      }
      zdiawset(unit, imp, 1, 1, env->nsMax, env->nlMax);
      if (status != SUCCESS) ABORT(status, msg, "VIDS-VRDIERR");
      break;

    default:
      ABORT(FAIL,"Unrecognized region type; consult VIDS programmer","VIDS-NOTIMPL");
  }

  status = FrameRgn(unit, imp, mask, color, rgn);    /* make sure frame is gone */
  zddbatch(unit, False);
  if (status != SUCCESS) ABORT(status, msg, "VIDS-VRDIERR");

  return SUCCESS;
}
/************************************************************************/
/* DrawLine will draw a line connecting the two given points on the 
 * display device in the given color on the current imp.
 */
int DrawLine(unit, imp, mask, color, start, end)
  int			unit,imp;	/* dev unit no. and image plane	*/
  int			mask;		/* which bits to draw		*/
  int			color;		/* color with which to draw	*/
  Point			*start,*end;	/* start, end points of line	*/
{
  int x[2],y[2];		/* start/end x and y points		*/
  int status;

  x[0] = start->h;	y[0] = start->v;
  x[1] = end->h;	y[1] = end->v;
  status = zdimpolyline(unit, imp,
                      mask, color, 2, x, y);
  return status;
}
/************************************************************************/
/* DrawText will draw text in the given colors on the given plane, in an
 * 'outline' format.  The string is drawn in the background color just above,
 * below, and to each side of the given position, then the foreground is
 * drawn in the middle.  The zdt routines should already have the mask,
 * rotation, size, etc. set up.  Returns the VRDI status.
 */
int DrawText(unit, imp, x, y, mode, len, text, fore, back)
  int			unit,imp;	/* dev unit no. and image plane	*/
  int			x, y;		/* Position for foreground char	*/
  int			mode;		/* Justify mode for zdttext	*/
  int			len;		/* Number of chars		*/
  char			*text;		/* The text to draw		*/
  int			fore, back;	/* Foreground and background colors */
{
  int status;
  int xx, yy;

  status = zdtcolor(back, 0);
  if (status != SUCCESS) return status;

  xx = x;
  yy = y;

  zddbatch(unit, True);
  xx++;
  zdttext(unit, imp, xx, yy, mode, len, text);	/* right side */
  xx -= 2;
  zdttext(unit, imp, xx, yy, mode, len, text);	/* left side */
  xx++;
  yy++;
  zdttext(unit, imp, xx, yy, mode, len, text);	/* bottom */
  yy -= 2;
  zdttext(unit, imp, xx, yy, mode, len, text);	/* top */

  status = zdtcolor(fore, 0);
  if (status != SUCCESS) return status;

  zdttext(unit, imp, x, y, mode, len, text);	/* Foreground */
  zddbatch(unit, False);

  return SUCCESS;
}
