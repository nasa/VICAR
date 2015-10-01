/* RgnUtil.c contains the utilities used for operations on regions.
 */
#include "VIDSdefs.h"

/************************************************************************/
/* StartRegionLine initializes a region state structure for use by
 * NextRegionLine().  Returns an error status if it runs out of memory.
 */

int StartRegionLine(rgn, rgnstate)
  Region	*rgn;			/* the region to use		*/
  RegionState	*rgnstate;		/* State info for the region	*/
{
  Rect	*bounds;
  int status;

  switch (rgn->type)
  {
    case Square:
    case Rectangle:
      rgnstate->r.y  = rgn->bounds.top;
      status = SUCCESS;
      break;
    case Circle:
    case Oval:
      bounds = &rgn->bounds;
      if ((bounds->left == bounds->right) || (bounds->top == bounds->bottom))
        rgnstate->o.nPoints = 0;		/* if no width, do nothing */
      else
      {
        rgnstate->o.nPoints = ((bounds->bottom - bounds->top) / 2) + 1;

        rgnstate->o.a = (float)(bounds->right - bounds->left) / 2;
        rgnstate->o.b = (float)(bounds->bottom - bounds->top) / 2;
	rgnstate->o.b2 = rgnstate->o.b * rgnstate->o.b;
        rgnstate->o.index = rgnstate->o.nPoints - 1;
        rgnstate->o.pass = 0;
      }
      status = SUCCESS;
      break;
    case Polygon:
      status = StartPolyLine(rgn->nPoints, rgn->pointList, rgnstate);
      break;
    default:
      break;
  }

  return status;
}

/************************************************************************/
/* StartPolyLine initializes a region state structure for a polygon
 * for use by NextRegionLine().  Returns an error status if it runs out
 * of memory.
 */

int StartPolyLine(npoints, pointlist, rgnstate)
  int		npoints;		/* Number of points in polygon	*/
  Point		pointlist[];		/* The vertices			*/
  RegionState	*rgnstate;		/* State info for the region	*/
{
  PolyEdge *ET, *edge, *next;
  int i, j;
  int y, ynext, yprev;
  int xmin, xmax;
  double x, xnext, xprev;
  double deltax;
  Boolean horiz;

  /* First generate the Edge Table (ET).  As we go, remove all		*/
  /* horizontal edges, as they are not needed.  Also, for any vertices	*/
  /* that will produce double intersections when they shouldn't,	*/
  /* shorten one of the edges.						*/

  ET = NULL;

  for (i=0; i<npoints; i++)
  {
    x = pointlist[i].h;				/* Current point */
    y = pointlist[i].v;

    xnext = pointlist[(i+1) % npoints].h;	/* Next point around polygon */
    ynext = pointlist[(i+1) % npoints].v;	/* (circular buffer)	     */

    xprev = pointlist[(i-1+npoints) % npoints].h;	/* Previous point */
    yprev = pointlist[(i-1+npoints) % npoints].v;	/* around polygon */
    j = 1;
    while (yprev == y && j < npoints)		/* Skip past horiz. lines */
    {
      j++;
      xprev = pointlist[(i-j+npoints) % npoints].h;
      yprev = pointlist[(i-j+npoints) % npoints].v;
    }

    if (x <= xnext)
    {
      xmin = (int)(x + 0.5);
      xmax = (int)(xnext + 0.5);
    }
    else
    {
      xmin = (int)(xnext + 0.5);
      xmax = (int)(x + 0.5);
    }

    if (y == ynext)			/* Horizontal lines are special */
    {
      horiz = True;
      if (x > xnext)			/* X-coord must be on left side */
      {
        deltax = x;
        x = xnext;
      }
      else
        deltax = xnext;			/* deltax holds ending coord of line */
    }
    else
    {
      horiz = False;

      deltax = (xnext - x) / (float)(ynext - y);

      /* If this vertex is not a local max or min (meaning both edges	*/
      /* go either up or down), then the edge must be shortened by one	*/
      /* scan line so we don't get double intersections.		*/

      if ((y > yprev) && (y < ynext))		/* edge going up */
      {
        y += 1;
        x += deltax;
      }
      if ((y < yprev) && (y > ynext))		/* edge going down */
      {
        y -= 1;
        x -= deltax;
      }
    }

    /* The point is now ready to go.  Link it in to the list. */

    edge = malloc(sizeof(PolyEdge));
    if (edge == NULL)
    {
      for (edge = ET; edge != NULL; edge = next)
      {					/* Out of mem, so clean up and exit */
        next = edge->next;
        free(edge);
      }
      rgnstate->p.ET = NULL;
      rgnstate->p.AET = NULL;
      return FAIL;
    }

    if (y <= ynext)
    {
      edge->ymin = y;
      edge->ymax = ynext;
      edge->x = x;
    }
    else
    {
      edge->ymin = ynext;
      edge->ymax = y;
      edge->x = xnext;
    }

    edge->xmin = xmin;
    edge->xmax = xmax;
    edge->deltax = deltax;
    edge->horiz = horiz;
    edge->next = ET;				/* link edge into list */
    ET = edge;
  }						/* end of for loop */

  SortPolyEdgeTable(&ET);

  /* Now, finally, fill in the state table structure */

  rgnstate->p.ET = ET;
  rgnstate->p.AET = NULL;
  rgnstate->p.nextpair = NULL;
  rgnstate->p.y = ET->ymin;		/* lowest y coordinate */
  rgnstate->p.maxx = 0;

  return SUCCESS;
}

/************************************************************************/
/* NextRegionLine returns the position of the next line segment in the
 * region.  StartRegionLine() must be called first to initialize.
 * Returns True if the returned point is good; False if there are no
 * more points (in which case the returned point is invalid).
 * Segments are returned in a strict top-to-bottom order, left-to-right
 * inside a line (if there's more than one segment on a line).
 */

Boolean NextRegionLine(rgn, rgnstate, y, x1, x2)
  Region	*rgn;			/* the region to use		*/
  RegionState	*rgnstate;		/* State info for the region	*/
  int		*y;			/* Y coordinate of line		*/
  int		*x1;			/* Starting X coordinate	*/
  int		*x2;			/* Ending X coordinate of line	*/
{
  Boolean status, NextOvalLine(), NextPolyLine();

  switch (rgn->type)
  {
    case Square:
    case Rectangle:
      if (rgnstate->r.y <= rgn->bounds.bottom)
      {
        *x1 = rgn->bounds.left;
        *x2 = rgn->bounds.right;
        *y = rgnstate->r.y;
        rgnstate->r.y++;
        status = True;
      }
      else
        status = False;
      break;
    case Circle:
    case Oval:
      status = NextOvalLine(&rgn->bounds, rgnstate, y, x1, x2);
      break;
    case Polygon:
      status = NextPolyLine(rgnstate, y, x1, x2);
      break;
    default:
      status = False;
  }

  return status;
}

/************************************************************************/
/* NextOvalLine returns the position of the next line segment in an
 * oval.  StartRegionLine() must be called first to initialize.
 * Returns True if the returned point is good; False if there are no
 * more points (in which case the returned point is invalid).
 */

Boolean NextOvalLine(bounds, rgnstate, y, x1, x2)
  Rect		*bounds;		/* Bounding rectangle of region	*/
  RegionState	*rgnstate;		/* State info for the region	*/
  int		*y;			/* Y coordinate of line		*/
  int		*x1;			/* Starting X coordinate	*/
  int		*x2;			/* Ending X coordinate of line	*/
{
  int i, x, y1, y2;
  double sqrt();

  if (rgnstate->o.nPoints == 0)
    return False;			/* no points, so just return */

  i = rgnstate->o.index;

  if (rgnstate->o.pass == 0)		/* First pass, check for turnaround */
  {
    if (i < 0)				/* go to second pass */
    {
      i = 0;
      y1 = bounds->top + (int) rgnstate->o.b        - i;
      y2 = bounds->top + (int)(rgnstate->o.b + 0.5) + i;
      if (y1 == y2)			/* don't duplicate lines */
        rgnstate->o.index = 1;
      else
        rgnstate->o.index = 0;
      rgnstate->o.pass = 1;
      i = rgnstate->o.index;
    }
  }
  if (rgnstate->o.pass == 1)		/* second pass, check for end */
    if (i >= rgnstate->o.nPoints)
      return False;				/* end of region */

  if (rgnstate->o.pass == 0)
    *y = bounds->top + (int) rgnstate->o.b        - i;
  else
    *y = bounds->top + (int)(rgnstate->o.b + 0.5) + i;

  x = (rgnstate->o.a * sqrt(1 - (i*i/rgnstate->o.b2)));

  *x1 = bounds->left + (int) rgnstate->o.a        - x;
  *x2 = bounds->left + (int)(rgnstate->o.a + 0.5) + x;

  if (rgnstate->o.pass == 0)
    rgnstate->o.index--;
  else
    rgnstate->o.index++;

  return True;
}

/************************************************************************/
/* NextPolyLine returns the position of the next line segment in a
 * polygon.  StartRegionLine() must be called first to initialize.
 * Returns True if the returned point is good; False if there are no
 * more points (in which case the returned point is invalid).
 */

Boolean NextPolyLine(rgnstate, y, x1, x2)
  RegionState	*rgnstate;		/* State info for the region	*/
  int		*y;			/* Y coordinate of line		*/
  int		*x1;			/* Starting X coordinate	*/
  int		*x2;			/* Ending X coordinate of line	*/
{
  Boolean resort;
  PolyEdge *edge, *nextedge, *prevedge;
  PolyEdge *np;				/* Mirrors rgnstate->p.nextpair	*/
  Boolean repeat;

  repeat = True;
  while (repeat)
  {
    repeat = False;	/* if we get an empty line, repeat to get another */

    if (rgnstate->p.ET == NULL && rgnstate->p.AET == NULL)
      return False;			/* End of polygon */

    *y = rgnstate->p.y;

    /* If there are no edge pairs left from previous line, start a new one */

    np = rgnstate->p.nextpair;

    if (np == NULL)
    {
      resort = False;
      while (rgnstate->p.ET != NULL && rgnstate->p.ET->ymin == *y)
      {				/* Move vertices that start on this line */
        edge = rgnstate->p.ET;
        rgnstate->p.ET = edge->next;	/* remove from ET */
        edge->next = rgnstate->p.AET;	/* put it on AET */
        rgnstate->p.AET = edge;
        resort = True;
      }
      if (resort)
        SortActiveEdgeTable(&rgnstate->p.AET);	/* Re-sort AET */

      np = rgnstate->p.AET;
      rgnstate->p.maxx = 0;
    }

    /* Get the next pair of edges from the AET (Active Edge Table) */

    if (np != NULL)				/* should always be true */
    {
      *x1 = np->x + 0.5; 	    /* Start coord. from first edge */
      *x2 = *x1;
      IncludeEdge(np, x1, x2);	/* include this edge in the current segment */
      while (np != NULL && np->horiz == True)	/* Starts with horiz. line, */
      {						/* include them in this seg */
        IncludeEdge(np, x1, x2);
        np = np->next;
      }
      if (np != NULL)
        np = np->next;			/* done with first point */

      while (np != NULL && np->horiz == True)	/* Skip over horiz. lines   */
      {						/* but include them in this */
        IncludeEdge(np, x1, x2);		/* segment.		    */
        np = np->next;
      }

      if (np == NULL)			/* Should never happen! */
      {
        NotifyUser(Inform, "VIDS-INTERR",
          "Internal error in NextPolyLine.  Please notify the system programmers.");
      }
      else
      {
        IncludeEdge(np, x1, x2);
        np = np->next;
        while (np != NULL && np->horiz == True && (int)(np->x+0.5) <= *x2)
        {					/* Include horiz. lines that */
          IncludeEdge(np, x1, x2);		/* are adjacent to this seg. */
          np = np->next;
        }
      }
      if (*x1 <= rgnstate->p.maxx)	/* Overlap, so adjust this line */
        *x1 = rgnstate->p.maxx + 1;	/* so we don't duplicate pixels	*/
      if (*x1 > *x2)
        repeat = True;			/* Empty line, so get another one */
      rgnstate->p.maxx = *x2;
    }

    /* If there are no more pairs left in the AET, then clean up this line */
    /* to get ready for the next one.					   */

    if (np == NULL)
    {
      prevedge = NULL;
      edge = rgnstate->p.AET;		/* Remove edges we're done with */
      while (edge != NULL)
      {
        nextedge = edge->next;
        if (edge->ymax == *y)
        {
          if (prevedge == NULL)		/* Remove from the linked list */
            rgnstate->p.AET = edge->next;
          else
            prevedge->next = edge->next;
          free(edge);			/* Deallocate the memory */
        }
        else
          prevedge = edge;
        edge = nextedge;
      }

      for (edge = rgnstate->p.AET; edge != NULL; edge = edge->next)
        edge->x += edge->deltax;	/* Bump the x-coord for each edge */

      SortActiveEdgeTable(&rgnstate->p.AET);

      rgnstate->p.y += 1;
    }

    rgnstate->p.nextpair = np;
  }					/* end of repeat loop */

  return True;
}

/************************************************************************/
/* IncludeEdge takes an edge of a polygon and attaches it to the current
 * segment.  This routine takes care of trying to match the edges of the
 * fill area with what polyline draws.  For almost horizontal lines, there
 * can be many pixels on the scan line from this edge; these must all be
 * included in this scan line segment.
 */

IncludeEdge(edge, x1, x2)
  PolyEdge *edge;		/* The edge in question */
  int *x1;			/* Starting point of the current segment */
  int *x2;			/* Ending point of the current segment */
{
  int x;
  double d;

  if (edge->horiz)
  {
    *x1 = MIN(*x1, (int)(edge->x + 0.5));
    *x2 = MAX(*x2, (int)(edge->deltax + 0.5));
  }
  else
  {
    if (edge->deltax >= 0)
      d = (edge->deltax / 2.0);
    else
      d = - (edge->deltax / 2.0);
    x = MAX(edge->xmin, (int)(edge->x - d + 0.5));
    if (x < (int)(edge->x + 0.5))
      x++;
    *x1 = MIN(*x1, x);
    x = MIN(edge->xmax, (int)(edge->x + d + 0.5));
    if (x > (int)(edge->x + 0.5))
      x--;
    *x2 = MAX(*x2, x);
  }
  return;
}

/************************************************************************/
/* SortPolyEdgeTable sorts a polygon edge table in order of increasing
 * ymin.  If two edges have the same ymin, they are sorted in order of
 * increasing x.  Very similar to SortActiveEdgeTable().  Maintain both
 * routines in parallel!
 */

SortPolyEdgeTable(table)
  PolyEdge **table;		/* Address of pointer to start of table */
{
  PolyEdge *newtbl;
  PolyEdge *edge, *next;
  PolyEdge *e, *p;

  newtbl = NULL;

  for (edge = *table; edge != NULL; edge = next)
  {
    next = edge->next;

    p = NULL;
    for (e = newtbl; e != NULL; e = e->next)
    {
      if (e->ymin > edge->ymin)
        break;				/* found the place */
      if (e->ymin == edge->ymin && e->x > edge->x)
        break;				/* found it for x */
      p = e;
    }

    /* Insert the new node before e */

    if (p == NULL)
      newtbl = edge;
    else
      p->next = edge;
    edge->next = e;
  }

  *table = newtbl;

  return;
}

/************************************************************************/
/* SortActiveEdgeTable sorts a polygon edge table in order of increasing
 * x.  It is similar to SortPolyEdgeTable() except that y is not used for
 * the sort.  Maintain both routines in parallel!
 */

SortActiveEdgeTable(table)
  PolyEdge **table;		/* Address of pointer to start of table */
{
  PolyEdge *newtbl;
  PolyEdge *edge, *next;
  PolyEdge *e, *p;
  newtbl = NULL;

  for (edge = *table; edge != NULL; edge = next)
  {
    next = edge->next;

    p = NULL;
    for (e = newtbl; e != NULL; e = e->next)
    {
      if (e->x > edge->x)		/* found the place */
        break;
      p = e;
    }

    /* Insert the new node before e */

    if (p == NULL)
      newtbl = edge;
    else
      p->next = edge;
    edge->next = e;
  }

  *table = newtbl;

  return;
}
