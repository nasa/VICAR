/************************************************************************/
/* Utilities for handling regions.  The functionality here closely      */
/* mimics the X Region routines, but the X routines use short ints      */
/* for coordinates, and we need full ints.  These routines are intended */
/* for internal use of the Xvic routines only.                          */
/************************************************************************/

#include "XvicRegion.h"
#include <stdlib.h>
#include <string.h>

#ifndef MAX
#define MAX(a,b) (((a)>(b))?(a):(b))
#endif

#ifndef MIN
#define MIN(a,b) (((a)<(b))?(a):(b))
#endif

#ifndef TRUE
#define TRUE 1
#endif

#ifndef FALSE
#define FALSE 0
#endif

#ifndef NULL
#ifdef __cplusplus	/* C++ likes NULL defined as 0 */
#define NULL 0
#else
#define NULL ((void*)0)
#endif
#endif

/************************************************************************/
/* Internal routine to add rectangles to a region			*/
/************************************************************************/

static int
#ifdef _NO_PROTO
more_rects(rgn, num)
   _XvicRegion *rgn;
   int num;
#else
more_rects(
   _XvicRegion *rgn,
   int num)
#endif
{
   _XvicRect *tmp;

   if (rgn->num_rects + num > rgn->rects_alloc) {
      rgn->rects_alloc += MAX(num, 5);
      tmp = (_XvicRect *)malloc(sizeof(_XvicRect) * rgn->rects_alloc);
      if (tmp == NULL) {
         rgn->rects_alloc -= MAX(num, 5);
         return FALSE;
      }
      if (rgn->num_rects > 0)
         memcpy(tmp, rgn->rects, sizeof(_XvicRect) * rgn->num_rects);
      if (rgn->rects)
         free(rgn->rects);
      rgn->rects = tmp;
   }
   return TRUE;
}

/************************************************************************/
/* Intersects two rectangles, returning the result in a third.		*/
/* Returns TRUE if they intersect, FALSE if not.			*/
/************************************************************************/

int
#ifdef _NO_PROTO
_XvicRectIntersectRect(r1, r2, dest)
   _XvicRect *r1;
   _XvicRect *r2;
   _XvicRect *dest;
#else
_XvicRectIntersectRect(
   _XvicRect *r1,
   _XvicRect *r2,
   _XvicRect *dest)
#endif
{
   dest->x1 = MAX(r1->x1, r2->x1);
   dest->x2 = MIN(r1->x2, r2->x2);
   dest->y1 = MAX(r1->y1, r2->y1);
   dest->y2 = MIN(r1->y2, r2->y2);

   if (dest->x1 > dest->x2 || dest->y1 > dest->y2)
      return FALSE;			/* no intersection */
   return TRUE;
}

/************************************************************************/
/* Returns the bounding rectangle for a region				*/
/************************************************************************/

int
#ifdef _NO_PROTO
_XvicRegionBounds(rgn, rect)
   _XvicRegion *rgn;
   _XvicRect *rect;
#else
_XvicRegionBounds(
   _XvicRegion *rgn,
   _XvicRect *rect)
#endif
{
   int i;

   if (rgn->num_rects == 0)
      return FALSE;

   rect->x1 = rgn->rects[0].x1;
   rect->x2 = rgn->rects[0].x2;
   rect->y1 = rgn->rects[0].y1;
   rect->y2 = rgn->rects[0].y2;

   for (i=1; i<rgn->num_rects; i++) {
      if (rgn->rects[i].x1 < rect->x1)
         rect->x1 = rgn->rects[i].x1;
      if (rgn->rects[i].x2 > rect->x2)
         rect->x2 = rgn->rects[i].x2;
      if (rgn->rects[i].y1 < rect->y1)
         rect->y1 = rgn->rects[i].y1;
      if (rgn->rects[i].y2 > rect->y2)
         rect->y2 = rgn->rects[i].y2;
   }
   return TRUE;
}

/************************************************************************/
/* Create an empty region						*/
/************************************************************************/

_XvicRegion *
#ifdef _NO_PROTO
_XvicRegionCreate()
#else
_XvicRegionCreate()
#endif
{
   _XvicRegion *rgn;

   rgn = (_XvicRegion *)malloc(sizeof(_XvicRegion));
   if (rgn == NULL)
      return NULL;

   rgn->num_rects = 0;
   rgn->rects_alloc = 0;
   rgn->rects = NULL;

   return rgn;
}

/************************************************************************/
/* Intersect a rectangle with a region, returning a new region.		*/
/* We could just create a new region, copy into it, and call		*/
/* RegionIntersect, but that is terribly inefficient for the use this	*/
/* routine is put to (culling out everything except the tile).		*/
/************************************************************************/

_XvicRegion *
#ifdef _NO_PROTO
_XvicRegionCreateIntersect(rect, rgn)
   _XvicRect *rect;
   _XvicRegion *rgn;
#else
_XvicRegionCreateIntersect(
   _XvicRect *rect,
   _XvicRegion *rgn)
#endif
{
   _XvicRegion *new_rgn;
   _XvicRect temp_rect;
   int i;

   new_rgn = _XvicRegionCreate();
   if (new_rgn == NULL)
      return NULL;

   for (i=0; i<rgn->num_rects; i++) {

      /* First clip each box to the rectangle */

      memcpy(&temp_rect, &rgn->rects[i], sizeof(_XvicRect));

      if (temp_rect.x1 < rect->x1)
         temp_rect.x1 = rect->x1;
      if (temp_rect.y1 < rect->y1)
         temp_rect.y1 = rect->y1;
      if (temp_rect.x2 > rect->x2)
         temp_rect.x2 = rect->x2;
      if (temp_rect.y2 > rect->y2)
         temp_rect.y2 = rect->y2;

      /* If the coordinates are not in the wrong order, the box has not	*/
      /* been fully clipped, so copy it into the destination.		*/

      if ((temp_rect.x1 <= temp_rect.x2) &&
          (temp_rect.y1 <= temp_rect.y2)) {

         if (!more_rects(new_rgn, 1)) {
            _XvicRegionDestroy(new_rgn);
            return NULL;
         }
         memcpy(&new_rgn->rects[new_rgn->num_rects],&temp_rect,sizeof(_XvicRect));
         new_rgn->num_rects++;
      }
   }
   return new_rgn;
}

/************************************************************************/
/* Create a region from a single rectangle				*/
/************************************************************************/

_XvicRegion *
#ifdef _NO_PROTO
_XvicRegionCreateRect(rect)
   _XvicRect *rect;
#else
_XvicRegionCreateRect(
   _XvicRect *rect)
#endif
{
   _XvicRegion *tmp;

   tmp = _XvicRegionCreate();
   if (tmp == NULL)
      return NULL;

   if (!more_rects(tmp, 1)) {
      _XvicRegionDestroy(tmp);
      return NULL;
   }

   memcpy(&tmp->rects[0], rect, sizeof(_XvicRect));
   tmp->num_rects = 1;

   return tmp;
}

/************************************************************************/
/* Destroys a region							*/
/************************************************************************/

void
#ifdef _NO_PROTO
_XvicRegionDestroy(rgn)
   _XvicRegion *rgn;
#else
_XvicRegionDestroy(
   _XvicRegion *rgn)
#endif
{
   free(rgn->rects);
   free(rgn);
}

/************************************************************************/
/* Returns the number of rectangles in a region				*/
/************************************************************************/

int 
#ifdef _NO_PROTO
_XvicRegionGetNumRects(rgn)
   _XvicRegion *rgn;
#else
_XvicRegionGetNumRects(
   _XvicRegion *rgn)
#endif
{
   return rgn->num_rects;
}

/************************************************************************/
/* Returns the list of rectangles in a region				*/
/************************************************************************/

_XvicRect * 
#ifdef _NO_PROTO
_XvicRegionGetRectangles(rgn)
   _XvicRegion *rgn;
#else
_XvicRegionGetRectangles(
   _XvicRegion *rgn)
#endif
{
   return rgn->rects;
}

/************************************************************************/
/* Returns TRUE if any part of the region is inside the given rectangle.*/
/************************************************************************/

int 
#ifdef _NO_PROTO
_XvicRegionHasRect(rect, rgn)
   _XvicRect *rect;
   _XvicRegion *rgn;
#else
_XvicRegionHasRect(
   _XvicRect *rect,
   _XvicRegion *rgn)
#endif
{
   int i;

   for (i=0; i<rgn->num_rects; i++) {

      if (rgn->rects[i].x2 >= rect->x1 && rgn->rects[i].x1 <= rect->x2 &&
          rgn->rects[i].y2 >= rect->y1 && rgn->rects[i].y1 <= rect->y2)
         return TRUE;
   }
   return FALSE;
}

/************************************************************************/
/* Intersect a rectangle with a region, discarding any part of the	*/
/* region outside of the rectangle					*/
/************************************************************************/

int
#ifdef _NO_PROTO
_XvicRegionIntersect(rect, rgn)
   _XvicRect *rect;
   _XvicRegion *rgn;
#else
_XvicRegionIntersect(
   _XvicRect *rect,
   _XvicRegion *rgn)
#endif
{
   int i, j;

   for (i=0; i<rgn->num_rects; i++) {

      /* First clip each box to the rectangle */

      if (rgn->rects[i].x1 < rect->x1)
         rgn->rects[i].x1 = rect->x1;
      if (rgn->rects[i].y1 < rect->y1)
         rgn->rects[i].y1 = rect->y1;
      if (rgn->rects[i].x2 > rect->x2)
         rgn->rects[i].x2 = rect->x2;
      if (rgn->rects[i].y2 > rect->y2)
         rgn->rects[i].y2 = rect->y2;

      /* If the coordinates are in the wrong order, the box has been  */
      /* fully clipped, so discard it by moving everything else down. */

      if ((rgn->rects[i].x1 > rgn->rects[i].x2) ||
          (rgn->rects[i].y1 > rgn->rects[i].y2)) {

         for (j = i+1; j < rgn->num_rects; j++)
            memcpy(&rgn->rects[j-1], &rgn->rects[j], sizeof(_XvicRect));
         rgn->num_rects--;
         i--;			/* compensate for i++ to re-do this index */
      }
   }
   return TRUE;
}

/************************************************************************/
/* Returns TRUE if region is empty, FALSE if it is not			*/
/************************************************************************/

int
#ifdef _NO_PROTO
_XvicRegionIsEmpty(rgn)
   _XvicRegion *rgn;
#else
_XvicRegionIsEmpty(
   _XvicRegion *rgn)
#endif
{
   if (rgn->num_rects == 0)
      return TRUE;
   else
      return FALSE;
}

/************************************************************************/
/* Adds an offset to all coordinates in a region			*/
/************************************************************************/

int
#ifdef _NO_PROTO
_XvicRegionOffset(rgn, xoff, yoff)
   _XvicRegion *rgn;
   int xoff;
   int yoff;
#else
_XvicRegionOffset(
   _XvicRegion *rgn,
   int xoff,
   int yoff)
#endif
{
   int i;

   for (i=0; i<rgn->num_rects; i++) {
      rgn->rects[i].x1 += xoff;
      rgn->rects[i].x2 += xoff;
      rgn->rects[i].y1 += yoff;
      rgn->rects[i].y2 += yoff;
   }
   return TRUE;
}

/************************************************************************/
/* Subtract a rectangle from a region, leaving only parts of the region	*/
/* that are outside the rectangle.  The general idea here is that Y	*/
/* clipping is done first, then X.  Any box that straddles the edge of	*/
/* the rectangle is split into two.  The first is completely clipped;	*/
/* the second is left until its turn in the loop to be clipped.		*/
/* The routine returns TRUE if the operation was successful, and FALSE	*/
/* if it ran out of memory.  This routine is different than others in 	*/
/* this package because a FALSE return leaves the job partially		*/
/* complete (instead of undoing it).  However, the same call may be	*/
/* performed again (once some memory has been freed); in other words,	*/
/* this operation is restartable.					*/
/************************************************************************/

int
#ifdef _NO_PROTO
_XvicRegionSubtract(rect, rgn)
   _XvicRect *rect;
   _XvicRegion *rgn;
#else
_XvicRegionSubtract(
   _XvicRect *rect,
   _XvicRegion *rgn)
#endif
{
   int i, j;

   for (i=0; i<rgn->num_rects; i++) {

      /* First subtract the Y direction */

      if ((rgn->rects[i].y1 < rect->y1 && rgn->rects[i].y2 >= rect->y1) &&
          (rgn->rects[i].x2 >= rect->x1 && rgn->rects[i].x1 <= rect->x2)) {

         /* Straddles top edge and there is an X intersect, so clip old	*/
         /* box to top part and create new box for the rest		*/

         if (!more_rects(rgn, 1))
            return FALSE;
         rgn->rects[rgn->num_rects].x1 = rgn->rects[i].x1;
         rgn->rects[rgn->num_rects].x2 = rgn->rects[i].x2;
         rgn->rects[rgn->num_rects].y1 = rect->y1;
         rgn->rects[rgn->num_rects].y2 = rgn->rects[i].y2;
         rgn->rects[i].y2 = rect->y1 - 1;
         rgn->num_rects++;
      }

      if ((rgn->rects[i].y1 <= rect->y2 && rgn->rects[i].y2 > rect->y2) &&
          (rgn->rects[i].x2 >= rect->x1 && rgn->rects[i].x1 <= rect->x2)) {

         /* Straddled bottom edge and there is an X intersect, so clip	*/
         /* old box to top part and create new box for the bottom	*/

         if (!more_rects(rgn, 1))
            return FALSE;
         rgn->rects[rgn->num_rects].x1 = rgn->rects[i].x1;
         rgn->rects[rgn->num_rects].x2 = rgn->rects[i].x2;
         rgn->rects[rgn->num_rects].y1 = rect->y2 + 1;
         rgn->rects[rgn->num_rects].y2 = rgn->rects[i].y2;
         rgn->rects[i].y2 = rect->y2;
         rgn->num_rects++;
      }

      /* Now subtract the X direction.  We know that Y is already clipped */

      if ((rgn->rects[i].x1 < rect->x1 && rgn->rects[i].x2 >= rect->x1) &&
          (rgn->rects[i].y1 >= rect->y1 && rgn->rects[i].y2 <= rect->y2)) {

         /* Straddles left edge and there is a Y intersect, so clip	*/
         /* old box to left part and create new box for the rest	*/

         if (!more_rects(rgn, 1))
            return FALSE;
         rgn->rects[rgn->num_rects].y1 = rgn->rects[i].y1;
         rgn->rects[rgn->num_rects].y2 = rgn->rects[i].y2;
         rgn->rects[rgn->num_rects].x1 = rect->x1;
         rgn->rects[rgn->num_rects].x2 = rgn->rects[i].x2;
         rgn->rects[i].x2 = rect->x1 - 1;
         rgn->num_rects++;
      }

      if ((rgn->rects[i].x1 <= rect->x2 && rgn->rects[i].x2 > rect->x2) &&
          (rgn->rects[i].y1 >= rect->y1 && rgn->rects[i].y2 <= rect->y2)) {

         /* Straddled right edge and there is a Y intersect, so clip	*/
         /* old box to left part and create new box for the right	*/

         if (!more_rects(rgn, 1))
            return FALSE;
         rgn->rects[rgn->num_rects].y1 = rgn->rects[i].y1;
         rgn->rects[rgn->num_rects].y2 = rgn->rects[i].y2;
         rgn->rects[rgn->num_rects].x1 = rect->x2 + 1;
         rgn->rects[rgn->num_rects].x2 = rgn->rects[i].x2;
         rgn->rects[i].x2 = rect->x2;
         rgn->num_rects++;
      }

      /* Now that the box is completely clipped, see if it is	*/
      /* inside the rectangle.  If so, delete it.		*/

      if (rgn->rects[i].x1 >= rect->x1 && rgn->rects[i].x2 <= rect->x2 &&
          rgn->rects[i].y1 >= rect->y1 && rgn->rects[i].y2 <= rect->y2) {

         for (j = i+1; j < rgn->num_rects; j++)
            memcpy(&rgn->rects[j-1], &rgn->rects[j], sizeof(_XvicRect));
         rgn->num_rects--;
         i--;		/* compensate for i++ to re-do this index */
      }
   }

   return TRUE;
}

/************************************************************************/
/* Add (logical union) a rectangle into a region			*/
/* **** TO DO ****!!!!							*/
/* Possible optimization not yet added: merge rects with matching y2	*/
/* but different y1's, and put the excess y into a new rect.  This	*/
/* would help with the ExposeInWork pan case, since a few tiles at the	*/
/* top have already been removed from the damage list.			*/
/* This routine returns FALSE on memory error, and leaves the region	*/
/* in a partially-completed state.  Like _XvicRegionSubtract, however,	*/
/* the operation is restartable (after some memory is freed).		*/
/************************************************************************/

int
#ifdef _NO_PROTO
_XvicRegionUnion(rect, rgn)
   _XvicRect *rect;
   _XvicRegion *rgn;
#else
_XvicRegionUnion(
   _XvicRect *rect,
   _XvicRegion *rgn)
#endif
{
   int i;

   /* Check for an easy merge: if y's match and x's overlap for any	*/
   /* rect, just merge the rects and we're done.  This is a common case	*/
   /* due to the way the tiles work.					*/

   for (i=0; i<rgn->num_rects; i++) {		/* Check for an easy merge */
      if (rgn->rects[i].y1 == rect->y1 && rgn->rects[i].y2 == rect->y2) {
	 /* Y height matches, check for L side */
         if (rect->x2+1 >= rgn->rects[i].x1 && rect->x1 <= rgn->rects[i].x1) {
            rgn->rects[i].x1 = rect->x1;	/* Left side merge */
            rgn->rects[i].x2 = MAX(rgn->rects[i].x2, rect->x2);
            return TRUE;
         }
         /* Now check for R side */
         if (rect->x1-1 <= rgn->rects[i].x2 && rect->x2 >= rgn->rects[i].x2) {
            rgn->rects[i].x2 = rect->x2;	/* Right side merge */
            rgn->rects[i].x1 = MIN(rgn->rects[i].x1, rect->x1);
            return TRUE;
         }
      }
   }

   /* Not easy, do it the hard way by subtracting the rect from the	*/
   /* region (to prevent overlaps), then adding the new rect.		*/

   if (!_XvicRegionSubtract(rect, rgn))
      return FALSE;

   if (!more_rects(rgn, 1))
      return FALSE;

   memcpy(&rgn->rects[rgn->num_rects], rect, sizeof(_XvicRect));
   rgn->num_rects++;

   return TRUE;
}

