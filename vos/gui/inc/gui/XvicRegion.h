#ifndef _Xvicregion_h
#define _Xvicregion_h

#ifndef _NO_PROTO
#if !defined(__STDC__) && !defined(__cplusplus)
#define _NO_PROTO
#endif
#endif

/************************************************************************/
/* Utilities for handling regions.  The functionality here closely	*/
/* mimics the X Region routines, but the X routines use short ints	*/
/* for coordinates, and we need full ints.  These routines are intended	*/
/* for internal use of the Xvic routines only.				*/
/************************************************************************/

typedef struct __XvicRect {		/* Application-visible structure */
   int x1, y1;
   int x2, y2;
} _XvicRect;

typedef struct __XvicRegion {		/* Opaque structure */
   int num_rects;
   int rects_alloc;
   _XvicRect *rects;
} _XvicRegion;

#ifdef _NO_PROTO

int _XvicRectIntersectRect();
int _XvicRegionBounds();
_XvicRegion *_XvicRegionCreate();
_XvicRegion *_XvicRegionCreateIntersect();
_XvicRegion *_XvicRegionCreateRect();
void _XvicRegionDestroy();
int _XvicRegionGetNumRects();
_XvicRect *_XvicRegionGetRectangles();
int _XvicRegionHasRect();
int _XvicRegionIntersect();
int _XvicRegionIsEmpty();
int _XvicRegionOffset();
int _XvicRegionSubtract();
int _XvicRegionUnion();

#else

int _XvicRectIntersectRect(
		_XvicRect *r1,
		_XvicRect *r2,
		_XvicRect *dest);
int _XvicRegionBounds(
		_XvicRegion *rgn,
		_XvicRect *rect);
_XvicRegion *_XvicRegionCreate();
_XvicRegion *_XvicRegionCreateIntersect(
		_XvicRect *rect,
		_XvicRegion *rgn);
_XvicRegion *_XvicRegionCreateRect(
		_XvicRect *rect);
void _XvicRegionDestroy(
		_XvicRegion *rgn);
int _XvicRegionGetNumRects(
		_XvicRegion *rgn);
_XvicRect *_XvicRegionGetRectangles(
		_XvicRegion *rgn);
int _XvicRegionHasRect(
		_XvicRect *rect,
		_XvicRegion *rgn);
int _XvicRegionIntersect(
		_XvicRect *rect,
		_XvicRegion *rgn);
int _XvicRegionIsEmpty(
		_XvicRegion *rgn);
int _XvicRegionOffset(
		_XvicRegion *rgn,
		int xoff,
		int yoff);
int _XvicRegionSubtract(
		_XvicRect *rect,
		_XvicRegion *rgn);
int _XvicRegionUnion(
		_XvicRect *rect,
		_XvicRegion *rgn);

#endif /* _NO_PROTO */

#endif

