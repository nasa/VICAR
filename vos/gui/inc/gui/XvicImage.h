#ifndef _XvicImage_h
#define _XvicImage_h

#include <X11/Intrinsic.h>
#include "XvicBasicImage.h"

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************/
/* version								*/
/* 1.0	Initial release.  No version present				*/
/* 1.1  Added HW overlay enable/disable, fix various overlay problems	*/
/* 1.2	Added fractional-pixel graphics and cursor locations		*/
/* 1.3  Added XvicDOUBLE_ARG macro in order to support 64-bit machines	*/
/*	(def'd in BasicImage but changes cursor(XY)fp calling sequence)	*/
/************************************************************************/

#define XvicImageMAJOR_VERSION		1
#define XvicImageMINOR_VERSION		3

/************************************************************************/
/* type defines								*/
/************************************************************************/

typedef struct _XvicImageClassRec	*XvicImageWidgetClass;
typedef struct _XvicImageRec		*XvicImageWidget;

/************************************************************************/
/* extern class								*/
/************************************************************************/

externalref WidgetClass xvicImageWidgetClass;

/************************************************************************/
/* fast subclass define							*/
/************************************************************************/

#ifndef XvicIsImage
#define XvicIsImage(w) 		XtIsSubclass(w, xvicImageWidgetClass)
#endif /* XvicIsImage */

/************************************************************************/
/* class strings							*/
/************************************************************************/

#define XvicCCursor		"Cursor"
#define XvicCCursorBackground	"CursorBackground"
#define XvicCCursorForeground	"CursorForeground"
#define XvicCCursorLoc		"CursorLoc"
#define XvicCCursorLocFp	"CursorLocFp"
#define XvicCCursorMode		"CursorMode"
#define XvicCEnableHWOverlay	"EnableHWOverlay"
#define XvicCScrollBarDisplayPolicy "ScrollBarDisplayPolicy"
#define XvicCTrackFloatingCursor "TrackFloatingCursor"

/************************************************************************/
/* representation type strings						*/
/************************************************************************/

/* Note that XvicRSBDP is different than XmRSBDP because Xvic allows	*/
/* XvicNEVER as a value.  Class & instance names can be the same.	*/

#define XvicRCursorMode		"CursorMode"
#define XvicRScrollBarDisplayPolicy "XvicScrollBarDisplayPolicy"

/************************************************************************/
/* instance strings							*/
/************************************************************************/

#define XvicNcursor		"cursor"
#define XvicNcursorBackground	"cursorBackground"
#define XvicNcursorCallback	"cursorCallback"
#define XvicNcursorForeground	"cursorForeground"
#define XvicNcursorMode		"cursorMode"
#define XvicNcursorX		"cursorX"
#define XvicNcursorXfp		"cursorXfp"
#define XvicNcursorY		"cursorY"
#define XvicNcursorYfp		"cursorYfp"
#define XvicNenableHWOverlay	"enableHWOverlay"
#define XvicNinputCallback	"inputCallback"
#define XvicNpanCallback	"panCallback"
#define XvicNscrollBarDisplayPolicy "scrollBarDisplayPolicy"
#define XvicNtrackFloatingCursor "trackFloatingCursor"

/************************************************************************/
/* enumerated constants							*/
/************************************************************************/

/* Callback Reasons (continued from XvicBasicImage.h) */
#define XvicCR_PAN		94
#define XvicCR_INPUT		95
#define XvicCR_CURSOR		96

/* ScrollBarDisplayPolicy */
#define XvicSTATIC		210
#define XvicAS_NEEDED		211
#define XvicNEVER		212

/* CursorMode */
#define XvicFLOATING		220
#define XvicPLANTED		221

/* Justify settings for text drawing routines */
#define XvicJUST_LEFT		230
#define XvicJUST_CENTER		231
#define XvicJUST_RIGHT		232

/************************************************************************/
/* public structures							*/
/************************************************************************/

/* Callback structure is defined by XvicBasicImage. */

typedef int XvicColor;
typedef int XvicGC;
typedef int XvicID;

typedef struct _XvicArc
{
	double	x;
	double	y;
	double	width;
	double	height;
	int	angle1;
	int	angle2;
} XvicArc;

typedef struct _XvicPoint
{
	double	x;
	double	y;
} XvicPoint;

typedef struct _XvicRectangle
{
	double	x;
	double	y;
	double	width;
	double	height;
} XvicRectangle;

typedef struct _XvicSegment
{
	double	x1;
	double	y1;
	double	x2;
	double	y2;
} XvicSegment;

/************************************************************************/
/* public functions							*/
/************************************************************************/

#ifdef _NO_PROTO

extern Widget XvicCreateImage();
extern void XvicImageChangeGC();
extern XvicGC XvicImageCreateGC();
extern XvicGC XvicImageCreateRubberGC();
extern XvicID XvicImageDrawArc();
extern XvicID XvicImageDrawArcs();
extern XvicID XvicImageDrawBitmap();
extern XvicID XvicImageDrawImageString();
extern XvicID XvicImageDrawImageString16();
extern XvicID XvicImageDrawLine();
extern XvicID XvicImageDrawLines();
extern XvicID XvicImageDrawPoint();
extern XvicID XvicImageDrawPoints();
extern XvicID XvicImageDrawRectangle();
extern XvicID XvicImageDrawRectangles();
extern XvicID XvicImageDrawSegments();
extern XvicID XvicImageDrawString();
extern XvicID XvicImageDrawString16();
extern XvicID XvicImageDrawText();
extern XvicID XvicImageDrawText16();
extern void XvicImageEraseObject();
extern void XvicImageEraseOverlay();
extern XvicID XvicImageFillArc();
extern XvicID XvicImageFillArcs();
extern XvicID XvicImageFillPolygon();
extern XvicID XvicImageFillRectangle();
extern XvicID XvicImageFillRectangles();
extern void XvicImageFreeGC();
extern XvicColor XvicImageGetGrColor();
extern XvicColor XvicImageGetGrColorRGB();
extern void XvicImageMoveObject();
extern void XvicImageSetDashes();
extern void XvicImageSetFontCursor();
extern void XvicImageSetGlyphCursor();
extern void XvicImageSetPixmapCursor();

#else

extern Widget XvicCreateImage(
		Widget		parent,
		char		*name,
		ArgList		args,
		Cardinal	argCount);

extern void XvicImageChangeGC(
		Widget		w,
		XvicGC		gc,
		unsigned long	valuemask,
		XGCValues	*values);

extern XvicGC XvicImageCreateGC(
		Widget		w,
		unsigned long	valuemask,
		XGCValues	*values);

extern XvicGC XvicImageCreateRubberGC(
		Widget		w,
		unsigned long	valuemask,
		XGCValues	*values);

extern XvicID XvicImageDrawArc(
		Widget		w,
		XvicID		id,
		XvicGC		gc,
		XvicColor	color,
		double		x,
		double		y,
		double		width,
		double		height,
		int		angle1,
		int		angle2);

extern XvicID XvicImageDrawArcs(
		Widget		w,
		XvicID		id,
		XvicGC		gc,
		XvicColor	color,
		XvicArc		*arcs,
		int		narcs);

extern XvicID XvicImageDrawBitmap(
		Widget		w,
		XvicID		id,
		XvicGC		gc,
		XvicColor	color,
		double		x,
		double		y,
		Pixmap		bitmap,
		unsigned int	width,
		unsigned int	height,
		int		hot_x,
		int		hot_y);

extern XvicID XvicImageDrawImageString(
		Widget		w,
		XvicID		id,
		XvicGC		gc,
		XvicColor	fg,
		XvicColor	bg,
		double		x,
		double		y,
		char		*string,
		int		length,
		int		justify);

extern XvicID XvicImageDrawImageString16(
		Widget		w,
		XvicID		id,
		XvicGC		gc,
		XvicColor	fg,
		XvicColor	bg,
		double		x,
		double		y,
		XChar2b		*string,
		int		length,
		int		justify);

extern XvicID XvicImageDrawLine(
		Widget		w,
		XvicID		id,
		XvicGC		gc,
		XvicColor	color,
		double		x1,
		double		y1,
		double		x2,
		double		y2);

extern XvicID XvicImageDrawLines(
		Widget		w,
		XvicID		id,
		XvicGC		gc,
		XvicColor	color,
		XvicPoint	*points,
		int		npoints,
		int		mode);

extern XvicID XvicImageDrawPoint(
		Widget		w,
		XvicID		id,
		XvicGC		gc,
		XvicColor	color,
		double		x,
		double		y);

extern XvicID XvicImageDrawPoints(
		Widget		w,
		XvicID		id,
		XvicGC		gc,
		XvicColor	color,
		XvicPoint	*points,
		int		npoints,
		int		mode);

extern XvicID XvicImageDrawRectangle(
		Widget		w,
		XvicID		id,
		XvicGC		gc,
		XvicColor	color,
		double		x,
		double		y,
		double		width,
		double		height);

extern XvicID XvicImageDrawRectangles(
		Widget		w,
		XvicID		id,
		XvicGC		gc,
		XvicColor	color,
		XvicRectangle	*rectangles,
		int		nrectangles);

extern XvicID XvicImageDrawSegments(
		Widget		w,
		XvicID		id,
		XvicGC		gc,
		XvicColor	color,
		XvicSegment	*segments,
		int		nsegments);

extern XvicID XvicImageDrawString(
		Widget		w,
		XvicID		id,
		XvicGC		gc,
		XvicColor	color,
		double		x,
		double		y,
		char		*string,
		int		length,
		int		justify);

extern XvicID XvicImageDrawString16(
		Widget		w,
		XvicID		id,
		XvicGC		gc,
		XvicColor	color,
		double		x,
		double		y,
		XChar2b		*string,
		int		length,
		int		justify);

extern XvicID XvicImageDrawText(
		Widget		w,
		XvicID		id,
		XvicGC		gc,
		XvicColor	color,
		double		x,
		double		y,
		XTextItem	*items,
		int		nitems,
		int		justify);

extern XvicID XvicImageDrawText16(
		Widget		w,
		XvicID		id,
		XvicGC		gc,
		XvicColor	color,
		double		x,
		double		y,
		XTextItem16	*items,
		int		nitems,
		int		justify);

extern void XvicImageEraseObject(
		Widget		w,
		XvicID		id);

extern void XvicImageEraseOverlay(
		Widget		w);

extern XvicID XvicImageFillArc(
		Widget		w,
		XvicID		id,
		XvicGC		gc,
		XvicColor	color,
		double		x,
		double		y,
		double		width,
		double		height,
		int		angle1,
		int		angle2);

extern XvicID XvicImageFillArcs(
		Widget		w,
		XvicID		id,
		XvicGC		gc,
		XvicColor	color,
		XvicArc		*arcs,
		int		narcs);

extern XvicID XvicImageFillPolygon(
		Widget		w,
		XvicID		id,
		XvicGC		gc,
		XvicColor	color,
		XvicPoint	*points,
		int		npoints,
		int		shape,
		int		mode);

extern XvicID XvicImageFillRectangle(
		Widget		w,
		XvicID		id,
		XvicGC		gc,
		XvicColor	color,
		double		x,
		double		y,
		double		width,
		double		height);

extern XvicID XvicImageFillRectangles(
		Widget		w,
		XvicID		id,
		XvicGC		gc,
		XvicColor	color,
		XvicRectangle	*rectangles,
		int		nrectangles);

extern void XvicImageFreeGC(
		Widget		w,
		XvicGC		gc);

extern XvicColor XvicImageGetGrColor(
		Widget		w,
		XColor		*xcolor);

extern XvicColor XvicImageGetGrColorRGB(
		Widget		w,
		int		red,
		int		green,
		int		blue);

extern void XvicImageMoveObject(
		Widget		w,
		XvicID		id,
		double		delta_x,
		double		delta_y);

extern void XvicImageSetDashes(
		Widget		w,
		XvicGC		gc,
		int		dash_offset,
		char		dash_list[],
		int		n);

extern void XvicImageSetFontCursor(
		Widget		w,
		unsigned int	shape);

extern void XvicImageSetGlyphCursor(
		Widget		w,
		Font		source_font,
		Font		mask_font,
		unsigned int	source_char,
		unsigned int	mask_char);

extern void XvicImageSetPixmapCursor(
		Widget		w,
		Pixmap		source,
		Pixmap		mask,
		unsigned int	x,
		unsigned int	y);

#endif /* _NO_PROTO */

/************************************************************************/

#ifdef __cplusplus
}  /* Close scope of 'extern "C"' declaration which encloses file. */
#endif

#endif /* _XvicImage_h */
/* DON'T ADD STUFF AFTER THIS #endif */

