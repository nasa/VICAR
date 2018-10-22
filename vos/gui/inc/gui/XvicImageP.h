#ifndef _XvicImageP_h
#define _XvicImageP_h

/* Motif is required for XvicImage, unlike XvicBasicImage */

#include "XvicImage.h"
#include <Xm/Xm.h>

#if XmVERSION==1 && XmREVISION<=1	/* Some things are different in 1.2...*/
#define MOTIF_1_1
#endif

#include <Xm/XmP.h>
#include "XvicBasicImageP.h"
#include <Xm/ScrollBar.h>
#include <Xm/ScrolledW.h>
#ifndef MOTIF_1_1
#include <Xm/PrimitiveP.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************/
/* Class structure							*/
/************************************************************************/

typedef struct _XvicImageClassPart
{
   XtPointer	extension;	/* Pointer to extension record */
} XvicImageClassPart;

typedef struct _XvicImageClassRec
{
   CoreClassPart		core_class;
   XmPrimitiveClassPart		primitive_class;
   XvicBasicImageClassPart	basic_image_class;
   XvicImageClassPart		image_class;
} XvicImageClassRec;

externalref XvicImageClassRec xvicImageClassRec;

/************************************************************************/
/* Structures needed by Instance					*/
/************************************************************************/

typedef struct _GrColor
{
   XColor xcolor;	/* desired values and pixel to use if not dither */
   Pixmap gc_tile;	/* Pixmap to use for dither tiling or NULL */
   int width, height;	/* w/h of Pixmap (if present) */
   Boolean active;	/* True if this color is in active use (not freed) */
   Boolean alloc_def;	/* True if this color is alloced from sys cmap */
   Boolean alloc_pvt;	/* True if alloced from private cmap (may be both) */
			/* (private cmap may be biw or overlay) */
} GrColor;

typedef struct _GrGC
{
   XGCValues values;	/* Values to set in the real GC */
   unsigned long mask;	/* (overridden by anything needed in SetUpGraphicsGC) */
   int dash_offset;	/* Dashes must be set separately... */
   char *dash_list;
   int num_dashes;
   Boolean is_rubber;	/* True if this is for rubber-banding */
   Boolean active;	/* True if this color is in active use (not freed) */
   GC gc;		/* The real X GC */
} GrGC;

/************************************************************************/
/* Object structures.  GrAny contains the common elements, and each	*/
/* graphics element type adds its own parameters.  This is modeled	*/
/* somewhat after the XEvent structure.  Gee, C++ sure would be handy	*/
/* here...								*/
/************************************************************************/

typedef enum {
	GrFillArc, GrFillArcs, GrFillPolygon, GrFillRectangle, GrFillRectangles,
	GrArc, GrArcs, GrBitmap, GrLine, GrLines, GrPoint, GrPoints,
	GrRectangle, GrRectangles, GrSegments,
	GrImageString, GrImageString16, GrString, GrString16, GrText, GrText16
} GrType;

typedef struct _GrAnyObject {
   GrType type;
   XvicID id;
   XvicColor color;
   XvicGC gc;
   struct _XvicRectFp {		/* like _XvicRect but floating-point */
      double x1, y1;
      double x2, y2;
   } bounds;			/* in FP Img coordinates */
} GrAnyObject;

typedef struct _GrArcObject {
   GrAnyObject any;
   double x, y;
   double width, height;
   int angle1, angle2;
} GrArcObject;

typedef struct _GrArcsObject {
   GrAnyObject any;
   XvicArc *arcs;
   int narcs;
} GrArcsObject;

typedef struct _GrBitmapObject {
   GrAnyObject any;
   double x, y;
   Pixmap bitmap;
   unsigned int width, height;
   int hot_x, hot_y;
} GrBitmapObject;

typedef struct _GrLineObject {
   GrAnyObject any;
   double x1, y1, x2, y2;
} GrLineObject;

typedef struct _GrLinesObject {
   GrAnyObject any;
   XvicPoint *points;
   int npoints;
   int mode;
   int shape;			/* for FillPolygon only */
} GrLinesObject;

typedef struct _GrPointObject {
   GrAnyObject any;
   double x, y;
} GrPointObject;

typedef struct _GrRectangleObject {
   GrAnyObject any;
   double x, y;
   double width, height;
} GrRectangleObject;

typedef struct _GrRectanglesObject {
   GrAnyObject any;
   XvicRectangle *rectangles;
   int nrectangles;
} GrRectanglesObject;

typedef struct _GrSegmentsObject {
   GrAnyObject any;
   XvicSegment *segments;
   int nsegments;
} GrSegmentsObject;

typedef struct _GrAnyStringObject {
   GrAnyObject any;
   int justify;
   int ascent;		/* Calculated by CalcStringBounds() */
   int descent;
   int width;
   int rbearing;
   int lbearing;
} GrAnyStringObject;

typedef struct _GrStringObject {
   GrAnyStringObject anystr;
   XvicColor fg;		/* bg is any.color to make drawing easier */
   double x, y;
   char *string;
   int length;
} GrStringObject;

typedef struct _GrString16Object {
   GrAnyStringObject anystr;
   XvicColor fg;		/* bg is any.color to make drawing easier */
   double x, y;
   XChar2b *string;
   int length;
} GrString16Object;

typedef struct _GrTextObject {
   GrAnyStringObject anystr;
   double x, y;
   XTextItem *items;
   int nitems;
} GrTextObject;

typedef struct _GrText16Object {
   GrAnyStringObject anystr;
   double x, y;
   XTextItem16 *items;
   int nitems;
} GrText16Object;

/* Note that the objects pointed at by gr_object array in the instance	*/
/* record are allocated to the *individual* size (sizeof(GrLineObject),	*/
/* etc.) not the size of the union (sizeof(GrObject)).			*/

typedef union _GrObject
{
   GrType			type;
   GrAnyObject			any;
   GrArcObject			arc;		/* Also for FillArc */
   GrArcsObject			arcs;		/* Also for FillArcs */
   GrBitmapObject		bitmap;
   GrLineObject			line;
   GrLinesObject		lines;	/* Also for FillPolygon and Points */
   GrPointObject		point;
   GrRectangleObject		rectangle;	/* Also for FillRectangle */
   GrRectanglesObject		rectangles;	/* Also for FillRectangles */
   GrSegmentsObject		segments;
   GrAnyStringObject		anystr;		/* For all the following... */
   GrStringObject		string;		/* Also for ImageString */
   GrString16Object		string16;	/* Also for ImageString16 */
   GrTextObject			text;
   GrText16Object		text16;
} GrObject;

/************************************************************************/
/* OverlayType and OverlayInfo describe the SERVER_OVERLAY_VISUALS	*/
/* property of servers that have hardware overlays.  This really should	*/
/* be in an X include somewhere, if overlays were officially supported.	*/
/* The VITec documentation (the only docs I could find) defines only	*/
/* three fields per overlay visual.  However, the HP and SGI define	*/
/* four fields.  The R6 Fresco *source* (Xdisplay.cxx) claims that the	*/
/* fourth field is the overlay "layer", numbered from top-to-bottom.	*/
/* It also calls the "value" field "transparent" and uses long instead	*/
/* of unsigned long (like the ViTEC docs show).  Ahh, standardization!!	*/
/************************************************************************/

typedef enum { NotTransparent = 0, TransparentPixel = 1, TransparentMask = 2 }
	OverlayType;

typedef struct _OverlayInfo
{
   unsigned long vid;
   OverlayType type;
   unsigned long value;
   unsigned long layer;
} OverlayInfo;

/************************************************************************/
/* Instance structure							*/
/************************************************************************/

typedef struct _XvicImagePart
{
   XtCallbackList	cursor_callback;
   XtCallbackList	input_callback;
   XtCallbackList	pan_callback;
   XtCallbackList	unrealize_callback;

   unsigned char	scrollbar_display_policy;
   unsigned char	cursor_mode;
   int			cursor_x;
   int			cursor_y;
   double		cursor_x_fp;
   double		cursor_y_fp;

   String		cursor;
   String		cursor_foreground;
   String		cursor_background;

   Boolean		track_floating_cursor;

   Boolean		enable_hw_overlay;

   /* Private members - not directly settable via resources */

   XmScrolledWindowWidget scroll_win;		/* ScrolledWindow parent */
   XmScrollBarWidget	h_scrollbar;		/* Horizontal scrollbar */
   XmScrollBarWidget	v_scrollbar;		/* Vertical scrollbar */

   Boolean		hsb_managed;		/* True iff currently managed */
   Boolean		vsb_managed;

   int			x_mouse_scr_pan;	/* Position of last mouse pan */
   int			y_mouse_scr_pan;	/* in Scr coordinates */

   Boolean		in_scrollbar;	/* True iff SetValues from SB callback*/

   GrColor		*gr_colors;	/* array of graphics colors */
   int			num_gr_colors;

   GrGC			*gr_gc_list;	/* array of user GC structs */
   int			num_gr_gc;

   GrObject		**gr_objects;	/* array of pointers to GrObjects */
   int			num_gr_objects;
   XvicID		max_gr_id;	/* max ID for assigning new ones */

   Pixmap		curs_source;
   Pixmap		curs_mask;
   unsigned int		curs_hot_x;
   unsigned int		curs_hot_y;
   unsigned int		curs_width;
   unsigned int		curs_height;
   Cursor		x_cursor;	/* X ID of cursor */

   GrColor		curs_fg;	/* For drawing the planted cursor */
   GrColor		curs_bg;
   GC			curs_fg_gc;
   GC			curs_bg_gc;
   Boolean		plant_set;	/* True if plant cur is ready to draw */
   Boolean		erasing_cursor;	/* True if plant cur is being erased */
   double		plant_curs_x;	/* Img coordinates of planted cursor */
   double		plant_curs_y;

   Visual		*overlay_visual;
   int			overlay_depth;
   OverlayType		overlay_type;
   unsigned long	overlay_value;
   Widget		overlay_widget;
   Colormap		overlay_colormap;
   int			overlay_cmap_size;
   GC			overlay_pan_gc;

   int			default_cmap_refcnt[CMAP_SIZE_MAX];
			/* below is for either biw->cmap or overlay->cmap */
   int			private_cmap_refcnt[CMAP_SIZE_MAX];

} XvicImagePart;

typedef struct _XvicImageRec
{
   CorePart		core;
   XmPrimitivePart	primitive;
   XvicBasicImagePart	bim;		/* short name cuz it's used a bunch */
   XvicImagePart	image;
} XvicImageRec;

/************************************************************************/
/* Private functions needed by other modules				*/
/************************************************************************/

#ifdef _NO_PROTO

extern void _XvicImageSetFontCursor();
extern void _XvicImageSetGlyphCursor();
extern void _XvicImageSetPixmapCursor();
extern void _XvicImageSetStringCursor();

#else

extern void _XvicImageSetFontCursor(
		XvicImageWidget	iw,
		unsigned int	shape);

extern void _XvicImageSetGlyphCursor(
		XvicImageWidget	iw,
		Font		source_font,
		Font		mask_font,
		unsigned int	source_char,
		unsigned int	mask_char);

extern void _XvicImageSetPixmapCursor(
		XvicImageWidget	iw,
		Pixmap		source,
		Pixmap		mask,
		unsigned int	x,
		unsigned int	y);

extern void _XvicImageSetStringCursor(
		XvicImageWidget iw,
		char *name);

#endif /* _NO_PROTO */

/************************************************************************/
/* Macros needed by implementation					*/
/************************************************************************/

#define CURSORFONT_NAME		"cursor"
#define DEFAULT_CURSOR		"crosshair"
#define DEFAULT_CURSOR_SHAPE	XC_crosshair

/* This value is used to indicate that the cursor resource values are	*/
/* not set.  It is possible this could be a valid value, but highly	*/
/* unlikely.  If the user is 32K pixels off the edge of the image,	*/
/* it's his own problem!  The STR form is used as a string default	*/
/* in the resource list for doubles.					*/
#define CURSOR_NO_LOC		-32767
#define CURSOR_NO_LOC_STR	"-32767"

/************************************************************************/

#ifdef __cplusplus
}  /* Close scope of 'extern "C"' declaration which encloses file. */
#endif

#endif /* _XvicImageP_h */
/* DON'T ADD STUFF AFTER THIS #endif */

